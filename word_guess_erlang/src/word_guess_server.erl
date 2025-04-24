%%%-------------------------------------------------------------------
%%% @doc word_guess server - central manager for game state and communications
%%%-------------------------------------------------------------------
-module(word_guess_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_game/3,
    join_game/2,
    get_game/1,
    get_public_games/0,
    make_guess/3,
    register_user/2,
    login_user/2,
    register_gameplay_pid/2,
    register_games_list_pid/1,
    unregister_gameplay_pid/2,
    unregister_games_list_pid/1,
    start_game/2,
    leave_game/2,
    player_disconnected/2,
    player_reconnected/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("word_guess_records.hrl").

-record(state, {
    games_list_pids = [] :: list(),      % WebSocket PIDs for game list updates
    gameplay_pids = #{} :: map(),        % Map of GameID -> [WebSocket PIDs] for gameplay updates
    player_timers = #{} :: map(),        % Map of {GameID, Player} -> TRef for turn timers
    disconnect_timers = #{} :: map()     % Map of {GameID, Player} -> TRef for disconnect timers
}).

%%===================================================================
%% API
%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_game(Creator, MaxPlayers, IsPublic) ->
    gen_server:call(?MODULE, {create_game, Creator, MaxPlayers, IsPublic}).

join_game(GameId, Player) ->
    gen_server:call(?MODULE, {join_game, GameId, Player}).

get_game(GameId) ->
    gen_server:call(?MODULE, {get_game, GameId}).

get_public_games() ->
    gen_server:call(?MODULE, get_public_games).

make_guess(GameId, Player, Guess) ->
    gen_server:call(?MODULE, {make_guess, GameId, Player, Guess}).

register_user(Username, Password) ->
    gen_server:call(?MODULE, {register_user, Username, Password}).

login_user(Username, Password) ->
    gen_server:call(?MODULE, {login_user, Username, Password}).

register_gameplay_pid(GameId, Pid) ->
    gen_server:cast(?MODULE, {register_gameplay_pid, GameId, Pid}).

register_games_list_pid(Pid) ->
    gen_server:cast(?MODULE, {register_games_list_pid, Pid}).

unregister_gameplay_pid(GameId, Pid) ->
    gen_server:cast(?MODULE, {unregister_gameplay_pid, GameId, Pid}).

unregister_games_list_pid(Pid) ->
    gen_server:cast(?MODULE, {unregister_games_list_pid, Pid}).

start_game(GameId, Creator) ->
    gen_server:call(?MODULE, {start_game, GameId, Creator}).

leave_game(GameId, Player) ->
    gen_server:call(?MODULE, {leave_game, GameId, Player}).

player_disconnected(GameId, Player) ->
    gen_server:cast(?MODULE, {player_disconnected, GameId, Player}).

player_reconnected(GameId, Player) ->
    gen_server:cast(?MODULE, {player_reconnected, GameId, Player}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({create_game, Creator, MaxPlayers, IsPublic}, _From, State) ->
    % Generate a random word from a predefined list
    Word = word_guess_game:get_random_word(),
    
    % Create a new game
    Game = #game{
        id = generate_game_id(),
        word = Word,
        maskedWord = string:copies("_", length(Word)),
        maxPlayers = MaxPlayers,
        isPublic = IsPublic,
        creator = Creator,
        players = [Creator],
        currentPlayerIndex = 0,
        guessedLetters = [],
        guessedWords = [],
        status = waiting,
        createdAt = erlang:system_time(seconds)
    },
    
    % Save the game
    case word_guess_mnesia:save_game(Game) of
        ok ->
            % Notify all game list WebSockets if the game is public
            case IsPublic of
                true -> 
                    notify_game_list_update(State#state.games_list_pids);
                false -> 
                    ok
            end,
            {reply, {ok, Game}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({join_game, GameId, Player}, _From, State) ->
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            % Check if player can join
            case can_join_game(Game, Player) of
                true ->
                    % Add player to game
                    UpdatedPlayers = Game#game.players ++ [Player],
                    UpdatedGame = Game#game{players = UpdatedPlayers},
                    
                    % Update game status if needed
                    FinalGame = case length(UpdatedPlayers) of
                        N when N >= Game#game.maxPlayers ->
                            UpdatedGame#game{status = in_progress};
                        _ ->
                            UpdatedGame
                    end,
                    
                    % Save updated game
                    case word_guess_mnesia:save_game(FinalGame) of
                        ok -> 
                            % Notify gameplay WebSockets
                            notify_gameplay_update(GameId, FinalGame, State#state.gameplay_pids),
                            
                            % Notify game list WebSockets if game is public
                            case FinalGame#game.isPublic of
                                true -> 
                                    notify_game_list_update(State#state.games_list_pids);
                                false -> 
                                    ok
                            end,
                            
                            {reply, {ok, FinalGame}, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {false, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_game, GameId}, _From, State) ->
    Reply = word_guess_mnesia:get_game(GameId),
    {reply, Reply, State};

handle_call(get_public_games, _From, State) ->
    Reply = word_guess_mnesia:get_public_games(),
    {reply, Reply, State};

handle_call({register_user, Username, Password}, _From, State) ->
    Reply = word_guess_mnesia:register_user(Username, Password),
    {reply, Reply, State};

handle_call({login_user, Username, Password}, _From, State) ->
    Reply = word_guess_mnesia:login_user(Username, Password),
    {reply, Reply, State};

handle_call({start_game, GameId, Creator}, _From, State) ->
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            {Status, UpdatedGame} = word_guess_game:start_game(Game, Creator),
            
            case Status of
                ok ->
                    case word_guess_mnesia:save_game(UpdatedGame) of
                        ok -> 
                            % Start turn timer for the first player
                            NewState = start_turn_timer(GameId, UpdatedGame, State),
                            
                            % Notify gameplay WebSockets
                            notify_gameplay_update(GameId, UpdatedGame, NewState#state.gameplay_pids),
                            
                            % Notify game list WebSockets if game is public
                            case UpdatedGame#game.isPublic of
                                true -> 
                                    notify_game_list_update(NewState#state.games_list_pids);
                                false -> 
                                    ok
                            end,
                            
                            {reply, {ok, UpdatedGame}, NewState};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                error ->
                    {reply, {error, UpdatedGame#game.error}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Leave game handler
handle_call({leave_game, GameId, Player}, _From, State) ->
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            {Status, UpdatedGame} = word_guess_game:player_left(Game, Player),
            
            % Cancel any timers for this player
            State1 = cancel_player_timers(GameId, Player, State),
            
            case word_guess_mnesia:save_game(UpdatedGame) of
                ok -> 
                    % Start new turn timer if needed
                    NewState = case UpdatedGame#game.status of
                        in_progress ->
                            start_turn_timer(GameId, UpdatedGame, State1);
                        _ ->
                            State1
                    end,
                    
                    % Notify gameplay WebSockets
                    notify_gameplay_update(GameId, UpdatedGame, NewState#state.gameplay_pids),
                    
                    % Notify game list WebSockets if game status changed
                    case Game#game.status =/= UpdatedGame#game.status andalso UpdatedGame#game.isPublic of
                        true -> 
                            notify_game_list_update(NewState#state.games_list_pids);
                        false -> 
                            ok
                    end,
                    
                    {reply, {Status, UpdatedGame}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State1}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({make_guess, GameId, Player, Guess}, _From, State) ->
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            % Check if it's the player's turn
            CurrentPlayerIndex = Game#game.currentPlayerIndex,
            Players = Game#game.players,
            CurrentPlayer = lists:nth(CurrentPlayerIndex + 1, Players),
            
            case CurrentPlayer =:= Player of
                true ->
                    % Process the guess
                    {Status, UpdatedGame} = word_guess_game:process_guess(Game, Guess),
                    
                    case Status of
                        error ->
                            % For error cases, don't cancel the timer
                            {reply, {Status, UpdatedGame}, State};
                        _ ->
                            % For valid guesses, cancel current turn timer
                            State1 = cancel_turn_timer(GameId, Player, State),
                            
                            % Save updated game
                            case word_guess_mnesia:save_game(UpdatedGame) of
                                ok -> 
                                    % Start new turn timer if the game is still in progress
                                    NewState = case UpdatedGame#game.status of
                                        in_progress ->
                                            start_turn_timer(GameId, UpdatedGame, State1);
                                        _ ->
                                            State1
                                    end,
                                    
                                    % Notify gameplay WebSockets
                                    notify_gameplay_update(GameId, UpdatedGame, NewState#state.gameplay_pids),
                                    
                                    % Notify game list WebSockets if game status changed to completed
                                    case UpdatedGame#game.status =:= completed andalso UpdatedGame#game.isPublic of
                                        true -> 
                                            notify_game_list_update(NewState#state.games_list_pids);
                                        false -> 
                                            ok
                                    end,
                                    
                                    {reply, {Status, UpdatedGame}, NewState};
                                {error, Reason} ->
                                    {reply, {error, Reason}, State1}
                            end
                    end;
                false ->
                    {reply, {error, not_your_turn}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register_gameplay_pid, GameId, Pid}, State) ->
    GamePids = maps:get(GameId, State#state.gameplay_pids, []),
    UpdatedGamePids = [Pid | lists:delete(Pid, GamePids)],
    
    % Monitor the process
    erlang:monitor(process, Pid),
    
    NewGameplayPids = maps:put(GameId, UpdatedGamePids, State#state.gameplay_pids),
    {noreply, State#state{gameplay_pids = NewGameplayPids}};

handle_cast({register_games_list_pid, Pid}, State) ->
    % Monitor the process
    erlang:monitor(process, Pid),
    
    UpdatedPids = [Pid | lists:delete(Pid, State#state.games_list_pids)],
    {noreply, State#state{games_list_pids = UpdatedPids}};

handle_cast({unregister_gameplay_pid, GameId, Pid}, State) ->
    GamePids = maps:get(GameId, State#state.gameplay_pids, []),
    UpdatedGamePids = lists:delete(Pid, GamePids),
    
    NewGameplayPids = case UpdatedGamePids of
        [] -> maps:remove(GameId, State#state.gameplay_pids);
        _ -> maps:put(GameId, UpdatedGamePids, State#state.gameplay_pids)
    end,
    
    {noreply, State#state{gameplay_pids = NewGameplayPids}};

handle_cast({unregister_games_list_pid, Pid}, State) ->
    UpdatedPids = lists:delete(Pid, State#state.games_list_pids),
    {noreply, State#state{games_list_pids = UpdatedPids}};

handle_cast({player_disconnected, GameId, Player}, State) ->
    % Start a 30-second disconnect timer for the player
    DisconnectTimerRef = erlang:send_after(30000, self(), {disconnect_timeout, GameId, Player}),
    
    % Store the timer reference
    DisconnectTimers = maps:put({GameId, Player}, DisconnectTimerRef, State#state.disconnect_timers),
    
    {noreply, State#state{disconnect_timers = DisconnectTimers}};

handle_cast({player_reconnected, GameId, Player}, State) ->
    % Cancel the disconnect timer if it exists
    case maps:get({GameId, Player}, State#state.disconnect_timers, undefined) of
        undefined ->
            {noreply, State};
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            DisconnectTimers = maps:remove({GameId, Player}, State#state.disconnect_timers),
            {noreply, State#state{disconnect_timers = DisconnectTimers}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({turn_timeout, GameId, Player}, State) ->
    % Handle turn timeout
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            % Check if it's still this player's turn
            CurrentPlayerIndex = Game#game.currentPlayerIndex,
            Players = Game#game.players,
            
            case length(Players) > 0 andalso lists:nth(CurrentPlayerIndex + 1, Players) =:= Player of
                true ->
                    % It's still their turn, process timeout
                    {_, UpdatedGame} = word_guess_game:turn_timeout(Game),
                    
                    % Save updated game
                    case word_guess_mnesia:save_game(UpdatedGame) of
                        ok ->
                            % Start new turn timer
                            NewState = start_turn_timer(GameId, UpdatedGame, State),
                            
                            % Notify gameplay WebSockets
                            notify_gameplay_update(GameId, UpdatedGame, NewState#state.gameplay_pids),
                            {noreply, NewState};
                        {error, _Reason} ->
                            {noreply, State}
                    end;
                false ->
                    % Turn already changed, ignore
                    {noreply, State}
            end;
        {error, _Reason} ->
            {noreply, State}
    end;

handle_info({disconnect_timeout, GameId, Player}, State) ->
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            % Process player leaving directly
            {_, UpdatedGame} = word_guess_game:player_left(Game, Player),
            
            % Cancel any timers for this player
            State1 = cancel_player_timers(GameId, Player, State),
            
            % Save the updated game
            case word_guess_mnesia:save_game(UpdatedGame) of
                ok -> 
                    % Start new turn timer if needed
                    NewState = case UpdatedGame#game.status of
                        in_progress ->
                            start_turn_timer(GameId, UpdatedGame, State1);
                        _ ->
                            State1
                    end,
                    
                    % Notify gameplay WebSockets
                    notify_gameplay_update(GameId, UpdatedGame, NewState#state.gameplay_pids),
                    
                    % Notify game list WebSockets if game status changed
                    case Game#game.status =/= UpdatedGame#game.status andalso UpdatedGame#game.isPublic of
                        true -> 
                            notify_game_list_update(NewState#state.games_list_pids);
                        false -> 
                            ok
                    end,
                    
                    {noreply, NewState};
                {error, _Reason} ->
                    {noreply, State1}
            end;
        {error, _Reason} ->
            % Game not found, just clear the timer
            DisconnectTimers = maps:remove({GameId, Player}, State#state.disconnect_timers),
            {noreply, State#state{disconnect_timers = DisconnectTimers}}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Handle WebSocket process termination
    % Remove the pid from games_list_pids
    UpdatedListPids = lists:delete(Pid, State#state.games_list_pids),
    
    % Remove the pid from gameplay_pids for all games
    UpdatedGameplayPids = maps:map(
        fun(_GameId, Pids) -> lists:delete(Pid, Pids) end,
        State#state.gameplay_pids
    ),
    
    % Remove empty game entries
    FinalGameplayPids = maps:filter(
        fun(_GameId, Pids) -> length(Pids) > 0 end,
        UpdatedGameplayPids
    ),
    
    {noreply, State#state{games_list_pids = UpdatedListPids, gameplay_pids = FinalGameplayPids}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

generate_game_id() ->
    integer_to_list(erlang:system_time(millisecond)).

can_join_game(Game, Player) ->
    % Check if game is already completed
    case Game#game.status =:= completed of
        true -> {false, game_completed};
        false ->
            % Check if player is already in the game
            case lists:member(Player, Game#game.players) of
                true -> {false, already_joined};
                false ->
                    % Check if game is full
                    case length(Game#game.players) >= Game#game.maxPlayers of
                        true -> {false, game_full};
                        false -> true
                    end
            end
    end.

notify_gameplay_update(GameId, Game, GameplayPids) ->
    case maps:get(GameId, GameplayPids, []) of
        [] -> ok;
        Pids ->
            GameJson = word_guess_game:to_json(Game),
            Message = jsx:encode(#{
                type => <<"game_update">>,
                data => GameJson
            }),
            [Pid ! {gameplay_update, Message} || Pid <- Pids]
    end.

notify_game_list_update(GameListPids) ->
    case word_guess_mnesia:get_public_games() of
        {ok, Games} ->
            GamesJson = [word_guess_game:to_json(Game) || Game <- Games],
            Message = jsx:encode(#{
                type => <<"games_list_update">>,
                data => GamesJson
            }),
            [Pid ! {games_list_update, Message} || Pid <- GameListPids];
        {error, _Reason} ->
            ok
    end.

start_turn_timer(GameId, Game, State) ->
    case Game#game.status of
        in_progress ->
            CurrentPlayerIndex = Game#game.currentPlayerIndex,
            Players = Game#game.players,
            
            case length(Players) > 0 of
                true ->
                    CurrentPlayer = lists:nth(CurrentPlayerIndex + 1, Players),
                    
                    % Cancel any existing timer for this player
                    State1 = cancel_turn_timer(GameId, CurrentPlayer, State),
                    
                    % Start a new 30-second timer
                    TimerRef = erlang:send_after(30000, self(), {turn_timeout, GameId, CurrentPlayer}),
                    
                    % Store the timer reference
                    PlayerTimers = maps:put({GameId, CurrentPlayer}, TimerRef, State1#state.player_timers),
                    State1#state{player_timers = PlayerTimers};
                false ->
                    State
            end;
        _ ->
            State
    end.

cancel_turn_timer(GameId, Player, State) ->
    case maps:get({GameId, Player}, State#state.player_timers, undefined) of
        undefined ->
            State;
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            PlayerTimers = maps:remove({GameId, Player}, State#state.player_timers),
            State#state{player_timers = PlayerTimers}
    end.

cancel_player_timers(GameId, Player, State) ->
    % Cancel turn timer
    State1 = cancel_turn_timer(GameId, Player, State),
    
    % Cancel disconnect timer
    case maps:get({GameId, Player}, State1#state.disconnect_timers, undefined) of
        undefined ->
            State1;
        DisconnectTimerRef ->
            erlang:cancel_timer(DisconnectTimerRef),
            DisconnectTimers = maps:remove({GameId, Player}, State1#state.disconnect_timers),
            State1#state{disconnect_timers = DisconnectTimers}
    end.

