%%%-------------------------------------------------------------------
%%% @doc word_guess Mnesia database interaction layer
%%%-------------------------------------------------------------------
-module(word_guess_mnesia).

%% API
-export([
    init/0,
    register_user/2,
    login_user/2,
    save_game/1,
    get_game/1,
    get_public_games/0,
    reset_database/0  % New function to reset the database
]).

-include("word_guess_records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%===================================================================
%% API
%%===================================================================

%% @doc Reset the Mnesia database completely
-spec reset_database() -> ok.
reset_database() ->
    distributed_node(),
    
    % Stop Mnesia if it's running
    case mnesia:system_info(is_running) of
        yes -> 
            mnesia:stop();
        no -> 
            ok
    end,
    
    % Delete the schema
    io:format("Deleting Mnesia schema...~n"),
    mnesia:delete_schema([node()]),
    
    % Reinitialize the database
    io:format("Reinitializing database...~n"),
    init(),
    
    io:format("Database reset complete.~n"),
    ok.

%% @doc Initialize Mnesia database
-spec init() -> ok.
init() ->
    distributed_node(),
    
    % Set up directory
    application:set_env(mnesia, dir, "mnesia_data"),
    
    % Create schema if it doesn't exist
    case mnesia:create_schema([node()]) of
        ok -> 
            io:format("Schema created successfully~n");
        {error, {_, {already_exists, _}}} -> 
            io:format("Schema already exists~n");
        Error -> 
            io:format("Error creating schema: ~p~n", [Error])
    end,
    
    % Start Mnesia
    ok = mnesia:start(),
    
    % Create user table with disc_copies
    case mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> 
            io:format("User table created successfully~n");
        {aborted, {already_exists, user}} -> 
            io:format("User table already exists~n");
        UserError -> 
            io:format("Error creating user table: ~p~n", [UserError])
    end,
    
    % Create game table with disc_copies
    case mnesia:create_table(game, [
        {attributes, record_info(fields, game)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> 
            io:format("Game table created successfully~n");
        {aborted, {already_exists, game}} -> 
            io:format("Game table already exists~n");
        GameError -> 
            io:format("Error creating game table: ~p~n", [GameError])
    end,
    
    % Wait for tables to be available
    case mnesia:wait_for_tables([user, game], 30000) of
        ok -> 
            io:format("Tables are now available~n");
        {timeout, BadTables} -> 
            io:format("Timeout waiting for tables: ~p~n", [BadTables]);
        {error, Reason} -> 
            io:format("Error waiting for tables: ~p~n", [Reason])
    end,
    ok.

%% Running with a proper distributed node name
distributed_node() ->
    case node() of
        'nonode@nohost' ->
            % Generate a node name based on hostname
            {ok, Hostname} = inet:gethostname(),
            NodeName = list_to_atom("word_guess@" ++ Hostname),
            net_kernel:start([NodeName, shortnames]),
            io:format("Started node as: ~p~n", [node()]);
        _ ->
            % Already running with a proper node name
            ok
    end.

%% @doc Register a new user
-spec register_user(string(), string()) -> {ok, #user{}} | {error, term()}.
register_user(Username, Password) ->
    F = fun() ->
        % Check if username already exists
        case mnesia:read(user, Username) of
            [] ->
                % Hash the password
                HashedPassword = crypto:hash(sha256, Password),
                
                % Create new user
                User = #user{
                    username = Username,
                    password_hash = HashedPassword,
                    createdAt = erlang:system_time(seconds)
                },
                
                % Write to database
                mnesia:write(User),
                {ok, User};
            [_] ->
                mnesia:abort(username_taken)
        end
    end,
    
    case mnesia:transaction(F) of
        {atomic, {ok, User}} -> {ok, User};
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Login user with username and password
-spec login_user(string(), string()) -> {ok, #user{}} | {error, term()}.
login_user(Username, Password) ->
    F = fun() ->
        case mnesia:read(user, Username) of
            [] ->
                mnesia:abort(invalid_credentials);
            [User] ->
                % Hash the provided password and compare
                HashedPassword = crypto:hash(sha256, Password),
                case HashedPassword =:= User#user.password_hash of
                    true -> {ok, User};
                    false -> mnesia:abort(invalid_credentials)
                end
        end
    end,
    
    case mnesia:transaction(F) of
        {atomic, {ok, User}} -> {ok, User};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Save game to database
-spec save_game(#game{}) -> ok | {error, term()}.
save_game(Game) ->
    F = fun() ->
        mnesia:write(Game)
    end,
    
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get game by ID
-spec get_game(string()) -> {ok, #game{}} | {error, term()}.
get_game(GameId) ->
    F = fun() ->
        case mnesia:read(game, GameId) of
            [] -> {error, game_not_found};
            [Game] -> {ok, Game}
        end
    end,
    
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get all public games
-spec get_public_games() -> {ok, [#game{}]} | {error, term()}.
get_public_games() ->
    % Create a match specification for public games that are not completed
    MatchSpec = ets:fun2ms(
        fun(Game = #game{isPublic = true, status = waiting}) ->
            Game
        end
    ),
    
    % Execute the query
    F = fun() ->
        mnesia:select(game, MatchSpec)
    end,
    
    case mnesia:transaction(F) of
        {atomic, Games} -> {ok, Games};
        {aborted, Reason} -> {error, Reason}
    end.