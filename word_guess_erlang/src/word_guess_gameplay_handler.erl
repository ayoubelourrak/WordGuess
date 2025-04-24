%%%-------------------------------------------------------------------
%%% @doc word_guess WebSocket handler for gameplay
%%%-------------------------------------------------------------------
-module(word_guess_gameplay_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-include("word_guess_records.hrl").

%%===================================================================
%% Cowboy WebSocket callbacks
%%===================================================================

init(Req, State) ->
    % Get game ID from path
    GameId = cowboy_req:binding(game_id, Req),
    
    % Get username from query parameters
    QsVals = cowboy_req:parse_qs(Req),
    Username = proplists:get_value(<<"username">>, QsVals),
    
    case Username of
        undefined ->
            % No username provided, reject connection
            Req2 = cowboy_req:reply(401, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"Unauthorized: username required">>, Req),
            {ok, Req2, State};
        Username ->
            % Set up WebSocket with username and game ID
            {cowboy_websocket, Req, #{
                username => binary_to_list(Username),
                game_id => binary_to_list(GameId)
            }}
    end.

websocket_init(State = #{username := Username, game_id := GameId}) ->
    % Register this WebSocket process for gameplay updates
    word_guess_server:register_gameplay_pid(GameId, self()),

    % Record that the player is connected
    word_guess_server:player_reconnected(GameId, Username),
    
    % Send initial game state
    case word_guess_mnesia:get_game(GameId) of
        {ok, Game} ->
            GameJson = word_guess_game:to_json(Game),
            Message = jsx:encode(#{
                type => <<"game_update">>,
                data => GameJson
            }),
            {reply, {text, Message}, State};
        {error, _Reason} ->
            ErrorMessage = jsx:encode(#{
                type => <<"error">>,
                message => <<"Game not found">>
            }),
            {reply, {text, ErrorMessage}, State}
    end.

websocket_handle({text, Msg}, State = #{username := Username, game_id := GameId}) ->
    try jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"guess">>, <<"guess">> := Guess} ->
            % Process the guess
            case word_guess_server:make_guess(GameId, Username, binary_to_list(Guess)) of
                {error, Game} ->
                    ErrorMessage = jsx:encode(#{
                        type => <<"error">>,
                        message => list_to_binary(Game#game.error)
                    }),
                    {reply, {text, ErrorMessage}, State};
                {_Status, _Game} ->
                    {ok, State}
            end;
        #{<<"type">> := <<"ping">>} ->
            Response = jsx:encode(#{
                type => <<"pong">>,
                timestamp => erlang:system_time(millisecond)
            }),
            {reply, {text, Response}, State};
        _ ->
            {ok, State}
    catch
        _:_ ->
            {ok, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({gameplay_update, Message}, State) ->
    {reply, {text, Message}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, #{game_id := GameId, username := Username}) ->
    % Record that the player is disconnected
    word_guess_server:player_disconnected(GameId, Username),
    
    % Unregister this WebSocket process
    word_guess_server:unregister_gameplay_pid(GameId, self()),
    ok;
terminate(_Reason, _PartialReq, _State) ->
    ok.