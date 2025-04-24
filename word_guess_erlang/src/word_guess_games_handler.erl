%%%-------------------------------------------------------------------
%%% @doc word_guess WebSocket handler for game list updates
%%%-------------------------------------------------------------------
-module(word_guess_games_handler).
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
    % Get username from query parameters
    QsVals = cowboy_req:parse_qs(Req),
    Username = proplists:get_value(<<"username">>, QsVals),
    
    case Username of
        undefined ->
            Req2 = cowboy_req:reply(401, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"Unauthorized: username required">>, Req),
            {ok, Req2, State};
        Username ->
            % Set up WebSocket with username
            {cowboy_websocket, Req, #{username => binary_to_list(Username)}}
    end.

websocket_init(State) ->
    % Register this WebSocket process for game list updates
    word_guess_server:register_games_list_pid(self()),
    
    % Send initial list of games
    case word_guess_mnesia:get_public_games() of
        {ok, Games} ->
            GamesJson = [word_guess_game:to_json(Game) || Game <- Games],
            Message = jsx:encode(#{
                type => <<"games_list_update">>,
                data => GamesJson
            }),
            {reply, {text, Message}, State};
        {error, _Reason} ->
            {ok, State}
    end.

websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
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

websocket_info({games_list_update, Message}, State) ->
    {reply, {text, Message}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    % Unregister this WebSocket process
    word_guess_server:unregister_games_list_pid(self()),
    ok.