%%%-------------------------------------------------------------------
%% @doc word_guess_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(word_guess_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Initialize Mnesia database
    ok = word_guess_mnesia:init(),
    
    % Define routes for Cowboy
    Dispatch = cowboy_router:compile([
        {'_', [
            % REST endpoints
            {"/api/users/register", word_guess_rest_user_handler, [{operation, register}]},
            {"/api/users/login", word_guess_rest_user_handler, [{operation, login}]},
            {"/api/games", word_guess_rest_game_handler, [{operation, list}]},
            {"/api/games/create", word_guess_rest_game_handler, [{operation, create}]},
            {"/api/games/:game_id", word_guess_rest_game_handler, [{operation, get}]},
            {"/api/games/:game_id/join", word_guess_rest_game_handler, [{operation, join}]},
            {"/api/games/:game_id/start", word_guess_rest_game_handler, [{operation, start}]},
            {"/api/games/:game_id/leave", word_guess_rest_game_handler, [{operation, leave}]},
            
            % WebSocket endpoints
            {"/ws/games", word_guess_games_handler, []},
            {"/ws/gameplay/:game_id", word_guess_gameplay_handler, []}
        ]}
    ]),
    
    % Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(
        word_guess_http_listener,
        [{ip, {0,0,0,0}}, {port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    % Start supervisor
    word_guess_erlang_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
