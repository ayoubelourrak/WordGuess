%%%-------------------------------------------------------------------
%%% @doc word_guess REST game handler
%%%-------------------------------------------------------------------
-module(word_guess_rest_game_handler).
-behaviour(cowboy_rest).

%% REST Callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

%% Request handlers
-export([
    handle_request/2,
    to_json/2
]).

-include("word_guess_records.hrl").

%%===================================================================
%% Cowboy REST callbacks
%%===================================================================


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    case proplists:get_value(operation, State) of
        list -> {[<<"GET">>], Req, State};
        get -> {[<<"GET">>], Req, State};
        _ -> {[<<"POST">>], Req, State}
    end.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_request}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

%%===================================================================
%% Request handlers
%%===================================================================

handle_request(Req, State) ->
    Operation = proplists:get_value(operation, State),
    case Operation of
        list ->
            handle_list_games(Req, State);
        get ->
            GameId = cowboy_req:binding(game_id, Req),
            handle_get_game(binary_to_list(GameId), Req, State);
        create ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            try jsx:decode(Body, [return_maps]) of
                JsonData ->
                    handle_create_game(JsonData, Req2, State)
            catch
                _:_ ->
                    Response = jsx:encode(#{
                        success => false,
                        error => <<"Invalid JSON format">>
                    }),
                    Req3 = cowboy_req:set_resp_body(Response, Req2),
                    {true, Req3, State}
            end;
        join ->
            GameId = cowboy_req:binding(game_id, Req),
            {ok, _Body, Req2} = cowboy_req:read_body(Req),
            handle_join_game(binary_to_list(GameId), Req2, State);
        start ->
            GameId = cowboy_req:binding(game_id, Req),
            {ok, _Body, Req2} = cowboy_req:read_body(Req),
            handle_start_game(binary_to_list(GameId), Req2, State);
        leave ->
            GameId = cowboy_req:binding(game_id, Req),
            {ok, _Body, Req2} = cowboy_req:read_body(Req),
            handle_leave_game(binary_to_list(GameId), Req2, State)
    end.

to_json(Req, State) ->
    Operation = proplists:get_value(operation, State),
    
    case Operation of
        list ->
            case validate_session(Req) of
                {ok, _Username} ->
                    case word_guess_server:get_public_games() of
                        {ok, Games} ->
                            GamesJson = [word_guess_game:to_json(Game) || Game <- Games],
                            Response = jsx:encode(#{
                                success => true,
                                games => GamesJson
                            }),
                            {Response, Req, State};
                        {error, Reason} ->
                            Response = jsx:encode(#{
                                success => false,
                                error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                            }),
                            {Response, Req, State}
                    end;
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(Reason)
                    }),
                    {Response, Req, State}
            end;
        get ->
            case validate_session(Req) of
                {ok, _Username} ->
                    GameId = cowboy_req:binding(game_id, Req),
                    case word_guess_server:get_game(binary_to_list(GameId)) of
                        {ok, Game} ->
                            GameJson = word_guess_game:to_json(Game),
                            Response = jsx:encode(#{
                                success => true,
                                game => GameJson
                            }),
                            {Response, Req, State};
                        {error, Reason} ->
                            Response = jsx:encode(#{
                                success => false,
                                error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                            }),
                            {Response, Req, State}
                    end;
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(Reason)
                    }),
                    {Response, Req, State}
            end;
        _ ->
            {<<"{}">>, Req, State}
    end.

%%===================================================================
%% Internal functions
%%===================================================================

handle_list_games(Req, State) ->
    % Handled by to_json for GET requests
    {true, Req, State}.

handle_get_game(_GameId, Req, State) ->
    % Handled by to_json for GET requests
    {true, Req, State}.

handle_create_game(JsonData, Req, State) ->
    % Extract username from session
    case validate_session(Req) of
        {ok, Username} ->
            MaxPlayers = maps:get(<<"maxPlayers">>, JsonData, 2),
            IsPublic = maps:get(<<"isPublic">>, JsonData, true),
            
            % Create the game
            case word_guess_server:create_game(Username, MaxPlayers, IsPublic) of
                {ok, Game} ->
                    GameJson = word_guess_game:to_json(Game),
                    Response = jsx:encode(#{
                        success => true,
                        game => GameJson
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            Response = jsx:encode(#{
                success => false,
                error => list_to_binary(Reason)
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, State}
    end.

handle_join_game(GameId, Req, State) ->
    % Extract username from session
    case validate_session(Req) of
        {ok, Username} ->
            % Join the game
            case word_guess_server:join_game(GameId, Username) of
                {ok, Game} ->
                    GameJson = word_guess_game:to_json(Game),
                    Response = jsx:encode(#{
                        success => true,
                        game => GameJson
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            Response = jsx:encode(#{
                success => false,
                error => list_to_binary(Reason)
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, State}
    end.

handle_start_game(GameId, Req, State) ->
    % Extract username from session
    case validate_session(Req) of
        {ok, Username} ->
            % Start the game
            case word_guess_server:start_game(GameId, Username) of
                {ok, Game} ->
                    GameJson = word_guess_game:to_json(Game),
                    Response = jsx:encode(#{
                        success => true,
                        game => GameJson
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State};
                {error, Error} when is_list(Error) ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(Error)
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            Response = jsx:encode(#{
                success => false,
                error => list_to_binary(Reason)
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, State}
    end.

handle_leave_game(GameId, Req, State) ->
    % Extract username from session
    case validate_session(Req) of
        {ok, Username} ->
            % Leave the game
            case word_guess_server:leave_game(GameId, Username) of
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State};
                {Status, Game} ->
                    GameJson = word_guess_game:to_json(Game),
                    Response = jsx:encode(#{
                        success => true,
                        status => atom_to_binary(Status, utf8),
                        game => GameJson
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            Response = jsx:encode(#{
                success => false,
                error => list_to_binary(Reason)
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, State}
    end.

validate_session(Req) ->
    % validate from a custom header field
    case cowboy_req:header(<<"x-username">>, Req) of
        undefined ->
            {error, "Username header missing"};
        Username ->
            {ok, binary_to_list(Username)}
    end.
