%%%-------------------------------------------------------------------
%%% @doc word_guess REST user handler
%%%-------------------------------------------------------------------
-module(word_guess_rest_user_handler).
-behaviour(cowboy_rest).

%% REST Callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    handle_request/2
]).

%% Request handlers
-export([
    handle_register/2,
    handle_login/2,
    to_json/2
]).

-include("word_guess_records.hrl").

%%===================================================================
%% Cowboy REST callbacks
%%===================================================================

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

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
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try jsx:decode(Body, [return_maps]) of
        JsonData ->
            case Operation of
                register -> handle_register(JsonData, Req2);
                login -> handle_login(JsonData, Req2)
            end
    catch
        _:_ ->
            Response = jsx:encode(#{
                success => false,
                error => <<"Invalid JSON format">>
            }),
            Req3 = cowboy_req:set_resp_body(Response, Req2),
            {true, Req3, State}
    end.

handle_register(JsonData, Req) ->
    Username = maps:get(<<"username">>, JsonData, <<>>),
    Password = maps:get(<<"password">>, JsonData, <<>>),
    
    % Validate input
    case {Username, Password} of
        {<<>>, _} ->
            Response = jsx:encode(#{
                success => false,
                error => <<"Username is required">>
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, register};
        {_, <<>>} ->
            Response = jsx:encode(#{
                success => false,
                error => <<"Password is required">>
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, register};
        _ ->
            % Convert binary to string
            UsernameStr = binary_to_list(Username),
            PasswordStr = binary_to_list(Password),
            
            % Register user
            case word_guess_server:register_user(UsernameStr, PasswordStr) of
                {ok, _User} ->
                    Response = jsx:encode(#{
                        success => true,
                        message => <<"User registered successfully">>
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, register};
                {error, username_taken} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => <<"Username already taken">>
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, register};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, register}
            end
    end.

handle_login(JsonData, Req) ->
    Username = maps:get(<<"username">>, JsonData, <<>>),
    Password = maps:get(<<"password">>, JsonData, <<>>),
    
    % Validate input
    case {Username, Password} of
        {<<>>, _} ->
            Response = jsx:encode(#{
                success => false,
                error => <<"Username is required">>
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, login};
        {_, <<>>} ->
            Response = jsx:encode(#{
                success => false,
                error => <<"Password is required">>
            }),
            Req2 = cowboy_req:set_resp_body(Response, Req),
            {true, Req2, login};
        _ ->
            % Convert binary to string
            UsernameStr = binary_to_list(Username),
            PasswordStr = binary_to_list(Password),
            
            % Login user
            case word_guess_server:login_user(UsernameStr, PasswordStr) of
                {ok, _User} ->
                    Response = jsx:encode(#{
                        success => true,
                        username => Username
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, login};
                {error, invalid_credentials} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => <<"Invalid username or password">>
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, login};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        success => false,
                        error => list_to_binary(io_lib:format("Error: ~p", [Reason]))
                    }),
                    Req2 = cowboy_req:set_resp_body(Response, Req),
                    {true, Req2, login}
            end
    end.

to_json(Req, State) ->
    {<<"{}">>, Req, State}.