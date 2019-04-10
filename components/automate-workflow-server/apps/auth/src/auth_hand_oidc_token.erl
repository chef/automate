-module(auth_hand_oidc_token).
-behaviour(cowboy_http_handler).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init(_Transport, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    process_request(cowboy_req:method(Req), State).

process_request({<<"POST">>, Req}, State) ->
    process_post(Req, State);
process_request({_, Req}, _) ->
    cowboy_req:reply(405, Req).

process_post(Req, State) ->
    {ok, ContentType, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
    case ContentType of
        { <<"application">>, <<"x-www-form-urlencoded">>, _} ->
            {ok, Params, Req3} = cowboy_req:body_qs(Req2),
            process_auth_code(proplists:get_value(<<"grant_type">>, Params), Params, Req3, State);
        _ ->
            chef_log:error("Invalid content type ~p", [ContentType]),
            ok(deliv_web_utils:error_response(400, invalid_request, Req2, State))
    end.

%% https://tools.ietf.org/html/rfc6749#section-5.2 defines error responses,
%% the oauth2 library returns one of these.
process_auth_code(<<"authorization_code">>, Params, Req, State) ->
    ClientId     = proplists:get_value(<<"client_id">>, Params),
    ClientSecret = proplists:get_value(<<"client_secret">>, Params),
    RedirectUri  = proplists:get_value(<<"redirect_uri">>, Params),
    Code         = proplists:get_value(<<"code">>, Params),

    case oauth2:authorize_code_grant({ClientId, ClientSecret}, Code, RedirectUri, none) of
        {ok, {_AppCtx, Auth}} ->
            EntName = auth_oidc_backend:get_enterprise(Auth),
            User = auth_oidc_backend:get_user(Auth),
            case auth_oidc_utils:access_token_response(EntName, User, ClientId) of
                {ok, Ejson} -> {Body, Req2, State2} = deliv_web_utils:content(Ejson, Req, State),
                               {ok, Reply} = cowboy_req:reply(200, [], Body, Req2),
                               {ok, Reply, State2};
                % this is none of the defined responses, but this only happens
                % if no shared key is configured
                {error, no_key} ->
                    chef_log:error("No shared key configured for OIDC"),
                    ok(deliv_web_utils:error_response(500, internal_server_error, Req, State))
            end;
        {error, Reason} -> ok(deliv_web_utils:error_response(400, Reason, Req, State))
    end;
process_auth_code(_, _, Req, State) ->
    ok(deliv_web_utils:error_response(400, unsupported_grant_type, Req, State)).

%% this is for re-using deliv_web_utils:error_response/4 in a plain-http cowboy
%% handler
-spec ok({halt, cowboy_req(), State}) -> {ok, cowboy_req(), State} when
    State :: any().
ok({halt, Req, State}) -> {ok, Req, State}.

terminate(_, _, _) -> ok.
