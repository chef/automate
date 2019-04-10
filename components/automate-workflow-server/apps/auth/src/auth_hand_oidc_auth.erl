-module(auth_hand_oidc_auth).
-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init(_Transport, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    process_request(cowboy_req:method(Req), State).

process_request({<<"GET">>, Req}, State) ->
    process_get(Req, State);
process_request({_, Req}, _) ->
    cowboy_req:reply(405, Req).

process_get(Req, State) ->
    {ok, Req2, Params} = auth_oidc_utils:auth_request_vals(Req),
    process_response_type(proplists:get_value(<<"response_type">>, Params),
                          Params, Req2, State).

%% NOTE Error responses are defined in
%% https://tools.ietf.org/html/rfc6749#section-4.2.2.1
process_response_type(<<"code">>, Params, Req, State) ->
    process_code(Params, Req, State);
process_response_type(_, Params, Req, State) ->
    ClientState = proplists:get_value(<<"state">>, Params, <<>>),
    RedirectUri = proplists:get_value(<<"redirect_uri">>, Params),
    auth_oidc_utils:redirect_error_response(RedirectUri, unsupported_response_type, ClientState, Req, State).

process_code(Params, Req, State) ->
    ClientState = proplists:get_value(<<"state">>, Params, <<>>),
    ScopeString = proplists:get_value(<<"scope">>, Params, <<>>),
    ClientId    = proplists:get_value(<<"client_id">>, Params),
    RedirectUri = proplists:get_value(<<"redirect_uri">>, Params),
    Scopes = binary:split(ScopeString, <<" ">>, [global]),

    User = undefined, %% we don't know the user yet (but that's ok)
    case oauth2:authorize_code_request(User, ClientId, RedirectUri, Scopes, State) of
        {ok, {_AppCtx, _Auth}} ->
            Relay = auth_saml_utils:encode_relaystate([{oidc_redirect, RedirectUri},
                                                       {oidc_client_id, ClientId},
                                                       {oidc_scopes, Scopes},
                                                       {client_state, ClientState}]),
            Target = auth_saml_utils:make_redirect_target(Relay),
            deliv_web_utils:redirect_302(Target, Req, State);
        {error, Reason} ->
            auth_oidc_utils:redirect_error_response(RedirectUri, Reason, ClientState, Req, State)
    end.

terminate(_, _, _) -> ok.
