-module(auth_hand_saml_auth).
-behaviour(cowboy_http_handler).

-export ([
          init/3,
          handle/2,
          terminate/3
         ]).

init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    redirect_to_saml_login(Req, State).

redirect_to_saml_login(Req, State) ->
    {[EntName], Req1} = deliv_web_utils:extract_bindings([ent_name], Req),
    {ReturnUrl, Req2} = cowboy_req:qs_val(<<"returnUrl">>, Req1),
    RelayState = auth_saml_utils:relay_url(ReturnUrl),
    Target = auth_saml_utils:make_redirect_target(EntName, RelayState),
    deliv_web_utils:redirect_302(Target, Req2, State).

terminate(_Reason, _Req, _State) -> ok.
