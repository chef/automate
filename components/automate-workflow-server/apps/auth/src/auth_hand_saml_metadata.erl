-module(auth_hand_saml_metadata).
-behaviour(cowboy_http_handler).

-export ([
          init/3,
          handle/2,
          terminate/3
         ]).

init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {[EntName], Req1} = deliv_web_utils:extract_bindings([ent_name], Req),
    ServiceProviderMetadata = auth_saml_config:service_provider_metadata(EntName),
    Xml = esaml:to_xml(ServiceProviderMetadata),
    Export = xmerl:export_simple([Xml], xmerl_xml),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/xml">>}], Export, Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.
