-module(auth_hand_saml_metadata_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

handle_returns_metadata_xml() ->
    EntName = <<"enterprise">>,
    ConsumeUri = "https://delivery.com/api/v0/e/enterprise/saml/consume",
    EntityId = "https://delivery.com/api/v0/e/enterprise/saml",
    ServiceProviderMetadata = #esaml_sp_metadata{
                                 consumer_location = ConsumeUri,
                                 entity_id = EntityId,
                                 signed_requests = false,
                                 signed_assertions = true
                                },
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(auth_saml_config,
              ?expect(service_provider_metadata,
                      ?withArgs([EntName]),
                      ?andReturn(ServiceProviderMetadata))),
    hoax:mock(esaml,
              ?expect(to_xml,
                      ?withArgs([ServiceProviderMetadata]),
                      ?andReturn(sp_metadata_xmerl_record))),
    hoax:mock(xmerl,
              ?expect(export_simple,
                      ?withArgs([[sp_metadata_xmerl_record], xmerl_xml]),
                      ?andReturn(xml_reply))),
    hoax:mock(cowboy_req,
              ?expect(reply,
                      ?withArgs([200,
                                 [{<<"content-type">>, <<"text/xml">>}],
                                 xml_reply, req1]),
                      ?andReturn({ok, req2}))),

    Actual = auth_hand_saml_metadata:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.
