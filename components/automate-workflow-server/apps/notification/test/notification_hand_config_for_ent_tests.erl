-module(notification_hand_config_for_ent_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

allowed_methods_returns_get() ->
    ?assertEqual({[<<"GET">>], request, state}, notification_hand_config_for_ent:allowed_methods(request, state)).

to_json_if_there_is_existing_configuration_returns_a_list_with_configuration() ->
    ConfigEjson = {[{<<"notifications">>, [<<"smtp">>]}]},
    EntName = <<"NCC-1701">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([config]))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([ConfigEjson, req1, state]),
                      ?andReturn({body, req2, state}))),

    ?assertEqual({body, req2, state}, notification_hand_config_for_ent:to_json(req, state)),
    ?verifyAll.

to_json_if_there_is_no_configuration_returns_empty_list() ->
    ConfigEjson = {[{<<"notifications">>, []}]},
    EntName = <<"NCC-1701">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([]))),
    hoax:mock(deliv_web_utils,
             ?expect(content,
                     ?withArgs([ConfigEjson, req1, state]),
                     ?andReturn({body, req2, state}))),

    ?assertEqual({body, req2, state}, notification_hand_config_for_ent:to_json(req, state)),
    ?verifyAll.

to_json_if_there_is_another_error_returns_500() ->
    EntName = <<"NCC-1701">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, why}))),
     hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, state]),
                      ?andReturn({body, req2, state}))),

    ?assertEqual({body, req2, state}, notification_hand_config_for_ent:to_json(req, state)),
    ?verifyAll.
