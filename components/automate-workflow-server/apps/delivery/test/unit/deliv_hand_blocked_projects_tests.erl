-module(deliv_hand_blocked_projects_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

to_json_returns_error_response_404_when_enterprise_not_found() ->
    EntName = <<"foo">>,
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_dependency_failures,
              ?expect(get_blocked_project_names,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, enterprise_not_found, req, State]),
                      ?andReturn(error_response))),

    deliv_hand_blocked_projects:to_json(req, State),
    ?verifyAll.

to_json_returns_blocked_projects_list() ->
    EntName = <<"foo">>,
    State = #handler{ent_name = EntName},
    ExpectedJson = {[{<<"blocked_projects">>, [<<"Project1">>, <<"Project2">>]}]},

    hoax:mock(deliv_dependency_failures,
              ?expect(get_blocked_project_names,
                      ?withArgs([EntName]),
                      ?andReturn({ok, [<<"Project1">>, <<"Project2">>]}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([ExpectedJson, req, State]),
                      ?andReturn(error_response))),

    deliv_hand_blocked_projects:to_json(req, State),
    ?verifyAll.
