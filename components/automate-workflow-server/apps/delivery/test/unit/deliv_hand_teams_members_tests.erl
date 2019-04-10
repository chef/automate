-module(deliv_hand_teams_members_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, "to_json_")
    ].

allowed_methods_returns_list_of_allowed_methods_test() ->
    ?assertEqual({[<<"GET">>], request, state}, deliv_hand_teams_named:allowed_methods(request, state)).

content_types_provided_returns_application_json_test() ->
    ExpectedContentType = {[{{<<"application">>, <<"json">>, '*'}, to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_teams_named:content_types_provided(request, state)).

to_json_when_fetch_succeeds_returns_team_members_json() ->
    EntName = <<"TestEnt">>,
    UserName = <<"TestUser">>,
    TeamName = <<"TestTeam">>,
    State = #handler{ent_name = EntName, user_name = UserName},

    MembersEjson = [{[
                      {<<"username">>, <<"bsmith">>},
                      {<<"fullname">>, <<"Bob Smith">>},
                      {<<"role">>, <<"admin">>}
                     ]},
                     {[
                      {<<"username">>, <<"asmith">>},
                      {<<"fullname">>, <<"Alice Smith">>},
                      {<<"role">>, <<"admin">>}
                     ]},
                     {[
                      {<<"username">>, <<"csmith">>},
                      {<<"fullname">>, <<"Carl Smith">>},
                      {<<"role">>, <<"admin">>}
                     ]}],

    ExpectedResponse = {[{<<"members">>, MembersEjson}]},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name], req) ->
                {[EntName, TeamName], req1};
            deliv_web_utils:content(ExpectedResponse, req1, State) ->
                {ExpectedResponse, req2, State};
            deliv_team:fetch_members(EntName, TeamName) ->
                MembersEjson
        end),

    ActualResult = deliv_hand_teams_members:to_json(req, State),
    ?assertEqual({ExpectedResponse, req2, State}, ActualResult),
    ?verifyAll.

to_json_when_fetch_fails_logs_and_returns_server_error() ->
    EntName = <<"TestEnt">>,
    UserName = <<"TestUser">>,
    TeamName = <<"TestTeam">>,
    State = #handler{ent_name = EntName, user_name = UserName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name], req) ->
                {[EntName, TeamName], req1};
            deliv_web_utils:error_response(500, internal_server_error, req1, State) ->
                error;
            chef_log:error("Fetch failed for members of team ~s: ~p", [TeamName, kaboom]) ->
                ok;
            deliv_team:fetch_members(EntName, TeamName) ->
                {error, kaboom}
        end),

    Result = deliv_hand_teams_members:to_json(req, State),
    ?assertEqual(error, Result),
    ?verifyAll.
