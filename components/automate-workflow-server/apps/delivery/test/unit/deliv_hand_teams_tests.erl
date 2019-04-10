-module(deliv_hand_teams_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

-define(TEST_ENT, <<"TestEnt">>).
-define(TEST_USER, <<"TestUser">>).
-define(TEST_USER_ID, 42).
-define(TEAM_NAME, <<"a_team_name">>).
-define(TEAM_DESC, <<"A Description">>).
-define(UPDATED_TIME, {{2016, 8, 24}, {23,2,42}}).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, "to_json_"),
     hoax:fixture(?MODULE, "from_json_")
    ].

content_types_provided_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'}, to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_teams:content_types_provided(request, state)).

content_types_accepted_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'}, from_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_teams:content_types_accepted(request, state)).

allowed_methods_returns_get_and_post_test() ->
    ?assertEqual({[<<"GET">>, <<"POST">>], request, state}, deliv_hand_teams:allowed_methods(request, state)).

to_json_returns_list_of_teams() ->
    State = make_state(),
    TeamList = [ <<"team1">>, <<"team2">>, <<"team3">> ],

    hoax:expect(
      receive
          cowboy_req:set_resp_header(<<"content-type">>,<<"application/json">>,request2) ->
              {?TEST_ENT, request2};
          deliv_team:fetch_names(?TEST_ENT) -> TeamList

      end),

    ExpectedJson = {[ {<<"teams">>, TeamList },
                      {<<"_links">>,
                       {[{ <<"self">>,
                           {[{<<"href">>,<<"/api/v0/e/TestEnt/teams">>}]}},
                         {<<"team">>,
                          {[{<<"href">>,<<"/api/v0/e/TestEnt/teams/{team_name}">>},
                            {<<"templated">>,true}]}}
                        ]}}
                    ]},
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_teams:to_json(request2, State)),
    ?verifyAll.

to_json_when_fetch_returns_error_returns_server_error() ->
    State = make_state(),

    hoax:expect(
      receive
          deliv_team:fetch_names(?TEST_ENT) -> {error, something_bad};
          deliv_web_utils:error_response(500, internal_server_error, req, State) ->
              {error, server_error}
      end),

    ?assertEqual({error, server_error}, deliv_hand_teams:to_json(req, State)),
    ?verifyAll.

from_json_returns_success_with_valid_data() ->
    ExpectedJson = make_team_json(),
    ExpectedTeam = make_team(),
    State = make_state(),

    hoax:expect(
      receive
          deliv_web_utils:parse_json_req(req) -> {ExpectedJson, req1};
          deliv_user:fetch(?TEST_ENT, ?TEST_USER) -> {ok, d_user};
          deliv_user:getval(id, d_user) -> ?TEST_USER_ID;
          deliv_team:insert(?TEST_ENT, ExpectedTeam) -> {d_team}
      end),

    ?assertEqual({true, req1, State}, deliv_hand_teams:from_json(req, State) ),

    ?verifyAll.

from_json_returns_error_when_it_cannot_parse() ->
    State = make_state(),

    hoax:expect(
      receive
          deliv_web_utils:parse_json_req(req) -> {{error, reason}, req1};
          deliv_web_utils:error_response(400, bad_request, req1, State) -> error
      end),

    ?assertEqual(error, deliv_hand_teams:from_json(req, State) ),

    ?verifyAll.

from_json_returns_409_when_team_already_exists() ->
    ExpectedJson = make_team_json(),
    ExpectedTeam = make_team(),
    State = make_state(),

    hoax:expect(
      receive
          deliv_web_utils:parse_json_req(req) -> {ExpectedJson, req1};
          deliv_web_utils:error_response(409, conflict, req1, State) -> error;
          deliv_user:fetch(?TEST_ENT, ?TEST_USER) -> {ok, d_user};
          deliv_user:getval(id, d_user) -> ?TEST_USER_ID;
          deliv_team:insert(?TEST_ENT, ExpectedTeam) -> {error, conflict}
      end),

    ?assertEqual(error, deliv_hand_teams:from_json(req, State) ),

    ?verifyAll.

make_state() ->
    #handler{ent_name = ?TEST_ENT,
             user_name = ?TEST_USER}.

make_team() ->
    [{name, ?TEAM_NAME}, {description, ?TEAM_DESC}, {creator_id, ?TEST_USER_ID},
     {updater_id, ?TEST_USER_ID}, {updated_at, <<"now()">>} ].

make_team_json() ->
    {[ { <<"name">>, ?TEAM_NAME },
       { <<"description">>, ?TEAM_DESC }]}.
