-module(deliv_hand_teams_named_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).



-define(TEST_ENT, <<"Calumet">>).
-define(TEAM_NAME, <<"Wolverines">>).
-define(TEAM_DESC, <<"Wolverines!!!">>).
-define(UpdatedAt, {{2016,09,15}, {15,47,22}}).
-define(CreatorName, <<"creator">>).
-define(UpdaterName, <<"updater">>).
-define(CreatorId, 2001).
-define(UpdaterId, 2002).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, "to_json_")
    ].

allowed_methods_returns_list_of_allowed_methods_test() ->
    ?assertEqual({[<<"GET">>], request, state}, deliv_hand_teams_named:allowed_methods(request, state)).

content_types_provided_returns_application_json_test() ->
    ExpectedContentType = {[{{<<"application">>, <<"json">>, '*'}, to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_teams_named:content_types_provided(request, state)).

to_json_when_team_exists_returns_team_json() ->
    State = #handler{ent_name = ?TEST_ENT},
    Team = deliv_team:fromlist([
                                {name, ?TEAM_NAME},
                                {description, ?TEAM_DESC},
                                {updated_at, ?UpdatedAt},
                                {creator_id, ?CreatorId},
                                {updater_id, ?UpdaterId}
                               ]),
    Ejson = make_team_json(),

    hoax:expect(
      receive
          deliv_user:fetch(?CreatorId) -> {ok, {d_user, creator}};
          deliv_user:fetch(?UpdaterId) -> {ok, {d_user, updater}};
          deliv_user:getval(name, {d_user, creator}) -> ?CreatorName;
          deliv_user:getval(name, {d_user, updater}) -> ?UpdaterName;

          deliv_web_utils:extract_bindings([ent_name, team_name], req) ->
              {[?TEST_ENT, ?TEAM_NAME], req2};
          deliv_web_utils:content(Ejson, req2, State) ->
              {body, req3, State};

          deliv_team:fetch(?TEST_ENT, ?TEAM_NAME) -> {ok, Team};
          deliv_team:getval(name, Team) -> ?TEAM_NAME;
          deliv_team:getval(description, Team) -> ?TEAM_DESC;
          deliv_team:getval(updated_at, Team) -> ?UpdatedAt;
          deliv_team:getval(creator_id, Team) -> ?CreatorId;
          deliv_team:getval(updater_id, Team) -> ?UpdaterId
      end),

    ActualResult = deliv_hand_teams_named:to_json(req, State),
    ?assertEqual({body, req3, State}, ActualResult),
    ?verifyAll.

to_json_when_team_does_not_exist_returns_not_found() ->
    State = #handler{ent_name = ?TEST_ENT},

    hoax:expect(
      receive
          deliv_web_utils:extract_bindings([ent_name, team_name], req) ->
              {[?TEST_ENT, ?TEAM_NAME], req2};
          deliv_web_utils:error_response(404, not_found, req2, State) ->
              {body, req3, State};
          deliv_team:fetch(?TEST_ENT, ?TEAM_NAME) ->
              {error, not_found}
      end),

    ActualResult = deliv_hand_teams_named:to_json(req, State),
    ?assertEqual({body, req3, State}, ActualResult),
    ?verifyAll.

to_json_when_fetch_fails_logs_and_returns_server_error() ->
    State = #handler{ent_name = ?TEST_ENT},

    hoax:expect(
      receive
          deliv_web_utils:extract_bindings([ent_name, team_name], req) ->
              {[?TEST_ENT, ?TEAM_NAME], req2};
          deliv_web_utils:error_response(500, internal_server_error, req2, State) ->
              error;
          chef_log:error("Fetch failed for team ~s: ~p", [?TEAM_NAME, kaboom]) -> ok;
          deliv_team:fetch(?TEST_ENT, ?TEAM_NAME) -> {error, kaboom}
      end),


    ActualResult = deliv_hand_teams_named:to_json(req, State),
    ?assertEqual(error, ActualResult),
    ?verifyAll.

make_team_json() ->
    {[
      {<<"name">>, ?TEAM_NAME},
      {<<"description">>, ?TEAM_DESC},
      {<<"creator_name">>, ?CreatorName},
      {<<"updater_name">>, ?UpdaterName},
      {<<"updated_at">>, chef_utils:format_timestamp(?UpdatedAt) }
     ]}.
