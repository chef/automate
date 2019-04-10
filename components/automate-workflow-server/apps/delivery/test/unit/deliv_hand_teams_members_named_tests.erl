-module(deliv_hand_teams_members_named_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, "from_json_")
    ].

allowed_methods_returns_list_of_allowed_methods_test() ->
    ?assertEqual({[<<"PUT">>], request, state}, deliv_hand_teams_members_named:allowed_methods(request, state)).

content_types_accepted_returns_application_json_test() ->
    ExpectedContentType = {[{{<<"application">>, <<"json">>,'*'}, from_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_teams_members_named:content_types_accepted(request, state)).

from_json_with_valid_team_and_user_returns_no_content() ->
    EntName = <<"my-ent">>,
    TeamName = <<"my-team">>,
    TeamId = 1,
    MemberName = <<"some-user">>,
    MemberId = 1,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {ok, team};
            deliv_team:getval(id, team) ->
                TeamId;
            deliv_user:fetch(EntName, MemberName) ->
                {ok, user};
            deliv_user:getval(id, user) ->
                MemberId;
            deliv_team:add_member(TeamId, MemberId) ->
                {ok, cool}
        end),

    ?assertEqual({true, req1, State}, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_when_membership_exists_returns_no_content() ->
    EntName = <<"my-ent">>,
    TeamName = <<"my-team">>,
    TeamId = 1,
    MemberName = <<"some-user">>,
    MemberId = 1,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {ok, team};
            deliv_team:getval(id, team) ->
                TeamId;
            deliv_user:fetch(EntName, MemberName) ->
                {ok, user};
            deliv_user:getval(id, user) ->
                MemberId;
            deliv_team:add_member(TeamId, MemberId) ->
                {error, {conflict, details}}
        end),

    ?assertEqual({true, req1, State}, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_with_invalid_team_returns_not_found() ->
    EntName = <<"my-ent">>,
    TeamName = <<"some-team">>,
    MemberName = <<"some-user">>,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {error, not_found};
            deliv_web_utils:error_response(404, not_found, req1, State) ->
                error
        end),

    ?assertEqual(error, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_with_invalid_user_returns_not_found() ->
    EntName = <<"my-ent">>,
    TeamName = <<"some-team">>,
    TeamId = 1,
    MemberName = <<"some-user">>,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {ok, team};
            deliv_team:getval(id, team) ->
                TeamId;
            deliv_user:fetch(EntName, MemberName) ->
                {error, not_found};
            deliv_web_utils:error_response(404, not_found, req1, State) ->
                error
        end),

    ?assertEqual(error, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_when_team_fetch_fails_logs_and_returns_server_error() ->
    EntName = <<"my-ent">>,
    TeamName = <<"my-team">>,
    MemberName = <<"some-user">>,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {error, something_bad};
            chef_log:error("Failed to add member ~s to team ~s: ~p", [MemberName, TeamName, something_bad]) ->
                ok;
            deliv_web_utils:error_response(500, server_error, req1, State) ->
                error
        end),

    ?assertEqual(error, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_when_user_fetch_fails_logs_and_returns_server_error() ->
    EntName = <<"my-ent">>,
    TeamName = <<"my-team">>,
    TeamId = 1,
    MemberName = <<"some-user">>,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {ok, team};
            deliv_team:getval(id, team) ->
                TeamId;
            deliv_user:fetch(EntName, MemberName) ->
                {error, something_bad};
            chef_log:error("Failed to add member ~s to team ~s: ~p", [MemberName, TeamName, something_bad]) ->
                ok;
            deliv_web_utils:error_response(500, server_error, req1, State) ->
                error
        end),

    ?assertEqual(error, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.

from_json_when_add_member_fails_logs_and_returns_server_error() ->
    EntName = <<"my-ent">>,
    TeamName = <<"my-team">>,
    TeamId = 1,
    MemberName = <<"some-user">>,
    MemberId = 1,
    State = #handler{ent_name = EntName},

    hoax:expect(
        receive
            deliv_web_utils:extract_bindings([ent_name, team_name, member_name], req) ->
                {[EntName, TeamName, MemberName], req1};
            deliv_team:fetch(EntName, TeamName) ->
                {ok, team};
            deliv_team:getval(id, team) ->
                TeamId;
            deliv_user:fetch(EntName, MemberName) ->
                {ok, user};
            deliv_user:getval(id, user) ->
                MemberId;
            deliv_team:add_member(TeamId, MemberId) ->
                {error, sigh};
            chef_log:error("Failed to add member ~s to team ~s: ~p", [MemberName, TeamName, sigh]) ->
                ok;
            deliv_web_utils:error_response(500, server_error, req1, State) ->
                error
        end),

    ?assertEqual(error, deliv_hand_teams_members_named:from_json(req, State)),
    ?verifyAll.
