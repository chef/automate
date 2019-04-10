-module(deliv_team_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, "fetch_"),
     hoax:fixture(?MODULE, "add_member_")
    ].

fetch_forwards_db_error() ->
    EntName = <<"cd">>,
    TeamName = <<"Wolverines">>,
    ExpectedError = {error, nooooooo},

    hoax:expect(
        receive
            deliv_db:fetch(deliv_team, [EntName], TeamName) ->
                ExpectedError
        end),

    Result = deliv_team:fetch(EntName, TeamName),
    ?assertEqual(ExpectedError, Result),
    ?verifyAll.

fetch_members_forwards_db_error() ->
    EntName = <<"cd">>,
    TeamName = <<"some-humans">>,
    ExpectedError = {error, whyyy},

    hoax:expect(
        receive
            deliv_db:query_to_json(deliv_team, fetch_members, [EntName, TeamName]) ->
                ExpectedError
        end),

    Result = deliv_team:fetch_members(EntName, TeamName),
    ?assertEqual(ExpectedError, Result),
    ?verifyAll.

add_member_forwards_db_error() ->
    TeamId = 1,
    MemberId = 1,
    ExpectedError = {error, nooooooo},

    hoax:expect(
        receive
            sqerl_rec:cquery(deliv_team, add_member_to_team, [TeamId, MemberId]) ->
                ExpectedError
        end),

    Result = deliv_team:add_member(TeamId, MemberId),
    ?assertEqual(ExpectedError, Result),
    ?verifyAll.
