%% Copyright 2016 Opscode, Inc. All Rights Reserved.

-module(deliv_team_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

-define(CreationTimeE, {{2015,2,12},{21,26,48.0}}).
-define(CreationTimeS, <<"2015-02-12 21:26:48">>).
-define(TeamDesc, <<"A nice, grammatically correct description.">>).

fixture_test_() ->
    [
     hoax:parameterized_fixture(?MODULE, fetch, setup, teardown),
     hoax:parameterized_fixture(?MODULE, insert, setup, teardown),
     hoax:parameterized_fixture(?MODULE, add_member, setup, teardown)
    ].

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"some_enterprise">>,
                            fun(Enterprise) ->
                                    {Enterprise}
                            end).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

fetch_an_empty_team_list({Enterprise}) ->
    EnterpriseName = deliv_enterprise:getval(name, Enterprise),
    TeamNames = [],
    ?assertEqual(TeamNames, deliv_team:fetch_names(EnterpriseName)).

insert_a_team({Enterprise}) ->
    EnterpriseName = deliv_enterprise:getval(name, Enterprise),

    TeamName = <<"DumbTeam">>,
    CreatorId = get_creator_id(Enterprise),
    TimeOut = {{2015,2,12},{21,26,48.0}},

    TeamRecord =  make_team_proplist(Enterprise,TeamName),

    InsertResult = deliv_team:insert(EnterpriseName, TeamRecord),

    ?assertMatch([{deliv_team, _, _, TeamName, ?TeamDesc, CreatorId, CreatorId, TimeOut}],
                 InsertResult ).

insert_a_team_list_all_teams({Enterprise}) ->
    EnterpriseName = deliv_enterprise:getval(name, Enterprise),

    TeamName = <<"DumbTeam">>,
    CreatorId = get_creator_id(Enterprise),
    TimeOut = {{2015,2,12},{21,26,48.0}},

    TeamRecord = make_team_proplist(Enterprise, TeamName),

    ?assertMatch([{deliv_team, _, _, TeamName, ?TeamDesc, CreatorId, CreatorId, TimeOut}],
                 deliv_team:insert(EnterpriseName, TeamRecord) ),

    ?assertEqual([TeamName], deliv_team:fetch_names(EnterpriseName)).

insert_and_fetch_a_team({Enterprise}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    TeamName = <<"A Team">>,

    TeamRecord = make_team_proplist(Enterprise, TeamName),
    UserId = get_creator_id(Enterprise),

    deliv_team:insert(EntName, TeamRecord),
    {ok, Team} = deliv_team:fetch(EntName, TeamName),

    ?assertEqual(TeamName, deliv_team:getval(name, Team)),
    ?assertEqual(?TeamDesc, deliv_team:getval(description, Team)),
    ?assertEqual(UserId,  deliv_team:getval(creator_id, Team)),
    ?assertEqual(?CreationTimeE, deliv_team:getval(updated_at, Team)),
    ?assertEqual(UserId,  deliv_team:getval(updater_id, Team)).

insert_with_existing_team_name({Enterprise}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    TeamName = <<"A Team">>,
    CreatorId = get_creator_id(Enterprise),
    TimeOut = {{2015,2,12},{21,26,48.0}},

    TeamRecord = make_team_proplist(Enterprise, TeamName),

    [{deliv_team, _, _, TeamName, ?TeamDesc, CreatorId, CreatorId, TimeOut}] =
        deliv_team:insert(EntName, TeamRecord),
    ?assertMatch({error, _}, deliv_team:insert(EntName, TeamRecord)).

add_member({Enterprise}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    TeamName = <<"A Team">>,
    MemberId = get_creator_id(Enterprise),
    TeamRecord = make_team_proplist(Enterprise, TeamName),

    deliv_team:insert(EntName, TeamRecord),
    {ok, Team} = deliv_team:fetch(EntName, TeamName),
    TeamId = deliv_team:getval(id, Team),

    ?assertMatch({ok, _}, deliv_team:add_member(TeamId, MemberId)).

add_member_when_member_exists({Enterprise}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    TeamName = <<"A Team">>,
    MemberId = get_creator_id(Enterprise),
    TeamRecord = make_team_proplist(Enterprise, TeamName),

    deliv_team:insert(EntName, TeamRecord),
    {ok, Team} = deliv_team:fetch(EntName, TeamName),
    TeamId = deliv_team:getval(id, Team),

    {ok, _} = deliv_team:add_member(TeamId, MemberId),
    ?assertMatch({error, {conflict, _}}, deliv_team:add_member(TeamId, MemberId)).

make_team_proplist(Enterprise, TeamName) ->
    Creator = eu_data:fetch_or_create_user(Enterprise, <<"drventure">>),
    CreatorId = deliv_user:getval(id, Creator),
    [{name, TeamName},
     {description, ?TeamDesc},
     {creator_id, CreatorId},
     {updater_id, CreatorId},
     {updated_at, <<?CreationTimeS/binary, "+00">>}].

get_creator_id(Enterprise) ->
    Creator = eu_data:fetch_or_create_user(Enterprise, <<"drventure">>),
    deliv_user:getval(id, Creator).
