-module(deliv_hand_teams_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    {[EntName, TeamName], Req1} = deliv_web_utils:extract_bindings([ent_name, team_name], Req),

    case deliv_team:fetch(EntName, TeamName) of
        {ok, Team} ->
            UpdatedAt = chef_utils:format_timestamp(deliv_team:getval(updated_at, Team)),
            TeamEjson = {[
                          {<<"name">>, deliv_team:getval(name, Team)},
                          {<<"description">>, deliv_team:getval(description, Team)},
                          {<<"creator_name">>, user_id_helper(creator_id, Team)},
                          {<<"updater_name">>, user_id_helper(updater_id, Team)},
                          {<<"updated_at">>, UpdatedAt}
                         ]},
            deliv_web_utils:content(TeamEjson, Req1, State);
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {error, Why} ->
            chef_log:error("Fetch failed for team ~s: ~p", [TeamName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
        end.

user_id_helper(Field, Team) ->
    UserId = deliv_team:getval(Field, Team),
    {ok, User} = deliv_user:fetch(UserId),
    deliv_user:getval(name, User).
