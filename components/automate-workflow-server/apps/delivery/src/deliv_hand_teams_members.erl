-module(deliv_hand_teams_members).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
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

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

-spec to_json(cowboy_req(), req_handler()) -> {binary(), cowboy_req(), req_handler()}.
to_json(Req, State) ->
    {[EntName, TeamName], Req1} = deliv_web_utils:extract_bindings([ent_name, team_name], Req),

    case deliv_team:fetch_members(EntName, TeamName) of
        {error, Why} ->
            chef_log:error("Fetch failed for members of team ~s: ~p", [TeamName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State);
        TeamEjson ->
            Members = {[{<<"members">>, TeamEjson}]},
            deliv_web_utils:content(Members, Req1, State)
    end.

