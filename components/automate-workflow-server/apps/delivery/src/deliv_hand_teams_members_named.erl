-module(deliv_hand_teams_members_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    {[EntName, TeamName, MemberName], Req1} = deliv_web_utils:extract_bindings([ent_name, team_name, member_name], Req),
    handle_fetch_team(deliv_team:fetch(EntName, TeamName), EntName, TeamName, MemberName, Req1, State).

handle_fetch_team({ok, Team}, EntName, TeamName, MemberName, Req, State) ->
    TeamId = deliv_team:getval(id, Team),
    handle_user_fetch(deliv_user:fetch(EntName, MemberName), MemberName, TeamName, TeamId, Req, State);
handle_fetch_team({error, not_found}, _EntName, _TeamName, _MemberName, Req, State) ->
    respond_404(Req, State);
handle_fetch_team({error, Why}, _EntName, TeamName, MemberName, Req, State) ->
    respond_500(MemberName, TeamName, Why, Req, State).

handle_user_fetch({ok, Member}, MemberName, TeamName, TeamId, Req, State) ->
    MemberId = deliv_user:getval(id, Member),
    handle_add_member(deliv_team:add_member(TeamId, MemberId), MemberName, TeamName, Req, State);
handle_user_fetch({error, not_found}, _MemberName, _TeamName, _TeamId, Req, State) ->
    respond_404(Req, State);
handle_user_fetch({error, Why}, MemberName, TeamName, _TeamId, Req, State) ->
    respond_500(MemberName, TeamName, Why, Req, State).

handle_add_member({ok, _}, _MemberName, _TeamName, Req, State) ->
    respond_204(Req, State);
handle_add_member({error, {conflict, _}}, _MemberName, _TeamName, Req, State) ->
    respond_204(Req, State);
handle_add_member({error, Why}, MemberName, TeamName, Req, State) ->
    respond_500(MemberName, TeamName, Why, Req, State).

respond_204(Req, State) ->
    {true, Req, State}.

respond_404(Req, State) ->
    deliv_web_utils:error_response(404, not_found, Req, State).

respond_500(MemberName, TeamName, Why, Req, State) ->
    chef_log:error("Failed to add member ~s to team ~s: ~p", [MemberName, TeamName, Why]),
    deliv_web_utils:error_response(500, server_error, Req, State).
