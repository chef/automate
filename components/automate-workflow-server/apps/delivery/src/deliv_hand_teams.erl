-module(deliv_hand_teams).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

-spec to_json(cowboy_req(), req_handler()) -> {binary(), cowboy_req(), req_handler()}.
to_json(Req, #handler{ent_name=EntName}=State) ->
    case deliv_team:fetch_names(EntName) of
        %% This can be an empty list if there are no teams defined yet.
        TeamNames when erlang:is_list(TeamNames) ->
            Body = {[{<<"teams">>, TeamNames},
                     {<<"_links">>, hal(EntName)}]},
            deliv_web_utils:content(Body, Req, State);
        {error, Reason} ->
            chef_log:error("Error fetching team names: ~p", [Reason]),
            deliv_web_utils:error_response(500, internal_server_error,
                                           Req, State)
    end.

hal(EntName) ->
    Links = [{self, deliv_web_utils:href(EntName, "/teams")},
             {team, {t, deliv_web_utils:href(EntName, "/teams/{team_name}")}}],
    deliv_web_utils:make_hal(Links).

-spec from_json(cowboy_req(), #handler{}) -> {atom() | binary(), cowboy_req(), #handler{} }.
from_json(Req, #handler{}=State) ->
    from_json_handle_parse(deliv_web_utils:parse_json_req(Req), State).

from_json_handle_parse({{error, _}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
from_json_handle_parse({Data, Req}, #handler{ent_name = EntName, user_name = CreatorName} = State) ->
    Name = ej:get([<<"name">>], Data),
    Desc = ej:get([<<"description">>], Data),
    {ok, Creator} = deliv_user:fetch(EntName, CreatorName),
    CreatorId = deliv_user:getval(id, Creator),

    Record = [{name, Name}, {description, Desc},
              {creator_id, CreatorId},
              {updater_id, CreatorId},
              {updated_at, <<"now()">>}],
    from_json_handle_insert(deliv_team:insert(EntName, Record), Req, State).

from_json_handle_insert({error, conflict}, Req, State) ->
    deliv_web_utils:error_response(409, conflict, Req, State);
from_json_handle_insert({error, Error}, Req, State) ->
    chef_log:error("Error inserting team: ~p", [Error]),
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
from_json_handle_insert(_Team, Req, State) ->
    {true, Req, State}.
