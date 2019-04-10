-module(deliv_hand_orgs_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2,
         delete_resource/2,
         delete_completed/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

delete_resource(Req, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    case deliv_organization:delete(EntName, OrgName) of
        ok ->
            {true, Req1, State};
        _Error ->
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.

handle(Req, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    case deliv_organization:fetch(EntName, OrgName) of
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {ok, Org} ->
            {Method, Req2} = cowboy_req:method(Req1),
            handle(Method, Org, EntName, Req2, State)
    end.

handle(<<"GET">>, Org, EntName, Req, State) ->
    Name = deliv_organization:getval(name, Org),
    SelfLink = deliv_web_utils:href(EntName, <<"/orgs/", Name/binary>>),
    Links = [{self, SelfLink},
             {projects, deliv_web_utils:href(EntName, ["/orgs/", Name, "/projects"])}
            ],
    Hal = deliv_web_utils:make_hal(Links),
    ProjNames = deliv_project:fetch_names(EntName, Name),
    NbProjects = erlang:length(ProjNames),
    Body = {[
             {<<"name">>, Name},
             {<<"project_count">>, NbProjects},
             {<<"_links">>, Hal}
            ]},
    deliv_web_utils:content(Body, Req, State);
handle(<<"PUT">>, Org, _EntName, Req, State) ->
    handle_json_input(
        deliv_web_utils:parse_json_req(Req, update_org_jesse_spec()),
        Org, State
    ).

handle_json_input({{error, _Why}, Req}, _Org, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({Json, Req}, Org, State) ->
    NewOrgName = ej:get([<<"name">>], Json),
    case deliv_organization:rename(Org, NewOrgName) of
        {ok, _RenamedOrg} ->
            {true, Req, State};
        {error, conflict} ->
            deliv_web_utils:error_response(409, bad_request, <<"organization already exists">>, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

%% Because of the library the front-end is using (v.0.18.2 of angular-restmod),
%% it cannot adhere to our strict expectation for what the body of the request
%% looks like. As a result, as a temporary workaround, we are loosening our
%% restriction on the schema.
update_org_jesse_spec() ->
    chef_json:simple_string_loose_spec([<<"name">>]).
