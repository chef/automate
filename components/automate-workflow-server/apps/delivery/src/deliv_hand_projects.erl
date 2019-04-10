-module(deliv_hand_projects).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         fetch_scm_url/3,
         init/3,
         rest_init/2,
         to_json/2,
         from_json/2,
         resource_exists/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req1} ->
            {false, Req1, State};
        {<<"GET">>, Req1} ->
            {true, Req1, State}
    end.

to_json(Req, #handler{ent_name = EntName, user_name = UserName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    case deliv_project:projects(EntName, OrgName) of
        {error, Why} ->
            chef_log:error("deliv_project:projects failed: ~p", [Why]),
            deliv_web_utils:error_response(500, internal_error, Req1, State);
        Projects ->
            Ejson = build_ejson(Projects, EntName, OrgName, UserName),
            deliv_web_utils:content(Ejson, Req1, State)
    end.

build_ejson(Projects, EntName, OrgName, UserName) ->
    build_ejson(Projects, EntName, OrgName, UserName, [], []).

build_ejson([], _, _, _, _, Ejson) -> Ejson;
build_ejson([Project | Projects], EntName, OrgName, UserName, ScmUrls, Ejson) ->
    ProjId = deliv_project:getval(id, Project),
    ProjName = deliv_project:getval(name, Project),
    ScmType = deliv_project:scm_type(Project),
    MetadataModule = deliv_project:metadata_module(Project),
    {ScmUrl, ScmUrls1} = fetch_scm_url(EntName, ScmType, ScmUrls),
    PipelineNames = deliv_pipeline:fetch_names(EntName, OrgName,
                                               ProjName),

    Body = build_body(MetadataModule, EntName, OrgName, ProjName, ProjId,
                      ScmType, UserName, PipelineNames, ScmUrl),

    build_ejson(Projects, EntName, OrgName, UserName, ScmUrls1, Ejson ++ [Body]).

build_body(none, EntName, OrgName, ProjName, _ProjId, ScmType, UserName, PipelineNames, _ScmUrl) ->
    deliv_project_json:create_body(EntName, OrgName, ProjName,
                                   ScmType, UserName, PipelineNames);
build_body(MetadataModule, EntName, OrgName, ProjName, ProjId, ScmType, UserName, PipelineNames, ScmUrl) ->

    {ok, MetaData} = MetadataModule:fetch_by_id(ProjId),
    deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType,
                                   MetaData, UserName, PipelineNames, ScmUrl).

from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, project), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    ProjName = ej:get([<<"name">>], Ejson),
    create_project(EntName, OrgName, ProjName, Req1, State).

create_project(_, _, <<"">>, Req, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
create_project(EntName, OrgName, ProjName, Req, State) ->
    case deliv_project:new(EntName, OrgName, ProjName) of
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        {ok, _Proj} ->
            FullLink = deliv_web_utils:href(EntName, ["/orgs/", OrgName, "/projects/", ProjName]),
            Links = [
                {<<"full">>, FullLink},
                {<<"branches">>, <<FullLink/binary, "/branches">>}
            ],
            HAL = deliv_web_utils:make_hal(Links),
            BodyEjson = {[{<<"_links">>, HAL}]},
            Req2 = deliv_web_utils:set_json_body(BodyEjson, Req),
            {{true, FullLink}, Req2, State}
    end.

-spec fetch_scm_url(binary(), binary(), list()) -> {undefined | binary(), list()}.
fetch_scm_url(_EntName, <<"local">>, ScmUrls) ->
    {undefined, ScmUrls};
fetch_scm_url(EntName, ScmType, ScmUrls) ->
    case proplists:get_value(ScmType, ScmUrls) of
        undefined ->
            AuthType = scm_type_to_basic_auth_type(ScmType),
            case scm_basic_auth:load_basic_auth_credentials(EntName, AuthType) of
                {ok, ScmConfig} ->
                    ScmUrl = deliv_basic_auth_application:getval(root_api_url, ScmConfig),
                    ScmUrls1 = [{ScmType, ScmUrl} | ScmUrls],
                    {ScmUrl, ScmUrls1};
                {error, not_found} -> {undefined, ScmUrls}
            end;
        ScmUrl ->
            {ScmUrl, ScmUrls}
    end.

scm_type_to_basic_auth_type(<<"githubV2">>) -> <<"github">>;
scm_type_to_basic_auth_type(ScmType) -> ScmType.
