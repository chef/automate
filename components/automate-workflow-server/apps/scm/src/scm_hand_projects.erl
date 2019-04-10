%% @doc cowboy handler for the bitbucket-projects endpoint. This endpoint is
%% for internal use only at the momemnt, and may change without notice.
-module(scm_hand_projects).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         from_json/2,
         resource_exists/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, #handler{} = State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, #handler{} = State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req1} ->
            {false, Req1, State}
    end.

%% @doc will create a new $SCM-TYPE-backed project with the provided params it
%% returns a precondition_failed error if global bitbucket configuration is not
%% setup.
from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, bitbucket_project), State).

handle_parse({{error, Why}, Req}, State) ->
    chef_log:error("~p", [Why]),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, #handler{ent_name = EntName} = State) ->
    SCMType = ej:get([<<"scm">>, <<"type">>], Ejson),
    handle_load_basic_auth_creds(scm_basic_auth:load_basic_auth_credentials(EntName, SCMType),
                                 {Ejson, Req}, State).

handle_load_basic_auth_creds({ok, BasicAuth}, {Ejson, Req}, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    WFProjName = ej:get([<<"name">>], Ejson),

    case deliv_basic_auth_application:getval(name, BasicAuth) of
        <<"bitbucket">> ->
            SCMTopLevel = ej:get([<<"scm">>, <<"project_key">>], Ejson),
            SCMProj = ej:get([<<"scm">>, <<"repo_name">>], Ejson),
            PipeName = ej:get([<<"scm">>, <<"pipeline_branch">>], Ejson),
            Type = bitbucket_scm;
        <<"github">> ->
            SCMTopLevel = ej:get([<<"scm">>, <<"organization">>], Ejson),
            SCMProj = ej:get([<<"scm">>, <<"project">>], Ejson),
            PipeName = ej:get([<<"scm">>, <<"branch">>], Ejson),
            Type = github_scm
    end,

    case deliv_project:new(EntName, OrgName, WFProjName, PipeName,
                           Type, SCMTopLevel, SCMProj) of
        {ok, _Proj} ->
            {true, Req1, State};
        {error, {conflict, _Why}} ->
            deliv_web_utils:error_response(409, conflict, "The repository name you entered has already been linked to another project in Chef Automate.", Req1, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req1, State)
    end;
handle_load_basic_auth_creds({error, _}, {_Ejson, Req}, State) ->
    deliv_web_utils:error_response(412, missing_bitbucket_configuration, Req, State).
