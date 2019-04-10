-module(deliv_hand_projects_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         delete_resource/2,
         delete_completed/2,
         resource_exists/2,
         from_json/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

-record(project_named, {
          project :: d_project(),
          ent_name :: binary(),
          org_name :: binary(),
          user_name :: binary(),
          state :: req_handler()
          }).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, #handler{ent_name = EntName, user_name = UserName} = State) ->
    {[OrgName, ProjName], Req1} = deliv_web_utils:extract_bindings([org_name, proj_name], Req),
    case deliv_project:fetch(EntName, OrgName, ProjName) of
        {ok, Project} -> {true, Req1, #project_named{project = Project,
                                                    ent_name = EntName,
                                                    org_name = OrgName,
                                                    user_name = UserName,
                                                    state = State}};
        {error, _} -> {false, Req1, State}
    end.

delete_resource(Req, #project_named{project = Project, state = State}) ->
    case deliv_project:delete(Project) of
        ok  ->
            {true, Req, State};
        {error, _Reason} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.

to_json(Req, #project_named{project = Proj,
               ent_name = EntName,
               org_name = OrgName,
               user_name = UserName,
               state = State}) ->
        ProjId = deliv_project:getval(id, Proj),
        ProjName = deliv_project:getval(name, Proj),
        ScmType = deliv_project:scm_type(Proj),
        MetadataModule = deliv_project:metadata_module(Proj),
        PipelineNames = deliv_pipeline:fetch_names(EntName, OrgName,
                                                   ProjName),

        Body = build_body(MetadataModule, EntName, OrgName, ProjName, ProjId,
                          ScmType, UserName, PipelineNames),
        deliv_web_utils:content(Body, Req, State).

from_json(Req, #project_named{project = Proj,
               ent_name = EntName,
               org_name = OrgName,
               state = State}) ->
    case deliv_web_utils:parse_json_req(Req) of
        {{error, _Why}, Req1} ->
            deliv_web_utils:error_response(400, malformed_json, <<"Project update failed.">>, Req1, State);
        {Json, Req1} ->
            update_project(check_if_supported_update(deliv_project:getval(scm_module, Proj),
                                                     ej:get([<<"scm">>, <<"type">>], Json)),
                           EntName, OrgName, Proj, Req1, State, Json)
    end.

update_project({ok, edit_local_project}, _EntName, _OrgName, _Proj, Req, State, _Json) ->
    % if someone hits save on local when their scm module is local, nothing
    % needs to be done.
    {true, Req, State};
update_project({ok, local_to_bitbucket}, EntName, OrgName, Proj, Req, State, Json) ->
    ProjName = deliv_project:getval(name, Proj),
    case deliv_change:changes(EntName, OrgName, ProjName, [{state, open}]) of
        {ok, []} ->
            handle_convert_project(deliv_project:convert_to_bb(Json, Proj), Req, State);
        {ok, [_ | _ ]} ->
            deliv_web_utils:error_response(412, precondition_failed, <<"Open changes exist in the system. Please close them and try again.">>, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State)
    end;
update_project({ok, bitbucket_to_local}, _EntName, _OrgName, Proj, Req, State, _Json) ->
    case deliv_project:convert_to_local(Proj) of
      {error, Why} ->
          chef_log:error("Could not convert project ~p from Bitbucket scm to local because ~p", [deliv_project:getval(name, Proj), Why]),
          deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State);
      _Project ->
          {true, Req, State}
    end;
update_project({ok, githubV2_to_local}, _EntName, _OrgName, Proj, Req, State, _Json) ->
    case deliv_project:convert_ghv2_to_local(Proj) of
      {error, Why} ->
          chef_log:error("Could not convert project ~p from GitHub scm to local because ~p", [deliv_project:getval(name, Proj), Why]),
          deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State);
      _Project -> {true, Req, State}
    end;
update_project({ok, edit_bitbucket_project}, EntName, OrgName, Proj, Req, State, Json) ->
    ProjId = deliv_project:getval(id, Proj),
    ProjName = deliv_project:getval(name, Proj),
    MetadataModule = deliv_project:metadata_module(Proj),
    NewProjKey = ej:get({"scm", "project_key"}, Json),
    NewRepoName = ej:get({"scm", "repo_name"}, Json),

    case MetadataModule:update_by_id(ProjId, NewProjKey, NewRepoName) of
        {ok, _UpdatedMetadata} ->
            {true, Req, State};
        {error, not_found} ->
            chef_log:error("Could not update bitbucket project ~s/~s/~s because the project metadata was not found", [EntName, OrgName, ProjName]),
            deliv_web_utils:error_response(404, not_found, <<"Project update failed.">>, Req, State);
        {error, {conflict, Why}} ->
            chef_log:error("Could not update bitbucket project ~s/~s/~s because the bb project is already linked: ~p", [EntName, OrgName, ProjName, Why]),
            deliv_web_utils:error_response(409, conflict, <<"This Bitbucket Project and Repo combination is already in use">>, Req, State);
        {error, Why} ->
            chef_log:error("Could not update bitbucket project ~s/~s/~s because of a db error: ~p", [EntName, OrgName, ProjName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State)
    end;
update_project({ok, edit_githubV2_project}, EntName, OrgName, Proj, Req, State, Json) ->
    ProjId = deliv_project:getval(id, Proj),
    ProjName = deliv_project:getval(name, Proj),
    MetadataModule = deliv_project:metadata_module(Proj),
    NewRepoOwner = ej:get({"scm", "repo_owner"}, Json),
    NewRepoName = ej:get({"scm", "repo_name"}, Json),

    case MetadataModule:update_by_id(ProjId, NewRepoOwner, NewRepoName) of
        {ok, _UpdatedMetadata} ->
            {true, Req, State};
        {error, not_found} ->
            chef_log:error("Could not update GitHub project ~s/~s/~s because the project metadata was not found", [EntName, OrgName, ProjName]),
            deliv_web_utils:error_response(404, not_found, <<"Project update failed.">>, Req, State);
        {error, {conflict, Why}} ->
            chef_log:error("Could not update GitHub project ~s/~s/~s because this project is already linked: ~p", [EntName, OrgName, ProjName, Why]),
            deliv_web_utils:error_response(409, conflict, <<"This GitHub Owner and Repo combination is already in use">>, Req, State);
        {error, Why} ->
            chef_log:error("Could not update GitHub project ~s/~s/~s because of a db error: ~p", [EntName, OrgName, ProjName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State)
    end;
update_project({ok, local_to_githubV2}, EntName, OrgName, Proj, Req, State, Json) ->
    ProjName = deliv_project:getval(name, Proj),
    case deliv_change:changes(EntName, OrgName, ProjName, [{state, open}]) of
        {ok, []} ->
            GitHubOwner = ej:get([<<"scm">>, <<"repo_owner">>], Json),
            GitHubRepo = ej:get([<<"scm">>, <<"repo_name">>], Json),
            handle_convert_project(deliv_project:convert_to_githubV2(GitHubOwner, GitHubRepo, Proj), Req, State);
        {ok, [_ | _ ]} ->
            deliv_web_utils:error_response(412, precondition_failed, <<"Open changes exist in the system. Please close them and try again.">>, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State)
    end;
update_project({error, not_supported}, _EntName, _OrgName, _Proj, Req, State, _Json) ->
    deliv_web_utils:error_response(501, not_implemented, <<"This is not yet supported.">>, Req, State);
update_project({error, no_type_to}, _EntName, _OrgName, _Proj, Req, State, _Json) ->
    deliv_web_utils:error_response(400, malformed_json, <<"Project update failed.">>, Req, State).

handle_convert_project({error, {conflict, Why}}, Req, State) ->
    chef_log:error("Could not convert project scm to bitbucket: ~p", [Why]),
    deliv_web_utils:error_response(409, conflict, <<"This Bitbucket Project and Repo combination is already in use">>, Req, State);
handle_convert_project({error, Why}, Req, State) ->
    chef_log:error("Could not convert project scm to bitbucket: ~p", [Why]),
    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, Req, State);
handle_convert_project(_Proj, Req, State) ->
    {true, Req, State}.

build_body(none, EntName, OrgName, ProjName, _ProjId, ScmType, UserName, PipelineNames) ->
    deliv_project_json:create_body(EntName, OrgName, ProjName,
                                   ScmType, UserName, PipelineNames);
build_body(MetadataModule, EntName, OrgName, ProjName, ProjId, ScmType, UserName, PipelineNames) ->

    {ok, MetaData} = MetadataModule:fetch_by_id(ProjId),

    {ScmUrl, _} = deliv_hand_projects:fetch_scm_url(EntName, ScmType, []),
    deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType,
                                   MetaData, UserName, PipelineNames, ScmUrl).

check_if_supported_update(<<"deliv_scm_local">>, <<"local">>) ->
    {ok, edit_local_project};
check_if_supported_update(<<"deliv_scm_local">>, <<"bitbucket">>) ->
    {ok, local_to_bitbucket};
check_if_supported_update(<<"bitbucket_scm">>, <<"local">>) ->
    {ok, bitbucket_to_local};
check_if_supported_update(<<"bitbucket_scm">>, <<"bitbucket">>) ->
    {ok, edit_bitbucket_project};
check_if_supported_update(<<"deliv_scm_local">>, <<"github">>) ->
    {ok, local_to_githubV2};
check_if_supported_update(<<"github_scm">>, <<"github">>) ->
    {ok, edit_githubV2_project};
check_if_supported_update(<<"github_scm">>, <<"local">>) ->
    {ok, githubV2_to_local};
check_if_supported_update(_ScmTypeOriginal, undefined) ->
    {error, no_type_to};
check_if_supported_update(_ScmTypeOriginal, _ScmTypeTo) ->
    {error, not_supported}.
