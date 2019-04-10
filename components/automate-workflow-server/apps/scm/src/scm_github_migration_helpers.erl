-module(scm_github_migration_helpers).

-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-export([validate_input/3,
         validate_project_scm_module/1,
         validate_no_open_changes/3,
         validate_no_local_git_repo/1,
         validate_github_scm_configured/1,
         validate_automate_has_push_permissions/1,
         list_project_webhooks/1,
         delete_webhooks/2,
         initialize_local_git_repo/1,
         set_scm_module_to_new_github/1,
         list_ghv1_projects/2,
         list_orgs/1
        ]).

-spec validate_input(({error, _Why} | {ok, d_enterprise()}),
                      ({error, _Why} | {ok, d_organization()}),
                      ({error, _Why} | {ok, d_project()})) -> no_return() | d_project().
validate_input({error, WhyEnt}, _, _) ->
    erlang:throw({<<"Failed to lookup enterprise">>, WhyEnt});
validate_input(_, {error, WhyOrg}, _) ->
    erlang:throw({<<"Failed to lookup organization">>, WhyOrg});
validate_input(_, _, {error, WhyProj}) ->
    erlang:throw({<<"Failed to lookup project">>, WhyProj});
validate_input({ok, _}, {ok, _}, {ok, Proj}) ->
    Proj.

-spec validate_project_scm_module(d_project()) -> true | no_return().
validate_project_scm_module(DProj) ->
    case deliv_project:getval(scm_module, DProj) of
        <<"deliv_scm_github">> -> true;
        Else -> erlang:throw({<<"The specified project is not integrated with GitHub">>, Else})
    end.

validate_no_open_changes(EntName, OrgName, ProjName) ->
    case deliv_change:changes(EntName, OrgName, ProjName, [{state, open}]) of
        {ok, []} -> true;
        {ok, _Changes} -> erlang:throw(<<"There are open changes associated with the project.\nPlease merge or delete the open changes and try again">>);
        {error, Why} -> erlang:throw(Why)
    end.

validate_github_scm_configured(EntName) ->
    case scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>) of
        {ok, _Auth} -> true;
        {error, not_found} -> erlang:throw(<<"SCM link has not been set up for the new version of GitHub integration">>);
        {error, Why} -> erlang:throw(Why)
    end.

-spec validate_automate_has_push_permissions(#proj_coordinates{}) -> boolean().
validate_automate_has_push_permissions(Coords) ->
    Scope = deliv_scopes:from_coords(Coords),
    case deliv_github_client:req(Scope, get, []) of
        {ok, 200, _Head, Body} ->
            Ejson = chef_json:decode(Body),
            case ej:get({<<"permissions">>, <<"push">>}, Ejson) of
                true -> true;
                false -> erlang:throw(<<"Please ensure that automate service account in GitHub has write permissions to repositories">>)
            end;
        _ -> erlang:throw(<<"Could not read repository info from GitHub">>)
     end.

-spec list_project_webhooks(#proj_coordinates{}) -> list() | no_return().
list_project_webhooks(Coords) ->
    Scope = deliv_scopes:from_coords(Coords),
    case deliv_github_client:req(Scope, get, ["/hooks"]) of
        {ok, 200, _Head, ResponseBody} ->
            DelivWebhookUrl = github_repo:webhook_callback_url(Coords),
            Webhooks = chef_json:decode(ResponseBody),
            lists:filtermap(
                    fun(Webhook) ->
                        case string:equal(chef_utils:to_str(ej:get({<<"config">>, <<"url">>}, Webhook)), chef_utils:to_str(DelivWebhookUrl)) of
                            true -> {true, ej:get({<<"id">>}, Webhook)};
                            _ -> false
                        end
                    end, Webhooks);
        {ok, _, _Head, ResponseBody} -> erlang:throw(ResponseBody);
        {error, Why} -> erlang:throw(Why)
    end.

delete_webhooks(_Coords, []) ->
    ok;
delete_webhooks(Coords, [Id | Rest]) ->
    Route = ["/hooks/", integer_to_list(Id)],
    Scope = deliv_scopes:from_coords(Coords),
    %% We will never get here unless the webhook exists ref list_project_webhooks
    %% We will never get here unless the user has write(push) permissions
    {ok, 204, _Header, _Body} = deliv_github_client:req(Scope, delete, Route),
    delete_webhooks(Coords, Rest).

initialize_local_git_repo(#proj_coordinates{ent_name = EntName,
                                            org_name = OrgName,
                                            proj_name = ProjName}=Coords) ->
    %% Since we ask the user to configure GHV2 before running the script,
    %% We will get the same auth info from either oauth or basicauth.
    {ok, GitUrl} = scm_repo:url(Coords),
    TemplateDir = delivery_app:get_env(deliv_git_repo_template),
    GitCmd = ["clone", "--bare",
              "--template=" ++ TemplateDir,
              GitUrl, "."],
    TrustedCertsFile = delivery_app:get_env(trusted_certificates_file),
    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    case deliv_git:run_git(clone_github, RepoPath, GitCmd, [{"GIT_SSL_CAINFO", TrustedCertsFile}]) of
        {ok, _} -> ok;
        {error, Why} -> erlang:throw(Why)
    end.

-spec set_scm_module_to_new_github(d_project()) -> ok.
set_scm_module_to_new_github(Project) ->
    GHV2Project = deliv_project:setvals([{scm_module, <<"github_scm">>}], Project),
    case deliv_project:update(GHV2Project) of
        {ok, _DProj} -> ok;
        {error, Why} -> erlang:throw({<<"Could not update scm_module to github_scm">>, Why})
    end.

list_ghv1_projects(EntName, OrgName) ->
    Projects = deliv_project:projects(EntName, OrgName),
    lists:filtermap(fun(Project) ->
            case deliv_project:getval(scm_module, Project) of
                <<"deliv_scm_github">> -> {true, deliv_project:getval(name, Project)};
                _ -> false
            end
        end, Projects).

list_orgs(EntName) ->
    %The query returns a list of ej objects with (org)name and project_count arrtibutes
    %[{[{<<"name">>,<<"taco911">>},{<<"project_count">>,1}]},
    %  {[{<<"name">>,<<"ventech">>},{<<"project_count">>,4}]}]
    %List comprehension is the easiest way to get all (org) names.
    case deliv_organization:fetch_for_ent(EntName) of
        {error, Why} -> erlang:throw({<<"Failed to lookup organization for Enterprise">>, Why});
        EjOrgs -> [Name || {[{<<"name">>, Name} | _]} <- EjOrgs]
    end.

validate_no_local_git_repo(#proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}) ->
    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    case filelib:is_dir(RepoPath) of
        false -> ok;
        true ->
            Message = <<"The local git repository path already exists. ",
                        "This could be due to an error in a previous migration attempt. ",
                        "The directory ", RepoPath/binary, " must be removed before rerunning the migration.">>,
            erlang:throw(Message)
    end.
