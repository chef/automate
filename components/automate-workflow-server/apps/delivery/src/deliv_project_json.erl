-module(deliv_project_json).

-include("deliv_types.hrl").

-export([
         create_body/6,
         create_body/8
        ]).

-spec create_body(binary(), binary(), binary(), binary(), binary(), [binary()]) -> json().
create_body(EntName, OrgName, ProjName, <<"local">>, UserName, PipelineNames) ->
    GitUrl = deliv_git:uri(UserName, EntName, OrgName, ProjName),
    PipelinesList = create_pipelines_links_list(EntName, OrgName,
                                                                                                                                                                                                ProjName,
                                                                                                                                                                                                PipelineNames),
    Hal = create_hal(EntName, OrgName, ProjName, PipelinesList),

    Ejson = {[
      {<<"name">>, ProjName},
      {<<"ent_name">>, EntName},
      {<<"org_name">>, OrgName},
      {<<"scm">>, {[
                    {<<"type">>, <<"local">>}
                   ]}},
      {<<"git_url">>, GitUrl},
      {<<"_links">>, Hal}
     ]},

     Ejson.

-spec create_body(binary(), binary(), binary(), binary(), tuple(), binary(), [binary()], binary()) -> json().
create_body(EntName, OrgName, ProjName, ScmType, Metadata, UserName, PipelineNames, ScmUrl) ->
    PipelinesList = create_pipelines_links_list(EntName, OrgName,
                                                ProjName,
                                                PipelineNames),
    Hal = create_hal(EntName, OrgName, ProjName, PipelinesList),

    Ejson = {[
      {<<"name">>, ProjName},
      {<<"ent_name">>, EntName},
      {<<"org_name">>, OrgName},
      {<<"scm">>, {[
                    {<<"type">>, ScmType}
                   ]}},
      {<<"_links">>, Hal}
     ]},

    Ejson1 = inject_git_url(UserName, EntName, OrgName, ProjName, ScmType, Ejson),

    inject_metadata_json(ScmType, Metadata, ScmUrl, Ejson1).

create_pipelines_links_list(EntName, OrgName, ProjName, PipelineNames) ->
    [
     {
                        [
       {href, deliv_web_utils:href(EntName, ["/orgs/", OrgName,
                                             "/projects/", ProjName,
                                             "/pipelines/", Pipeline])},
       {name, Pipeline}
      ]
                 } || Pipeline <- PipelineNames].

create_hal(EntName, OrgName, ProjName, PipelinesList) ->
    FullLink = deliv_web_utils:href(EntName, <<"/orgs/", OrgName/binary,
                                               "/projects/", ProjName/binary>>),
    PipelinesLink = deliv_web_utils:href(EntName, ["/orgs/", OrgName,
                                                   "/projects/", ProjName,
                                                   "/pipelines"]),
    {HList} = deliv_web_utils:make_hal([
                                                                                                                                                                {full, FullLink},
                                                                                                                                                                {list_pipelines, PipelinesLink}
                                                                                                                                                         ]),
    {HList ++ [{pipelines, PipelinesList}]}.


inject_git_url(UserName, EntName, OrgName, ProjName, ScmType, Ejson) when ScmType == <<"local">>
                                                                   orelse ScmType == <<"bitbucket">>
                                                                   orelse ScmType == <<"githubV2">> ->
    GitUrl = deliv_git:uri(UserName, EntName, OrgName, ProjName),
    ej:set({"git_url"}, Ejson, GitUrl);
inject_git_url(_UserName, _EntName, _OrgName, _ProjName, _ScmType, Ejson) -> Ejson.

inject_metadata_json(<<"github">>, Metadata, _ScmUrl, Ejson) ->
    RepoOwner = deliv_project_github_metadata:getval(repo_owner, Metadata),
    RepoName = deliv_project_github_metadata:getval(repo_name, Metadata),
    Ejson1 = ej:set({"scm", "repo_owner"}, Ejson, RepoOwner),
    ej:set({"scm", "repo_name"}, Ejson1, RepoName);
inject_metadata_json(<<"githubV2">>, Metadata, ScmUrl, Ejson) ->
    RepoOwner = scm_github_project_metadata:getval(repo_owner, Metadata),
    RepoName = scm_github_project_metadata:getval(repo_name, Metadata),
    FullScmUrl = chef_utils:join_binaries([ScmUrl, RepoOwner, RepoName], <<"/">>),
    Ejson1 = ej:set({"scm", "url"}, Ejson, FullScmUrl),
    Ejson2 = ej:set({"scm", "repo_owner"}, Ejson1, RepoOwner),
    ej:set({"scm", "repo_name"}, Ejson2, RepoName);
inject_metadata_json(<<"bitbucket">>, Metadata, ScmUrl, Ejson) ->
    BBProj = scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata),
    RepoName = scm_bitbucket_project_metadata:getval(repo_name, Metadata),
    FullScmUrl = chef_utils:join_binaries([ScmUrl, <<"projects">>, BBProj, <<"repos">>, RepoName], <<"/">>),
    Ejson1 = ej:set({"scm", "url"}, Ejson, FullScmUrl),
    Ejson2 = ej:set({"scm", "project_key"}, Ejson1, BBProj),
    ej:set({"scm", "repo_name"}, Ejson2, RepoName).
