-module(scm_github_project_metadata).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

%% DB operations
-export([
         fetch_all_for_ent_name_url/2,
         fetch_by_id/1,
         update_by_id/3
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

%% deliv_project_metadata exports
-export([]).

-compile({parse_transform, sqerl_gobot}).

-record(?MODULE, {project_id,
                  repo_owner,
                  repo_name,
                  token}).

'#insert_fields'() -> [project_id, repo_owner, repo_name].

'#update_fields'() -> [repo_owner, repo_name].

'#statements'() ->
    [default,
    {fetch_all_for_ent_name_url,
     <<"SELECT *
          FROM project_github_metadata AS m
          JOIN projects AS p
            ON m.project_id = p.id
          JOIN organizations AS o
            ON p.organization_id = o.id
          JOIN enterprises AS e
            ON o.enterprise_id = e.id
          JOIN external_basic_auth_applications AS ebaa
            ON ebaa.ent_id = e.id
         WHERE e.name = $1
           AND ebaa.root_api_url = $2
           AND p.scm_module = 'github_scm'">>},
      {fetch_by_id, sqerl_rec:gen_fetch(?MODULE, project_id)},
      {update_by_id,
       <<"UPDATE project_github_metadata pgm
            SET repo_owner = $2,
                repo_name = $3
          WHERE pgm.project_id = $1
      RETURNING pgm.*">>}
    ].

'#table_name'() -> "project_github_metadata".

%% fetch only githubV2 projects
-spec fetch_all_for_ent_name_url(binary(), binary()) -> term().
fetch_all_for_ent_name_url(EntName, Url) ->
    case deliv_db:qfetch(scm_github_project_metadata, fetch_all_for_ent_name_url, [EntName, Url]) of
        [] -> {error, not_found};
        {error, Reason} = Error ->
            chef_log:failed_call(?MODULE, fetch_all_for_ent_name_url, [EntName, Url], Reason),
            Error;
        Result -> {ok, Result}
    end.

%% @doc Fetches github project metadata given a ProjectId.
-spec fetch_by_id(non_neg_integer()) -> {ok, d_project_github_metadata()} | {error, not_found} | {error, term()}.
fetch_by_id(ProjectId) ->
    deliv_db:fetch_by_id(?MODULE, ProjectId).

-spec update_by_id(non_neg_integer(), binary(), binary()) -> {ok, d_project_bitbucket_metadata()} | {error, not_found} | {error, term()}.
update_by_id(ProjectId, RepoOwner, RepoName) ->
    case deliv_db:qfetch(?MODULE, update_by_id, [ProjectId, RepoOwner, RepoName]) of
        [BitbucketMetadata] ->
            {ok, BitbucketMetadata};
        [] ->
            {error, not_found};
        {error, Why} = Error->
            chef_log:failed_call(?MODULE, update_by_id, [ProjectId, RepoOwner, RepoName], Why),
            Error
    end.
