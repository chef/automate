-module(scm_bitbucket_project_metadata).
-behaviour(deliv_project_metadata).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include("scm_types.hrl").

%% DB operations
-export([
         fetch_all_for_ent_name_url/2,
         fetch_by_project_coords/1,
         fetch_by_id/1,
         update_by_id/3,
         %% this function fetches by metadata type for all external scm.
         fetch_scm_metadata_by_coords/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(?MODULE, {project_id,
                  bitbucket_project,
                  repo_name}).

'#insert_fields'() -> [project_id, bitbucket_project, repo_name].

'#update_fields'() -> [bitbucket_project, repo_name].

'#statements'() ->
    [default,
     {fetch_by_id, sqerl_rec:gen_fetch(?MODULE, project_id)},
     {fetch_all_for_ent_name_url,
      <<"SELECT *
           FROM project_bitbucket_metadata AS m
           JOIN projects AS p
             ON m.project_id = p.id
           JOIN organizations AS o
             ON p.organization_id = o.id
           JOIN enterprises AS e
             ON o.enterprise_id = e.id
           JOIN external_basic_auth_applications AS ebaa
             ON ebaa.ent_id = e.id
          WHERE e.name = $1
            AND ebaa.root_api_url = $2">>},
     {fetch_by_project_coords,
      <<"SELECT m.*
           FROM project_bitbucket_metadata AS m
           JOIN projects AS p
             ON m.project_id = p.id
           JOIN organizations AS o
             ON p.organization_id = o.id
           JOIN enterprises AS e
             ON o.enterprise_id = e.id
          WHERE e.name = $1
            AND o.name = $2
            AND p.name = $3">>},
     {fetch_scm_metadata_by_project,
        <<"SELECT m.scm_type, m.repo_group, m.repo_name
           FROM enterprises e
           JOIN organizations o
           ON o.enterprise_id = e.id
           JOIN projects p
           ON p.organization_id = o.id
           JOIN
              (SELECT project_id,
                      'github' AS scm_type,
                      repo_owner AS repo_group,
                      repo_name
              FROM project_github_metadata
              UNION ALL
              SELECT project_id,
                     'bitbucket' AS scm_type,
                     bitbucket_project AS repo_group,
                     repo_name FROM project_bitbucket_metadata
              ) AS m
            ON m.project_id = p.id
            WHERE e.name = $1
            AND o.name = $2
            AND p.name = $3">>},
     {update_by_id,
      <<"UPDATE project_bitbucket_metadata bpm
            SET bitbucket_project = $2,
                repo_name = $3
          WHERE bpm.project_id = $1
      RETURNING bpm.*">>}
    ].

'#table_name'() -> "project_bitbucket_metadata".

%% @doc Fetches bitbucket project metadata given a ProjectId.
-spec fetch_by_id(non_neg_integer()) -> {ok, d_project_bitbucket_metadata()} | {error, not_found} | {error, term()}.
fetch_by_id(ProjectId) ->
    deliv_db:fetch_by_id(?MODULE, ProjectId).

%% @doc Fetch all the Bitbucket project metadatas
-spec fetch_all_for_ent_name_url(binary(), binary()) -> {ok, [d_project_bitbucket_metadata()]} | {error, term()}.
fetch_all_for_ent_name_url(EntName, Url) ->
    case deliv_db:qfetch(scm_bitbucket_project_metadata, fetch_all_for_ent_name_url, [EntName, Url]) of
        [_ | _] = Results ->
            {ok, Results};
        [] ->
            {error, not_found};
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, fetch_all_for_ent_name_url, [EntName, Url], Why),
            Error
    end.

%% @doc Fetch the Bitbucket project metadata by `proj_coordinates`
-spec fetch_by_project_coords(#proj_coordinates{}) -> {error, not_found} | {ok, d_project_bitbucket_metadata()}.
fetch_by_project_coords(Coords = #proj_coordinates{
                                    ent_name = EntName,
                                    org_name = OrgName,
                                    proj_name = ProjName}) ->
    case deliv_db:qfetch(scm_bitbucket_project_metadata, fetch_by_project_coords, [EntName, OrgName, ProjName]) of
        [Result] ->
            {ok, Result};
        [] ->
            {error, not_found};
        {error, Why} ->
            chef_log:failed_call(?MODULE, fetch_by_project_coords, [Coords], Why),
            {error, not_found}
    end.

-spec update_by_id(non_neg_integer(), binary(), binary()) -> {ok, d_project_bitbucket_metadata()} | {error, not_found} | {error, term()}.
update_by_id(ProjectId, ProjectKey, RepoName) ->
    case deliv_db:qfetch(scm_bitbucket_project_metadata, update_by_id, [ProjectId, ProjectKey, RepoName]) of
        [BitbucketMetadata] ->
            {ok, BitbucketMetadata};
        [] ->
            {error, not_found};
        {error, Why} = Error->
            chef_log:failed_call(?MODULE, update_by_id, [ProjectId, ProjectKey, RepoName], Why),
            Error
    end.

-spec fetch_scm_metadata_by_coords(#proj_coordinates{}) -> {ok, #metadata_by_scm{}} | {error, term()}.
fetch_scm_metadata_by_coords(#proj_coordinates{
                                    ent_name = EntName,
                                    org_name = OrgName,
                                    proj_name = ProjName} = Coords) ->
    case deliv_db:select(?MODULE, fetch_scm_metadata_by_project, [EntName, OrgName, ProjName], rows_as_records, [metadata_by_scm, record_info(fields, metadata_by_scm)]) of
        {ok, [Result]} -> {ok, Result};
        {ok, []} ->
            chef_log:failed_call(?MODULE, fetch_scm_metadata_by_coords, [Coords], not_found),
            {error, not_found};
        {ok, Result} when is_list(Result) ->
            chef_log:failed_call(?MODULE, fetch_scm_metadata_by_coords, [Coords], multiple_scm_configs),
            {error, multiple_scm_configs};
        Error -> Error
    end.
