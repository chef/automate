-module(deliv_project).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

%% DB operations
-export([
         delete/1,
         delete/3,
         delete/4,
         insert/3,
         fetch_by_id/1,
         fetch/1,
         fetch/3,
         scoping_names_by_id/1,
         update/1,
         projects/2,
         convert_to_bb/2,
         convert_to_local/1,
         convert_to_githubV2/3,
         convert_ghv2_to_local/1
       ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

%% deliv_sqerl_rec callbacks
-export([
        scoping_parent/0,
        scoping_column_name/0,
        scoping_index_constraint/0
       ]).

%% others
-export([
         changes/5,
         rename/2,
         new/3,
         new/7,
         ensure_repo_hooks/1,
         ensure_repo_hooks/3,
         fetch_names/2,
         repo_path/1,
         repo_path/3,
         to_coords/1,
         working_tree_path/1,
         compose_repo_name/3,
         decompose_repo_name/1,
         scm_type/1,
         metadata_module/1
        ]).

-record(deliv_project, {
          id                :: db_id(),
          organization_id   :: db_id(),
          scm_module        :: binary(),
          guid              :: db_guid(),
          name              :: binary()
         }).

-compile({parse_transform, sqerl_gobot}).

'#insert_fields'() ->
    [organization_id, name].

'#update_fields'() ->
    [organization_id, name, scm_module].

'#statements'() ->
    [default,
     {fetch_by_name, sqerl_rec:gen_fetch(deliv_project, name)},
     {fetch_names, deliv_sqerl_rec:gen_fetch_names_by_scoping_names(?MODULE)},
     {create_scm_project,
      "SELECT * FROM create_scm_project($1, $2, $3, $4, $5, $6, $7)"},
     {create_bitbucket_scm_project,
      "SELECT * FROM create_bitbucket_scm_project($1, $2, $3, $4, $5, $6, $7)"},
     {scoping_names_by_id,
      <<"SELECT e.name AS ent_name,
                o.name AS org_name,
                p.name AS proj_name
           FROM projects AS p
           JOIN organizations AS o
             ON p.organization_id = o.id
           JOIN enterprises AS e
             ON o.enterprise_id = e.id
          WHERE p.id = $1">>},
     {projects,
      <<"SELECT * FROM projects WHERE organization_id = $1">>},
     {convert_to_bb,  "SELECT * FROM convert_to_bb($1, $2, $3)"},
     {convert_to_local, "SELECT * FROM convert_to_local($1)"},
     {convert_ghv2_to_local, "SELECT * FROM convert_ghv2_to_local($1)"},
     {convert_to_githubV2, <<"SELECT * FROM convert_to_githubV2($1, $2, $3)">>}
     | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

'#table_name'() ->
    "projects".

-spec fetch_names(binary(), binary()) -> db_op_result(binary()).
fetch_names(EntName, OrgName) ->
    deliv_db:fetch_names(?MODULE, [EntName, OrgName]).

-spec projects(binary(), binary()) -> list() | {error, _}.
projects(EntName, OrgName) ->
    {ok, Org} = deliv_organization:fetch(EntName, OrgName),
    OrgId = deliv_organization:getval(id, Org),
    deliv_db:qfetch(?MODULE, projects, [OrgId]).

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_organization, organization_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec update(d_project()) -> {error, conflict | any()} | {ok, d_project()}.
update(Proj) ->
    deliv_db:update(Proj).

-spec insert(binary(), binary(), binary()) -> db_op_result(d_project()).
insert(EntName, OrgName, ProjName) ->
    deliv_db:insert(?MODULE, [EntName, OrgName], ProjName).

-spec delete(d_project()) -> ok | {error, any()}.
delete(Proj) ->
    deliv_db:delete(Proj).

%% @doc Deletes the repo, if it exists, and the project from the database.
%% A subsequent call to an already deleted project also results in a
%% succesful operation.
%%
%% To disable that behavior, and return information about projects that don't
%% exist, use delete/4.
-spec delete(binary(), binary(), binary()) -> ok | {error, any()}.
delete(EntName, OrgName, ProjName) ->
    delete(EntName, OrgName, ProjName, false).

%% @doc Does the same thing as delete/3, but if the project in question does
%% not exist, it returns that information instead of silently swallowing it.
-spec delete(binary(), binary(), binary(), atom()) -> ok | not_found | {error, any()}.
delete(EntName, OrgName, ProjName, Verbose) ->
    case repo_path(EntName, OrgName, ProjName) of
        {ok, RepoPath} ->
            deliv_git:delete_repo(RepoPath);
        {error, Err} ->
            chef_log:warning("repo does not exist for ~p/~p/~p: ~p~n",
                            [EntName, OrgName, ProjName, Err])
    end,
    case Verbose of
        true ->
            deliv_db:delete_unique_by_scoping_params(?MODULE, [EntName, OrgName], ProjName);
        _ ->
            deliv_db:delete(?MODULE, [EntName, OrgName], ProjName)
    end.

%% @doc Fetch project by id
%% TODO: this should be renamed fetch_by_guid
%% and we should probably use GUIDs as PK anyway!
-spec fetch_by_id(non_neg_integer()) -> {error, not_found} | {ok, d_project()}.
fetch_by_id(ProjectId) ->
    deliv_db:fetch_by_id(?MODULE, ProjectId).

%% @doc Fetch by name
-spec fetch(binary()) -> {error, not_found | _Why}  | {ok, d_project()}.
fetch(Name) ->
    deliv_db:fetch(?MODULE, Name).

%% @doc Fetches by enterprise name & org name & name
-spec fetch(binary(), binary(), binary()) -> {error, not_found | _Why}  |
                                             {ok, d_project()}.
fetch(EntName, OrgName, ProjName) ->
    deliv_db:fetch(?MODULE, [EntName, OrgName], ProjName).

-spec scoping_names_by_id(non_neg_integer()) -> {binary(), binary(), binary()} | {error, atom()}.
scoping_names_by_id(ProjectId) ->
    case deliv_db:select(?MODULE, scoping_names_by_id, [ProjectId]) of
        {ok, [Row]} ->
            to_scoping_names(Row);
        {error, _} = Error ->
            Error
    end.

%% @private
to_scoping_names(Proplist) ->
    EntName = proplists:get_value(<<"ent_name">>, Proplist),
    OrgName = proplists:get_value(<<"org_name">>, Proplist),
    ProjName = proplists:get_value(<<"proj_name">>, Proplist),
    {EntName, OrgName, ProjName}.

-spec to_coords(d_project()) -> #proj_coordinates{}.
to_coords(#deliv_project{id = Id}) ->
    {EntName, OrgName, ProjName} = scoping_names_by_id(Id),
    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}.

%% @doc Rename given project object and new name
-spec rename(d_project(), binary()) -> {error, conflict | any()} |
                                       {ok, d_project()}.
rename(Proj, NewName) ->
    deliv_db:rename(?MODULE, Proj, NewName).

%% @doc Creates a new project, both in the DB and as a repo on the disk
-spec new(binary(), binary(), binary()) -> {ok, d_project()} | {error, _Why}.
new(EntName, OrgName, ProjName) ->
    %% let's first create the object in the DB
    case insert(EntName, OrgName, ProjName) of
        {error, _Why} = Error ->
            Error;
        [Proj] ->
            RepoPath = repo_path(Proj),
            handle_create_repo(deliv_git:create_repo(RepoPath), Proj)
    end.

%% @doc Creates a new SCM project in the DB.
-spec new(binary(), binary(), binary(), binary(), binary() | atom(), binary(), binary()) -> db_op_single_result(d_project()).
new(EntName, OrgName, ProjName, PipeName, deliv_scm_github, RepoOwner, RepoName) ->
    % this is GitHubV1 integration code path.
    Params = [EntName, OrgName, ProjName, PipeName, deliv_scm_github, RepoOwner, RepoName],
    case deliv_db:qfetch(?MODULE, create_scm_project, Params) of
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, new, Params, Why),
            Err;
        [Proj] ->
            {ok, Proj}
    end;
new(EntName, OrgName, ProjName, PipeName, bitbucket_scm, BitbucketKey, RepoName) ->
    Params = [EntName, OrgName, ProjName, PipeName, bitbucket_scm, BitbucketKey, RepoName],
    case deliv_db:qfetch(?MODULE, create_bitbucket_scm_project, Params) of
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, new, Params, Why),
            Err;
        [Proj] ->
            RepoPath = repo_path(Proj),
            handle_create_repo(deliv_git:create_repo(RepoPath), Proj)
    end;
new(EntName, OrgName, ProjName, PipeName, github_scm, RepoOwner, RepoName) ->
    % this is GitHubV2 integration code path.
    Params = [EntName, OrgName, ProjName, PipeName, github_scm, RepoOwner, RepoName],
    case deliv_db:qfetch(?MODULE, create_scm_project, Params) of
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, new, Params, Why),
            Err;
        [Proj] ->
            RepoPath = repo_path(Proj),
            handle_create_repo(deliv_git:create_repo(RepoPath), Proj)
    end.

handle_create_repo({error, _Why} = Error, Proj) ->
    %% let's cleanup the DB if possible, but we don't report an error
    %% in doing so, if any occurs
    delete(Proj),
    Error;
handle_create_repo({ok, _Output}, Proj) ->
    ensure_repo_hooks(Proj).

%% @doc Updates the hooks in a project's repo to the latest.
-spec ensure_repo_hooks(d_project()) -> {ok, d_project()} |
                                        {error, binary() | atom()}.
ensure_repo_hooks(Proj) ->
    RepoPath = repo_path(Proj),
    case deliv_git:ensure_hooks(RepoPath) of
      ok ->
        {ok, Proj};
      Error ->
        Error
    end.

-spec ensure_repo_hooks(binary(), binary(), binary()) -> ok |
                                                         {error, binary() |
                                                                 atom()}.
ensure_repo_hooks(EntName, OrgName, ProjName) ->
    {ok, RepoPath} = repo_path(EntName, OrgName, ProjName),
    deliv_git:ensure_hooks(RepoPath).

%% @doc Returns the absolute path on disk to the corresponding bare git repo
-spec repo_path(d_project()) -> binary().
repo_path(Proj) ->
    ReposRoot = delivery_app:get_env(deliv_git_repos_root),
    SlicedGuid = slice_guid(Proj),
    <<ReposRoot/binary, "/", SlicedGuid/binary>>.

%% @doc Returns the absolute path on disk to the corresponding working tree
%% Said working tree might actually not exist (if it hasn't been created
%% yet). This fun is actually pretty much only meant for
%% `deliv_git_working_tree''s use, and should not be used anywhere else.
-spec working_tree_path(d_project()) -> binary().
working_tree_path(Proj) ->
    WorkingTreesRoot = delivery_app:get_env(deliv_git_working_tree_dir),
    SlicedGuid = slice_guid(Proj),
    <<WorkingTreesRoot/binary, "/", SlicedGuid/binary>>.

%% @doc Convenience method to retrieve the repo path for a project
%% using just names.
%%
%% It returns {ok, binary()}, while `repo_path/1' just returns a
%% binary() because it's possible the fetch call here may fail, and
%% callers of this function will want to easily pattern-match to
%% detect the difference.
-spec repo_path(binary(), binary(), binary()) -> {ok, binary()} | {error, not_found}.
repo_path(EntName, OrgName, ProjName) ->
    case fetch(EntName, OrgName, ProjName) of
        {ok, Proj} ->
            {ok, repo_path(Proj)};
        {error, not_found} ->
            {error, not_found}
    end.

%% @private
%% @doc To avoid having one directory containing many
%% subdirectories, which could possibly be problematic on
%% old file systems, we slice the project's GUID in several
%% chuncks of pre-determined lengths, "a la" chef-server for
%% bookshelf; e.g "7f695370-0eeb-11e4-9191-0800200c9a66" gets
%% translated to "7f/69/53/70/0eeb-11e4-9191-0800200c9a66"
-spec slice_guid(d_project()) -> binary().
slice_guid(#deliv_project{guid=Guid}) ->
    <<A:16, B:16, C:16, D:16, $-, Rest/binary>> = Guid,
    <<A:16, "/", B:16, "/", C:16, "/", D:16, "/", Rest/binary>>.

%% @doc Returns the name/path the corresponding git repo, i.e. the one
%% that's visible to end-users, the one they use when doing e.g.
%% git clone '<user>@<ent>'@'<cd-server>/<repo-name>
%% We encode the input to make sure this operation will be reversible
-spec compose_repo_name(binary(), binary(), binary()) -> binary().
compose_repo_name(EntName, OrgName, ProjName) ->
    chef_utils:to_bin(string:join(
        [deliv_encode:encode(chef_utils:to_str(Item)) || Item
            <- [EntName, OrgName, ProjName]],
        "/"
    )).

%% @doc Reverse operation from `compose_repo_name/3' above
%% And so obviously must remain consistent with it!
-spec decompose_repo_name(RepoName) ->
    {EntName, OrgName, ProjName} | error when
    RepoName :: str_or_binary(),
    EntName :: binary(),
    OrgName :: binary(),
    ProjName :: binary().
decompose_repo_name(RepoName) ->
    case re:split(RepoName, "/", [{return, binary}]) of
        List when erlang:is_list(List) andalso length(List) =:= 3 ->
            list_to_tuple([deliv_encode:decode(Item) || Item <- List]);
        _ ->
            error
    end.

%% @doc Returns the project's SCM provider (e.g., "local", "github", "githubV2", "bitbucket")
%% based on the value we store in the DB.
%% Remove the original GitHub provider info when GA
-spec scm_type(d_project()) -> binary().
scm_type(#deliv_project{scm_module = <<"deliv_scm_local">>}) ->
    <<"local">>;
scm_type(#deliv_project{scm_module = <<"bitbucket_scm">>}) ->
    <<"bitbucket">>;
scm_type(#deliv_project{scm_module = <<"deliv_scm_github">>}) ->
    <<"github">>;
scm_type(#deliv_project{scm_module = <<"github_scm">>}) ->
    <<"githubV2">>.

%% TODO: Remove this API and just use deliv_change:changes/5 directly
changes(EntName, OrgName, ProjectName, _UserName, Params) ->
    deliv_change:changes(EntName, OrgName, ProjectName, Params).

-spec convert_to_bb(json(), d_project()) -> d_project() | {error, binary()}.
convert_to_bb(Json, Proj) ->
    ProjId = deliv_project:getval(id, Proj),
    BitbucketKey = ej:get([<<"scm">>, <<"project_key">>], Json),
    BitbucketRepo = ej:get([<<"scm">>, <<"repo_name">>], Json),
    case deliv_db:qfetch(?MODULE, convert_to_bb, [ProjId, BitbucketKey, BitbucketRepo]) of
        [BitbucketProject] ->
            BitbucketProject;
        {error, Why} ->
            {error, Why}
    end.

-spec convert_to_local(d_project()) -> d_project() | {error, binary()}.
convert_to_local(#deliv_project{id = ProjectId}) ->
    case deliv_db:qfetch(?MODULE, convert_to_local, [ProjectId]) of
        [Project] ->
            Project;
        {error, Why} ->
            {error, Why}
    end.

-spec convert_ghv2_to_local(d_project()) -> d_project() | {error, binary()}.
convert_ghv2_to_local(#deliv_project{id = ProjectId}) ->
    case deliv_db:qfetch(?MODULE, convert_ghv2_to_local, [ProjectId]) of
        [Project] -> Project;
        {error, _Why} = Error -> Error
    end.

-spec convert_to_githubV2(binary(), binary(), d_project()) -> d_project() | {error, binary()}.
convert_to_githubV2(GitHubOwner, GitHubRepo, Proj) ->
    ProjId = deliv_project:getval(id, Proj),
    case deliv_db:qfetch(?MODULE, convert_to_githubV2, [ProjId, GitHubOwner, GitHubRepo]) of
        [GitHubProject] ->
            GitHubProject;
        {error, Why} ->
            {error, Why}
    end.

%% @doc Returns the metadata module that can be used to load the given projects metadata.
-spec metadata_module(d_project()) -> none | scm_bitbucket_project_metadata | deliv_project_github_metadata.
metadata_module(#deliv_project{scm_module = <<"deliv_scm_local">>}) ->
    none;
metadata_module(#deliv_project{scm_module = <<"bitbucket_scm">>}) ->
    scm_bitbucket_project_metadata;
metadata_module(#deliv_project{scm_module = <<"github_scm">>}) ->
    scm_github_project_metadata;
metadata_module(_) -> deliv_project_github_metadata.
