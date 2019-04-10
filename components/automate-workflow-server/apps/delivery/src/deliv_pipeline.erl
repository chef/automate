-module(deliv_pipeline).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
         delete/1,
         delete/4,
         insert/4,
         create/6,
         fetch_by_id/1,
         fetch/1,
         fetch/4,
         scoping_names/1,
         update/1
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
         fetch_names/3,
         rename/2,
         delete_with_branch/4
        ]).

-record(deliv_pipeline, {
          id            :: db_id(),
          project_id    :: db_id(),
          name          :: binary()
         }).

-compile({parse_transform, sqerl_gobot}).

'#insert_fields'() ->
    [project_id, name].

'#update_fields'() ->
    [project_id, name].

'#statements'() ->
    [default,
      {fetch_by_name, sqerl_rec:gen_fetch(deliv_enterprise, name)},
      {delete_by_name, sqerl_rec:gen_delete(deliv_enterprise, name)},
      {fetch_names, deliv_sqerl_rec:gen_fetch_names_by_scoping_names(?MODULE)},
      deliv_sqerl_rec:gen_fetch_scoping_params_from_id(?MODULE)
     | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

'#table_name'() ->
    "pipelines".

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_project, project_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec fetch_names(binary(), binary(), binary()) -> db_op_result(binary()).
fetch_names(EntName, OrgName, ProjName) ->
    deliv_db:fetch_names(?MODULE, [EntName, OrgName, ProjName]).

-spec update(d_pipeline()) -> {error, conflict | any()} |
                              {ok, d_pipeline()}.
update(Pipe) ->
    deliv_db:update(Pipe).

-spec insert(binary(), binary(), binary(), binary()) -> db_op_result(d_pipeline()).
insert(EntName, OrgName, ProjName, PipeName) ->
    deliv_db:insert(?MODULE, [EntName, OrgName, ProjName], PipeName).

-spec create(binary(), binary(), binary(), binary(), binary(), binary()) ->
                    ok | {error, atom() | tuple()}.
create(Base, EntName, OrgName, ProjName, PipeName, RepoPath) ->
    BranchType   = deliv_git:branch_type(PipeName),
    TargetExists = deliv_git:branch_exists(RepoPath, PipeName),

    Ret =
        case {Base, TargetExists, BranchType} of
            %% Happy-path cases
            {undefined, branch_exists, normal_branch} ->
                %% Normal case: Creating a pipeline from an existing branch
                ok;
            {Base, no_branch_exists, normal_branch} when Base =/= undefined ->
                %% Normally I wouldn't like setting up the pattern
                %% matching using a guard like this, but this allows us to
                %% put the common, happy-path cases at the front of the
                %% list

                %% Normal case: Creating a new branch and corresponding pipeline at once
                case deliv_git:create_branch(RepoPath, PipeName, Base) of
                    ok ->
                        ok;
                    _Error ->
                        %% Git failed creating the branch; the error will
                        %% have already been logged
                        {error, system_error}
                end;

            %% Error cases from here on out
            {undefined, no_branch_exists, normal_branch} ->
                {error, {early, no_base_and_branch_doesnt_exist}};
            {Base, branch_exists, normal_branch} ->
                %% You tried to create a new branch (because you specified
                %% a base from which to branch off), but it already
                %% exists!
                {error, {early, given_base_but_branch_exists}};
            {_, _, reserved_branch} ->
                %% You can't set up a pipeline from one of our internal
                %% branches, or create a new internal branch this way
                {error, {early, reserved_branch_name}};
            {_, _Error, _} ->
                %% If we get here, Git failed when checking if the branch
                %% exists; the error will have already been logged, though
                {error, {early, system_error}}
        end,
    case Ret of
        ok ->
            %% we've made sure to create the branch before this point
            case insert(EntName, OrgName, ProjName, PipeName) of
                [_Pipe] ->
                    ok;
                Err ->
                    Err
            end;
        Err ->
            %% Clean up the git branch we created, but only if we
            %% actually created it (in which case, we will have been
            %% given a base reference)
            case Base of
                undefined -> ok;
                Base ->
                    case deliv_git:force_delete_branch(RepoPath, PipeName) of
                        ok -> ok;
                        {error, Reason} ->
                            %% Just log the error for now; we're still
                            %% going to return an error to the user.
                            chef_log:log(error,
                                          "Failed to delete branch '~p' when setting up a pipeline "
                                          "for it in ~p/~p/~p in repo ~p: ~p",
                                          [PipeName, EntName, OrgName, ProjName, RepoPath, Reason])
                    end
            end,
            Err
    end.

-spec delete(d_pipeline()) -> ok | {error, _}.
delete(Pipe) ->
    deliv_db:delete(Pipe).

-spec delete(binary(), binary(), binary(), binary()) -> ok | {error, _}.
delete(EntName, OrgName, ProjName, PipeName) ->
    deliv_db:delete(?MODULE, [EntName, OrgName, ProjName], PipeName).

%% @doc Deletes both the record in the DB, and the branch
%% TODO: here too, we could use transactions!
%% We should rollback the DB deletion if the branch deletion fails
-spec delete_with_branch(binary(), binary(), binary(), binary()) -> {ok, integer()} | {error, _}.
delete_with_branch(EntName, OrgName, ProjName, PipeName) ->
    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    ok = deliv_git:force_delete_branch(RepoPath, PipeName),
    delete(EntName, OrgName, ProjName, PipeName).

    %% TODO, think of better way to do below
    %% case delete(EntName, OrgName, ProjName, PipeName) of
    %%     {ok, 0} ->
    %%         %% no need to delete any branch
    %%         {ok, 0};
    %%     {ok, 1} ->
    %%         %% TODO: account for possible errors here
    %%         %% didn't do it yet, as I can't rollback the DB deletion as is,
    %%         %% so no clean way to do it... :-/
    %%         {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    %%         ok = deliv_git:force_delete_branch(RepoPath, PipeName),
    %%         {ok, 1};
    %%     {error, _Why} = Error ->
    %%         Error
    %% end.

-spec fetch_by_id(non_neg_integer()) -> {ok, d_pipeline()} | {error, not_found}.
fetch_by_id(PipelineId) ->
    deliv_db:fetch_by_id(?MODULE, PipelineId).

-spec fetch(binary()) -> {error, not_found} |
                         {ok, d_pipeline()}.
fetch(PipeName) ->
    deliv_db:fetch(?MODULE, PipeName).

-spec fetch(binary(), binary(), binary(), binary()) -> {error, not_found} |
                                                       {ok, d_pipeline()}.
fetch(EntName, OrgName, ProjName, PipeName) ->
    deliv_db:fetch(?MODULE, [EntName, OrgName, ProjName], PipeName).

%% @doc Returns a list `[EntName, OrgName, ProjName]'
-spec scoping_names(non_neg_integer()) -> [binary()] | {error, _Why}.
scoping_names(PipelineId) ->
    deliv_sqerl_rec:fetch_scoping_params_from_id(?MODULE, PipelineId).

-spec rename(d_pipeline(), binary())
        -> {ok, d_pipeline()} | {error, branch_exists | term()}.
rename(Pipe, NewName) ->
    NewPipe = setvals([{name, NewName}], Pipe),
    case update(NewPipe) of
        {ok, UpdatedPipe} ->
            handle_rename_pipeline_branch(
              rename_pipeline_branch(Pipe, NewName),
              Pipe, UpdatedPipe);
        Other ->
            Other
    end.

handle_rename_pipeline_branch(ok, _OldPipe, UpdatedPipe) ->
    {ok, UpdatedPipe};
handle_rename_pipeline_branch({error, _Why} = Error, OldPipe, _UpdatedPipe) ->
    %% then we need to revert the renaming in the DB
    %% TODO: this should be turned into a transaction when sqerl supports them
    update(OldPipe),
    Error.

%% @private
%% @doc When renaming a pipeline, we also need/want to move the corresponding
%% git branch
-spec rename_pipeline_branch(d_pipeline(), binary()) -> ok | {error, _Why}.
rename_pipeline_branch(OldPipe, NewName) ->
    OldName = getval(name, OldPipe),
    ProjectId = getval(project_id, OldPipe),
    case deliv_project:fetch_by_id(ProjectId) of
        {ok, Project} ->
            RepoPath = deliv_project:repo_path(Project),
            deliv_git:move_branch(RepoPath, OldName, NewName);
        {error, _} = Error ->
            Error
    end.
