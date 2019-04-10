-module(deliv_patchset_commit).

-include("deliv_types.hrl").

%% API
-export([
         fetch/7,
         to_json/1
       ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_patchset_commit, {
          patchset_id   :: db_id(),
          sha           :: binary(),
          subject       :: binary(),
          body          :: binary()
         }).

%% we use a DB-side fun to do the inserts
'#insert_fields'() -> [].

%% we never update those
'#update_fields'() -> [].

'#statements'() ->
    [{insert_patchset_commit,
      "SELECT * FROM insert_patchset_commit($1, $2, $3, $4, $5)"},
     {fetch_by_patchset_id,
      sqerl_rec:gen_fetch(?MODULE, [patchset_id])}].

'#table_name'() ->
    %% actually a view, but doesn't matter here
    "patchset_commits".

-spec to_json(#deliv_patchset_commit{}) -> [binary()].
to_json(#deliv_patchset_commit{subject = Subject, body = Body, sha = Sha}) ->
    [Subject, Body, Sha].

%% @doc Fetches all the commits for the given patchset
%% If the data is in the DB, it gets it from there
%% Otherwise, it gets the data from the git repo itself, and proceeds
%% to cache it in the DB
-spec fetch(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
        -> {ok, [d_patchset_commit()]} | {error, _Why}.
fetch(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_db:fetch2(?MODULE, patchset_id, PatchsetId) of
        {ok, []} ->
            %% means we haven't cached that patchset yet
            compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset);
        {ok, List} when is_list(List) ->
            %% the DB returns sorted by ASC order (more efficient for it - ?)
            %% but we want DESC
            {ok, lists:reverse(List)};
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% @doc Gets the result from git calls, then caches it in the DB
compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    case deliv_git:patchset_commit_log(EntName, OrgName, ProjName, ChangeId,
                                       PipeName, FeatureBranch, Patchset) of
        {ok, Commits} = Result ->
            %% we ignore any error that might arise during the caching
            cache(Commits, Patchset),
            Result;
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% @doc Does the caching. As mentioned above, we ignore if we fail
%% TODO: bulk insert, in a transaction! otherwise we can end up with a funny
%% state here (next PR?)
-spec cache([d_patchset_commit()], d_patchset()) -> _.
cache(Commits, Patchset) ->
    %% we need the project id - ugly, but fine for now
    {ok, Change} = deliv_change:fetch_by_id(deliv_patchset:getval(change_id, Patchset)),
    {ok, Pipeline} = deliv_pipeline:fetch_by_id(deliv_change:getval(pipeline_id, Change)),
    ProjectId = deliv_pipeline:getval(project_id, Pipeline),
    lists:foreach(
        fun(Commit) -> do_cache(Commit, ProjectId) end,
        %% we need to store them in chronological order
        lists:reverse(Commits)
    ).

%% @private
-spec do_cache(d_patchset_commit(), non_neg_integer()) -> _.
do_cache(Commit, ProjectId) ->
    Params = [ProjectId, getval(patchset_id, Commit),
                         getval(sha, Commit),
                         getval(subject, Commit),
                         getval(body, Commit)],
    deliv_db:select(?MODULE, insert_patchset_commit, Params).
