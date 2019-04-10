-module(deliv_patchset_diffstat).

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

-record(deliv_patchset_diffstat, {
          id                :: db_id(),
          files_changed     :: non_neg_integer(),
          insertions        :: non_neg_integer(),
          deletions         :: non_neg_integer()
         }).

'#insert_fields'() -> [id, files_changed, insertions, deletions].

%% we never update those
'#update_fields'() -> [].

'#statements'() -> [default].

'#table_name'() -> "patchset_diffstats".

-spec to_json(d_patchset_diffstat()) -> json().
to_json(DiffStat) ->
    {[{<<"insertions">>, getval(insertions, DiffStat)},
      {<<"deletions">>, getval(deletions, DiffStat)}]}.

%% @doc Fetches the diffstats for the given patchset
%% If the data is in the DB, it gets it from there
%% Otherwise, it gets the data from the git repo itself, and proceeds
%% to cache it in the DB
-spec fetch(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
        -> {ok, d_patchset_diffstat()} | {error, _Why}.
fetch(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_db:fetch_by_id(?MODULE, PatchsetId) of
        {error, not_found} ->
            %% means we haven't cached that patchset yet
            compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset);
        {ok, _DiffStat} = Result ->
            Result;
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% @doc Gets the result from git calls, then caches it in the DB
compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    case deliv_git:patchset_diffstats(EntName, OrgName, ProjName, ChangeId,
                                      PipeName, FeatureBranch, Patchset) of
        {ok, DiffStat} = Result ->
            %% we ignore any error that might arise during the caching
            deliv_db:insert(DiffStat),
            Result;
        {error, _Why} = Error ->
            Error
    end.
