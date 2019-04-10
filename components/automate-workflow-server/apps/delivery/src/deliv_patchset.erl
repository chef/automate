-module(deliv_patchset).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
-export([
         delete/1,
         fetch/1,
         fetch/2,
         fetch/6,
         new/7,
         patchsets_for_change/1,
         latest_patchset_for_change/1,
         update/1,
         list_all/0
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

-export([
         to_ejson/2,
         subscribe_patchset_events/0
        ]).


-record(deliv_patchset, {
          id                    :: db_id(),
          change_id             :: db_guid(),
          sequence_number       :: non_neg_integer(),
          submitted_at          :: calendar:datetime(),
          sha                   :: binary(),
          submitter_id          :: db_id(),
          verified_against_sha, %% should be a binary but appears to be unused?
          is_verified           :: boolean(),
          status                :: binary()
         }).
%% Those are not actually used since we do
%% all the insert/update operations through a custom DB function. We do keep
%% a few fields, since they are used as part of the gen_scoped_sqerl_statements_test
%% TODO: Determine if that test is accurate or needs to be rewritten
'#insert_fields'() -> [change_id].
'#update_fields'() -> [is_verified, status].

'#statements'() ->
    %% we use at least `deliv_patchset_fetch_by_id' from the default queries
    [default,
     {create_patchset_and_change,
      "SELECT * FROM create_patchset_and_change($1, $2, $3, $4, $5, $6, $7)"},
     {fetch_by_change_id_and_sequence_number,
      sqerl_rec:gen_fetch(?MODULE, [change_id, sequence_number])},
     %% Results are returned in reverse-chronological order. If the
     %% given change has been merged, this will return one patchset
     %% (the most recent, the one that got merged); for any other
     %% status, all patchset records are returned.
     {patchsets_for_change,
      "SELECT * FROM get_patchsets_for_change($1)"},
     {get_latest_patchset,
      "SELECT * FROM get_latest_patchset($1)"},
     {fetch_all, sqerl_rec:gen_fetch_all(?MODULE, change_id)}
     | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

'#table_name'() ->
    "patchsets".

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_change, change_id}.

-spec scoping_column_name() -> sequence_number.
scoping_column_name() -> sequence_number.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec delete(d_patchset()) -> ok | {error, _}.
delete(Patchset) ->
    deliv_db:delete(Patchset).

%% @doc Ensures an on-going change exists for this pipeline and feature
%% branch names, and proceeds to create a new patchset
%% Since we can easily have conflicts when trying to create new patchsets
%% due to race conditions (and since that's the only way we can have conflicts)
%% we retry a few times if we get a conflict (especially relevant for tests, but
%% might happen IRL too!)
-spec new(binary(), binary(), binary(), binary(), binary(), binary(), binary())
         -> db_op_result(d_patchset()).
new(EntName, UserName, OrgName, ProjName, PipeName, FeatureBranch, Sha) ->
    retry_new([EntName, UserName, OrgName, ProjName, PipeName, FeatureBranch, Sha], 5, null).

%% @private
retry_new(_Params, 0, LastError) ->
    LastError;
retry_new(Params, RemainingTries, _LastError) ->
    case deliv_db:qfetch(?MODULE, create_patchset_and_change, Params) of
        [_Patchset] = Success -> Success;
        {error, {conflict, _}} = Error -> retry_new(Params, RemainingTries - 1, Error);
        {error, _Why} = OtherError -> OtherError
    end.

-spec fetch(non_neg_integer()) -> {ok, d_patchset()} | {error, not_found | _Why}.
fetch(PatchsetId) ->
    deliv_db:fetch_by_id(?MODULE, PatchsetId).

-spec fetch(binary(), non_neg_integer()) -> db_op_result(d_patchset()).
fetch(ChangeId, SeqNumber) ->
    deliv_db:qfetch(?MODULE, fetch_by_change_id_and_sequence_number, [ChangeId, SeqNumber]).

%% @doc Fetches a patchset for an _opened_ changed (i.e. not merged yet)
%% Otherwise, use `fetch/2'
-spec fetch(binary(), binary(), binary(), binary(), binary(), non_neg_integer())
           -> db_op_result(d_patchset()) | {error, not_found}.
fetch(EntName, OrgName, ProjName, PipeName, FeatBranchName, SeqNumber) ->
    deliv_db:fetch(?MODULE, [EntName, OrgName, ProjName,
                             PipeName, FeatBranchName], SeqNumber).

-spec update(d_patchset()) ->  {error, conflict | any()} |
                               {ok, d_patchset()}.
update(Patchset) ->
    deliv_db:update(Patchset).

%% @doc Return all patchsets for a given change (identified by UUID)
%% in order by sequence number, starting at "1".
-spec patchsets_for_change(binary()) -> db_op_result(d_patchset()).
patchsets_for_change(ChangeId) ->
    sqerl_rec:qfetch(?MODULE, patchsets_for_change, [ChangeId]).

%% @doc Returns the latest patchset for that change
-spec latest_patchset_for_change(binary()) -> {ok, d_patchset()}
                                                  | {error, _Why}.
latest_patchset_for_change(ChangeId) ->
    case sqerl_rec:qfetch(?MODULE, get_latest_patchset, [ChangeId]) of
        [Patchset] ->
            {ok, Patchset};
        [] ->
            {error, patchset_not_found};
        {error, _Why} = Error ->
            Error
    end.

%% @doc Return a JSON representation of the Patchset.
-spec to_ejson(d_common_scope(), d_patchset()) -> {ok, json()} | {error, term()}.
to_ejson(Scope, Patchset) ->
    ScmMod = deliv_scopes:'#get'(scm_module, Scope),
    case ScmMod:patchset_metadata_ejson(Scope, Patchset) of
        {ok, {CommitsJson, DiffstatsJson, ChangedFilesJson}} ->
            Id = deliv_patchset:getval(sequence_number, Patchset),
            SubmittedAt = chef_utils:format_timestamp(deliv_patchset:getval(submitted_at, Patchset)),
            SHA = deliv_patchset:getval(sha, Patchset),
            {ok, {[
                   {<<"sequence_number">>, Id},
                   {<<"sha">>, SHA},
                   {<<"submitted_at">>, SubmittedAt},
                   {<<"stats">>, DiffstatsJson},
                   {<<"files_changed">>, ChangedFilesJson},
                   {<<"commit_msgs">>, CommitsJson}
                  ]}};
        {error, _} = Error -> Error
    end.

subscribe_patchset_events() ->
  deliv_event:subscribe([patchset_created]).

list_all() ->
  deliv_db:list_all(?MODULE).
