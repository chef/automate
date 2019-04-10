-module(deliv_patchset_changed_file).

-include("deliv_types.hrl").

%% API
-export([
         delete_files/1,
         fetch/7,
         to_json/1,
         update_by_patchset_id_file/1,
         refresh_cache/3
       ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-compile({parse_transform, sqerl_gobot}).


-record(deliv_patchset_changed_file, {
          patchset_id   :: db_id(),
          status        :: binary(), %% added, deleted, modified
          file          :: binary(),
          inserts       :: binary(),
          deletes       :: binary()
         }).
'#insert_fields'() -> [patchset_id, status, file, inserts, deletes].

%% we never update those
'#update_fields'() -> [status, file, inserts, deletes].

'#statements'() ->
    [default, %% used for inserts
     {delete_by_patchset_id,
      sqerl_rec:gen_delete(?MODULE, [patchset_id])},
     {fetch_by_patchset_id,
      sqerl_rec:gen_fetch(?MODULE, [patchset_id])},
     {update_by_patchset_id_file,
      <<"UPDATE patchset_changed_files SET
        status = $1, file = $2, inserts = $3, deletes = $4
        WHERE patchset_id = $5 AND file = $2">>}
    ].

'#table_name'() -> "patchset_changed_files".

-spec to_json(#deliv_patchset_changed_file{}) -> [binary()].
to_json(#deliv_patchset_changed_file{status = Status, file = File, inserts = Inserts, deletes = Deletes}) ->
    [translate(Status), File, Inserts, Deletes].

translate(<<"added">>) -> <<"A">>;
translate(<<"modified">>) -> <<"M">>;
translate(<<"deleted">>) -> <<"D">>.

%% @doc Refreshes the changed files in the database from disk
-spec refresh_cache(d_common_scope(), d_change(), d_patchset()) -> ok | {error, term()}.
refresh_cache(Scope, Change, Patchset) ->
    [EntName, OrgName, ChangeId, PipeName, ProjName, ScmMod]
        = deliv_scopes:'#get'([ent_name, org_name, change_id, pipe_name, proj_name, scm_module], Scope),
    case ScmMod of
        deliv_scm_github ->
            %% We can't pull patchset file data from github
            ok;
        _ ->
            case delete_files(Patchset) of
                ok ->
                    FeatureBranch = deliv_change:getval(feature_branch, Change),
                    case fetch(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) of
                        {ok, _List} -> ok;
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

%% @doc Deletes all the changed files for the given patchset
-spec delete_files(d_patchset()) -> ok | {error, _Why}.
delete_files(Patchset) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_db:fetch2(?MODULE, patchset_id, PatchsetId) of
        {ok, []} ->
            %% no files to worry about
            ok;
        {ok, [PatchsetFile | _List]} ->
            % We need an example record for delete2, so we just pull the first one;
            % deleting by patchset_id will delete all relevant records in the set.
            deliv_db:delete2(PatchsetFile, patchset_id);
        {error, _Why} = Error ->
            Error
    end.

%% @doc Fetches all the changed files for the given patchset
%% If the data is in the DB, it gets it from there
%% Otherwise, it gets the data from the git repo itself, and proceeds
%% to cache it in the DB
-spec fetch(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
        -> {ok, [d_patchset_changed_file()]} | {error, _Why}.
fetch(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_db:fetch2(?MODULE, patchset_id, PatchsetId) of
        {ok, []} ->
            %% means we haven't cached that patchset yet
            compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset);
        {ok, List} when is_list(List) ->
            {ok, translate_rows(List)};
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% @doc Gets the result from git calls, then caches it in the DB
compute_and_cache(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    {ok, ChangedFiles} = Result = deliv_git:patchset_changed_files(EntName, OrgName, ProjName, ChangeId,
                                                          PipeName, FeatureBranch, Patchset),
    PatchsetId = deliv_patchset:getval(id, Patchset),
    %% we ignore any error that might arise during the caching
    cache(ChangedFiles, PatchsetId),
    Result.

%% @private
%% @doc Does the caching. As mentioned above, we ignore if we fail
%% TODO: bulk insert, in a transaction! otherwise we can end up with a funny
%% state here (next PR?)
-spec cache([d_patchset_changed_file()], non_neg_integer()) -> _.
cache([], PatchsetId) ->
    cache([#deliv_patchset_changed_file{file = <<"none">>,
                                        status = <<"none">>,
                                        patchset_id = PatchsetId}], PatchsetId);
cache(ChangedFiles, _PatchsetId) ->
    lists:foreach(
        fun(ChangedFile) -> deliv_db:insert(ChangedFile) end,
        ChangedFiles
    ).

%% @private
%% @doc We store a dummy row for patchsets that don't actually
%% change any file
-spec translate_rows([d_patchset_changed_file()]) -> [d_patchset_changed_file()].
translate_rows([#deliv_patchset_changed_file{file = <<"none">>,
                                             status = <<"none">>}]) -> [];
translate_rows(Other) -> Other.

update_by_patchset_id_file(#deliv_patchset_changed_file{
          patchset_id = PatchsetId,
          status = Status,
          file = File,
          inserts = Inserts,
          deletes = Deletes
         }) ->
  sqerl_rec:cquery(deliv_patchset_changed_file, update_by_patchset_id_file, [Status, File, Inserts, Deletes, PatchsetId]).
