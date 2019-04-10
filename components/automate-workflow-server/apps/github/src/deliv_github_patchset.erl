-module(deliv_github_patchset).

-include_lib("delivery/include/deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
-export([
         insert/1,
         fetch_by_patchset_id/1,
         fetch_latest_by_issue_url/1,
         get_change_id_by_patchset/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-export([
         clone_url/1,
         pr_url/1,
         save/2,
         update/1
        ]).

-record(deliv_github_patchset, {
          id :: db_id(),
          patchset_id :: db_id(),
          payload :: binary(),
          status_comment_id :: integer()
         }).

%% Those are not actually used since we do
%% all the insert/update operations through custom DB funs
'#insert_fields'() -> [patchset_id, payload, status_comment_id].
'#update_fields'() -> [payload, status_comment_id].

'#table_name'() -> "github_patchsets".

'#statements'() ->
    [default,

     %% $1 = Patchset ID (GUID)
     {get_clone_url,
      "SELECT payload->'pull_request'->'head'->'repo'->>'ssh_url' AS clone_url FROM "
      ++ '#table_name'() ++ " WHERE patchset_id=$1"},
     {fetch_by_patchset_id,
      sqerl_rec:gen_fetch(?MODULE, patchset_id)},
     {fetch_latest_by_issue_url,
      %% Json query, looking for a patchset where the payload has a key issue_url under
      %%  pull_request with a value that matches $1
      "SELECT * FROM github_patchsets WHERE payload #>> '{pull_request, issue_url}' = $1"
      ++ "ORDER BY patchset_id DESC LIMIT 1"},
     {get_pr_url,
      "SELECT payload->'pull_request'->>'html_url' AS pr_url FROM "
      ++ '#table_name'() ++ " WHERE patchset_id=$1"}
    ].

%% @doc Take a payload and save it as a new github_patchset record.
-spec save(json(), d_patchset()) -> db_op_result(d_github_patchset()).
save(Payload, Patchset) ->
    insert(#deliv_github_patchset{patchset_id = deliv_patchset:getval(id, Patchset),
                                  payload = chef_json:encode(Payload)}).

%% @doc Fetch a github_patchset record based on the patchset_id
-spec fetch_by_patchset_id(non_neg_integer()) -> db_op_result(d_github_patchset()).
fetch_by_patchset_id(PatchsetId) ->
    deliv_db:qfetch(?MODULE, fetch_by_patchset_id, [PatchsetId]).

%% @doc Fetch the latest github_patchset record based on the issue url
-spec fetch_latest_by_issue_url(binary()) -> db_op_result(d_github_patchset()).
fetch_latest_by_issue_url(IssueUrl) ->
    deliv_db:qfetch(?MODULE, fetch_latest_by_issue_url, [IssueUrl]).

%% @doc Insert a new github_patchset record into the database.
-spec insert(d_github_patchset()) -> db_op_result(d_phase_run()).
insert(#deliv_github_patchset{} = Rec) ->
    deliv_db:insert(Rec).

%% @doc Update a github_patchset record in the database.
-spec update(d_github_patchset()) -> {error, not_found} |
                            {ok, d_github_patchset()}.
update(Patchset) ->
    deliv_db:update(Patchset).

%% @doc Fetch the clone_url value from the payload of the github_patchset
%% associated with the given Patchset.
-spec clone_url(d_patchset()) -> binary().
clone_url(Patchset) ->
    {ok, [Row]} = deliv_db:select(?MODULE, get_clone_url,
                                  [deliv_patchset:getval(id, Patchset)]),
    proplists:get_value(<<"clone_url">>, Row).

%% @doc Return the ChangeId looked up by patchset
-spec get_change_id_by_patchset(d_github_patchset()) -> binary().
get_change_id_by_patchset(GithubPatchset) ->
    PatchsetId = getval(patchset_id, GithubPatchset),
    {ok, Patchset} = deliv_patchset:fetch(PatchsetId),
    deliv_patchset:getval(change_id, Patchset).

-spec pr_url(d_patchset()) -> binary().
pr_url(Patchset) ->
    %% deliv_patchset id and deliv_github_patchset patchset_id are the same,
    %% see: save/2
    PatchsetId = deliv_patchset:getval(id, Patchset),
    {ok, [Row]} = deliv_db:select(?MODULE, get_pr_url, [PatchsetId]),
    proplists:get_value(<<"pr_url">>, Row).
