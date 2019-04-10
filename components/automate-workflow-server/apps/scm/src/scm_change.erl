-module(scm_change).
%% doc This module saves and fetches rows on the change_bitbucket_metadata
%% table

-include_lib("delivery/include/deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
-export([
         save/3,
         get_hal/2,
         fetch_by_change_id/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(?MODULE, { id,
                   change_id,
                   pr_url,
                   pr_id}).
%% pr_id is an external id set by the external scm

'#insert_fields'() -> [change_id, pr_id, pr_url].

'#update_fields'() -> [].

'#statements'() ->
    [default,
     {fetch,
      <<"SELECT m.*
           FROM scm_changes AS m
          WHERE m.change_id = $1">>}
    ].

'#table_name'() -> "scm_changes".

-spec fetch_by_change_id(binary()) -> {error, not_found} | {ok, scm_change()}.
fetch_by_change_id(ChangeId) ->
    case deliv_db:qfetch(scm_change, fetch, [ChangeId]) of
        [Result] ->
            {ok, Result};
        [] ->
            {error, not_found};
        {error, Why} ->
            chef_log:failed_call(?MODULE, fetch, [ChangeId], Why),
            {error, not_found}
    end.

-spec save(binary(), non_neg_integer(), binary()) -> db_op_result().
save(ChangeId, PrId, PrUrl) ->
    deliv_db:insert(#scm_change{change_id = ChangeId, pr_url = PrUrl, pr_id = PrId}).

-spec get_hal(binary(), bitbucket_scm | github_scm) -> json().
get_hal(ChangeId, ScmModule) ->
    case fetch_by_change_id(ChangeId) of
        {ok, #scm_change{pr_id = PullRequestId, pr_url = PullRequestUrl}} ->
            PullRequestTitle = ScmModule:pull_request_description(PullRequestId),
            {[{<<"href">>, PullRequestUrl}, {<<"title">>, PullRequestTitle}]};
        {error, not_found} ->
            {[]}
    end.
