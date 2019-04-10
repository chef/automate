%% @doc Module for generating a md table showing delivery's status for a change.
%%      This is used in deliv_github_chatops to post a comment in a github.
-module(deliv_github_status_comment).

-include_lib("delivery/include/deliv_types.hrl").

-export([sync_comment/2]).

%% Export Helpers for testing.
-export([build_status_comment_from/2,
         build_comment_plist/1,
         build_comment_table/2,
         build_status_shield/2,
         list_to_table_row/1,
         zipn/1]).

-spec sync_comment(d_common_scope(), binary()) -> ok | {error, binary()}.
sync_comment(Scope, ChangeId) ->
    {ok, Patchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
    PatchsetId = deliv_patchset:getval(id, Patchset),

    [GHPatchset] = deliv_github_patchset:fetch_by_patchset_id(PatchsetId),
    {ok, PhaseRunSummary} = deliv_change:get_phase_run_summary(ChangeId),

    case deliv_github_patchset:getval(status_comment_id, GHPatchset) of
        undefined ->
            %% No comment set yet. Lets Create it.
            create_comment(Scope, GHPatchset, PhaseRunSummary);
        StatusCommentId ->
            {ok, _} = deliv_github_api:update_commit_comment(Scope, chef_utils:to_bin(StatusCommentId),
                                                             build_status_comment_from(Scope, PhaseRunSummary))
    end.

%% @private
create_comment(Scope, GHPatchset, PhaseRunSummary) ->
    create_comment(chef_json:decode(deliv_github_patchset:getval(payload, GHPatchset)), Scope, GHPatchset, PhaseRunSummary).

create_comment({error, invalid_json}, _, GHPatchset, _) ->
    chef_log:error("Github failed to decode json payload for github patchset ~p", [GHPatchset]);
create_comment(GHPayloadJson, Scope, GHPatchset, PhaseRunSummary) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    Sha = deliv_github_pull_request:commit_sha(GHPayloadJson),
    CommentBody = build_status_comment_from(Scope, PhaseRunSummary),
    case deliv_github_api:create_commit_comment(Scope, Sha, CommentBody) of
        {ok, Comment} ->
            CommentId = ej:get([<<"id">>], chef_json:decode(Comment)),
            UpdatedGHPatchset = deliv_github_patchset:setvals([{status_comment_id, CommentId}], GHPatchset),
            {ok, _} = deliv_github_patchset:update(UpdatedGHPatchset);
        Err ->
            chef_log:error("Github failed to create status comment for ChangeId ~p, error: ~p",
                           [ChangeId, Err])
    end.

%% @private
%% @returns the markdown for the status comment
build_status_comment_from(Scope, PhaseRunSummary) ->
  CommentPList = build_comment_plist(PhaseRunSummary),
  build_comment_table(Scope, CommentPList).

%% @private
%% Takes a PhaseRunSummary and returns it in the below format.  Used for the status comment
%% Return structure: [{stage, [#phase_run_summary, ...]}, {stage2, [#phase_run_summary..]} ]
build_comment_plist(PhaseRunSummary) ->
  lists:foldl(fun comment_plist_helper/2, [], PhaseRunSummary).

%% @private
comment_plist_helper(#phase_run_summary{stage = Stage} = PhaseRun, StagePlist) ->
   case proplists:get_value(Stage, StagePlist) of
       undefined ->
          lists:append(StagePlist, [{Stage, [PhaseRun]}]);
       Value ->
          lists:keyreplace(Stage, 1, StagePlist, {Stage, lists:append(Value, [PhaseRun])})
   end.

%% @private
%% The table view of the status comment
build_comment_table(Scope, CommentPList) ->
    %% use prop lists?
    Keys = [K || {K,_V} <- CommentPList],
    Header = [list_to_table_row(lists:map(fun chef_utils:capitalize_str/1, Keys)),
              list_to_table_row(lists:map(fun(_) -> ":--" end, Keys))],
    Body = build_comment_table_body(Scope, CommentPList, Keys),
    erlang:iolist_to_binary(
        ["Delivery Status:\n",
         "----------------\n",
         Header,
         Body]).

%% @private
%% Builds markdown for the body of the table in the status comment.
build_comment_table_body(Scope, CommentPList, Keys) ->
    Arrays = lists:map(fun(Key) -> V = proplists:get_value(Key, CommentPList), V end, Keys),

    %% Pad the arrays so they're all the same length
    Max = lists:max(lists:map(fun(A) -> length(A) end, Arrays)),
    PaddedArrays = lists:map(fun(A) ->
                                Diff = Max - length(A),
                                lists:append(A, lists:duplicate(Diff, undefined))
                             end,
                             Arrays),

    %% We've got our arrays now, but we want to display them vertically
    TransposedArrays = zipn(PaddedArrays),

    ShieldForPhaseRun = fun(#phase_run_summary{} = PhaseRun) ->
                               build_status_shield(Scope, PhaseRun);
                           (undefined) ->
                               " "
                        end,
    lists:map(fun(PhaseRun) ->
                  list_to_table_row(PhaseRun, ShieldForPhaseRun)
              end,
              TransposedArrays).

%% @private
%% This function zips n lists of equal length. It ia analogus to lists:zip3/3
%% except handles n lists passed in as a list of lists of equal length. We are
%% using it here to group_by and change the axises of the phase run matrix.
%% All subarrays are required to be the same length
%% Example: [[1,2,3],[a,b,c]] -> [[1,a],[2,b],[3,c]]
zipn([[]|_]) ->[];
zipn(M) ->
    [lists:map(fun hd/1, M) | zipn(lists:map(fun tl/1, M))].

%% @private
%% Takes the input list and converts to a markdown table row
list_to_table_row(List) ->
    list_to_table_row(List, fun(A) -> A end).

%% Takes the input list and converts to a markdown table row
%% Applies Fun to each element while building the row
list_to_table_row(List, Fun) ->
    Tabled = lists:map(fun(E) -> ["|", Fun(E)] end, List),
    lists:flatten([Tabled, ["|\n"]]).

%% @private
%% Builds a github style shield showing the current status of a phase run
build_status_shield(Scope, #phase_run_summary{stage = Stage, phase = Phase, phase_status = Status}) ->
    [ChangeId, [Ent, Org, Proj, _Pipe]] = deliv_scopes:'#get'([change_id, scoping_names], Scope),
    CPhase = chef_utils:capitalize_str(Phase),
    CStatus = chef_utils:capitalize_str(Status),
    Color = shield_color_for_status(Status),

    TargetUrl = erlang:iolist_to_binary(
        [deliv_web_utils:make_web_url_for_change(Ent, Org, Proj, ChangeId), "/status/", Stage]),
    ImageUrl = erlang:iolist_to_binary(
        ["https://img.shields.io/badge/", CPhase, "-", CStatus, "-", Color, ".svg"]),

    lists:flatten(io_lib:format("<a href='~s', target='_blank'><img src='~s', alt='~s'></a>",
                  [TargetUrl, ImageUrl, CPhase])).

%% @private
%% @doc returns the correct badge color for given a delivery status.
shield_color_for_status(<<"passed">>) -> <<"brightgreen">>;
shield_color_for_status(<<"failed">>) -> <<"red">>;
shield_color_for_status(<<"idle">>) -> <<"lightgrey">>;
shield_color_for_status(<<"running">>) -> <<"blue">>;
shield_color_for_status(<<"skipped">>) -> <<"lightgrey">>;
shield_color_for_status(<<"skipping">>) -> <<"lightgrey">>.
