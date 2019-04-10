%% Transforms stage_run_summary records into HAL links.

-module(deliv_stage_run_summary).

-include("deliv_types.hrl").

-export([
         to_hal/6
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HAL from Stage Run Summary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc produces hal links form the stage run summary. Each function head
%% matches a paricular case where hal should be returned. It also checks that
%% the user is authorized.
%%
%% The underlying query that produces the list of stage_run_summaries sorts
%% them in decending order by stage run id. This ensures that the head of the
%% list always has the latest stage_run_summary for the the latests stage so
%% patern matching can always grab the head of the list. In the case of
%% acceptance we expect only one since it is the first stage.
-spec to_hal([#stage_run_summary{}], binary(), {binary(), binary(), binary()},
    binary(), binary(), binary()) -> json().
to_hal([#stage_run_summary{stage = <<"verify">>, status = Status, finished = true}],
        <<"open">>, RepoCoordinates, Pipeline, UserName, ChangeId) ->
    {EntName, OrgName, ProjName} = RepoCoordinates,
    Hal = {[]},
    Hal2 = case {Status, deliv_change_auth:authorized_change_action(EntName,
            OrgName, ProjName, Pipeline, UserName, trigger_verify)} of
        {<<"failed">>, allow} ->
            Url = deliv_web_utils:relative_href_for(trigger_stage, [EntName, OrgName,
                ProjName, ChangeId, <<"verify">>]),
            ej:set_p([<<"trigger-verify">>, <<"href">>], Hal, Url);
         _ -> Hal
    end,

    Hal3 = case {Status, deliv_change_auth:authorized_change_action(EntName,
            OrgName, ProjName, Pipeline, UserName, delete)} of
        {_, allow} ->
            Url2 = deliv_web_utils:relative_href_for(delete_change, [EntName, OrgName,
                ProjName, ChangeId]),
            ej:set_p([<<"delete">>, <<"href">>], Hal2, Url2);
         _ -> Hal2
    end,

    case {Status, deliv_change_auth:authorized_change_action(EntName, OrgName,
            ProjName, Pipeline, UserName, approve)} of
        {<<"passed">>, allow} ->
            Url3 = deliv_web_utils:relative_href_for(merge_change, [EntName, OrgName,
                ProjName, ChangeId]),
            ej:set_p([<<"approve">>, <<"href">>], Hal3, Url3);
        _ -> Hal3
    end;
to_hal([#stage_run_summary{stage = <<"build">>, status = <<"failed">>,
        finished = true, pipeline_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(trigger_build, trigger_stage, ["build"],
        <<"trigger-build">>, RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal([#stage_run_summary{stage = <<"acceptance">>, status = <<"failed">>,
        finished = true, pipeline_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(trigger_acceptance, trigger_stage, [<<"acceptance">>],
        <<"trigger-acceptance">>, RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal([#stage_run_summary{stage = <<"acceptance">>, status = <<"passed">>,
        finished = true, pipeline_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(deliver, deliver_change, [], <<"deliver">>,
        RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal([#stage_run_summary{stage = <<"union">>, status = <<"failed">>,
        finished = true, system_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(trigger_union, trigger_stage, [<<"union">>],
        <<"trigger-union">>, RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal([#stage_run_summary{stage = <<"rehearsal">>, status = <<"failed">>,
        finished = true, system_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(trigger_rehearsal, trigger_stage, [<<"rehearsal">>],
        <<"trigger-rehearsal">>, RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal([#stage_run_summary{stage = <<"delivered">>, status = <<"failed">>,
        finished = true, system_latest = true} | _], <<"merged">>,
        RepoCoordinates, Pipeline, UserName, ChangeId) ->
    hal_for_change_action(trigger_delivered, trigger_stage, [<<"delivered">>],
        <<"trigger-delivered">>, RepoCoordinates, Pipeline, UserName, ChangeId);
to_hal(_, _, _, _, _, _) ->
    %% If we don't match above return no HAL.
    {[]}.

-spec hal_for_change_action(change_action(), atom(), [binary()], binary(),
    {binary(), binary(), binary()}, binary(), binary(), binary()) -> json().
hal_for_change_action(ChangeAction, URL_Key, URL_Parms, HAL_Key, RepoCoordinates,
        Pipeline, UserName, ChangeId) ->
    {EntName, OrgName, ProjName} = RepoCoordinates,
    case deliv_change_auth:authorized_change_action(EntName, OrgName, ProjName,
            Pipeline, UserName, ChangeAction) of
        allow ->
            Url = deliv_web_utils:relative_href_for(URL_Key, [EntName, OrgName,
                ProjName, ChangeId | URL_Parms]),
            {[{HAL_Key, {[{<<"href">>, Url}]}}]};
        forbid ->
            {[]}
    end.
