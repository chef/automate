%% @doc Generate base64 encoded command binary for use with launching delivery phase jobs.
-module(deliv_job_command).

-include("deliv_phase.hrl").
-include("deliverance_types.hrl").

-export([
         command/6,
         generate_id/3
        ]).

%% @private
%% @doc Creates the delivery-cmd command to run the job
-spec command(d_stage_run(),
              d_phase_run(),
              d_change(),
              d_patchset(),
              binary(),
              integer() | undefined) -> binary().
command(StageRun, PhaseRun, Change, Patchset, DispatchVersion, Timeout) ->
    Scope = deliv_scopes:from_change(Change),
    ScmMod = deliv_scopes:'#get'(scm_module, Scope),

    GitUrl = ScmMod:clone_url(?BUILDER_NAME, Patchset, Scope,
                               delivery_app:get_env(deliv_fips_mode, false)),

    ChangeId = deliv_change:getval(id, Change),
    [EnterpriseName, OrganizationName, ProjectName, PipelineName] = deliv_change:scoping_names(ChangeId),
    ChangeURL = deliv_web_utils:make_web_url_for_change(EnterpriseName,
                                                        OrganizationName,
                                                        ProjectName,
                                                        ChangeId),

    %% We're matching on the happy-path case because there will always be a "builder" user;
    %% when there's no longer a builder user, this code won't work anyway :)
    {ok, BuilderToken} = deliv_token:assign_token(EnterpriseName, ?BUILDER_NAME),

    APIUrl = deliv_web_utils:make_api_url_prefix(),

    {Sha, PatchsetBranch} = case deliv_change:is_merged(Change) of
        false ->
            {deliv_patchset:getval(sha, Patchset), ScmMod:patchset_branch(Change, Patchset, Scope)};
        true ->
            {deliv_change:getval(merge_sha, Change), null}
    end,

    Json = chef_json:encode({[{<<"enterprise">>, EnterpriseName},
                               {<<"organization">>, OrganizationName},
                               {<<"project">>, ProjectName},
                               {<<"pipeline">>, PipelineName},
                               {<<"change_id">>, ChangeId},
                               {<<"patchset_number">>, deliv_patchset:getval(sequence_number, Patchset)},
                               {<<"stage">>, deliv_stage_run:getval(stage, StageRun)},
                               {<<"stage_run_id">>, deliv_stage_run:getval(id, StageRun)},
                               {<<"phase">>, deliv_phase_run:getval(phase, PhaseRun)},
                               {<<"phase_run_id">>, deliv_phase_run:getval(id, PhaseRun)},
                               {<<"git_url">>, GitUrl},
                               {<<"sha">>, Sha},
                               {<<"fips">>, chef_utils:to_bin(delivery_app:get_env(deliv_fips_mode, false))},
                               {<<"fips_git_port">>, delivery_app:get_env(deliv_fips_stunnel_proxy_port)},
                               {<<"a2_mode">>, delivery_app:get_env(a2_mode, false)},
                               {<<"patchset_branch">>, PatchsetBranch},
                               {<<"delivery_api_url">>, APIUrl},
                               {<<"delivery_data_url">>, APIUrl},
                               {<<"delivery_change_url">>, ChangeURL},
                               %% TODO: shouldn't the loglevel come from the project's config instead?
                               {<<"log_level">>, <<"info">>},
                               {<<"token">>, BuilderToken},
                               {<<"dispatch_version">>, DispatchVersion},
                               {<<"timeout">>, Timeout}]}),

    Base64 = base64:encode(Json),
    <<"delivery-cmd ", Base64/binary>>.

%% This can be generated on the command line with
%%     echo -n $stage_run_id$phase_run_id$change_id | shasum -a 256
-spec generate_id(db_id(), db_id(), db_guid()) -> binary().
generate_id(StageRunId, PhaseRunId, ChangeId) ->
    <<Key:256/big-unsigned-integer>> = crypto:hash(sha256,
                                                   <<(chef_utils:to_bin(StageRunId))/binary,
                                                     (chef_utils:to_bin(PhaseRunId))/binary,
                                                     ChangeId/binary>>),
    chef_utils:to_bin("~64.16.0b", [Key]).
