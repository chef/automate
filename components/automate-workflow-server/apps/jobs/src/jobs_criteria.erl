-module(jobs_criteria).

-include("jobs_types.hrl").

-export([
         match/2
        ]).

-spec match(deliv_ssh_job_criteria(), runner_properties()) -> boolean().
match(#deliv_ssh_job_criteria{os = OsList,
                              platform_family = PlatformFamilyList,
                              platform = PlatformList,
                              platform_version = PlatformVersionList},
      #runner_properties{os = Os,
                         platform_family = PlatformFamily,
                         platform = Platform,
                         platform_version = PlatformVersion}) ->
    lists:all(fun subset_values/1,
              [{Os, OsList},
               {PlatformFamily, PlatformFamilyList},
               {Platform, PlatformList},
               {PlatformVersion, PlatformVersionList}]).

%% If any single criteria from deliv_ssh_job_criteria is not specified, it
%% will come back as undefined. We should always match on that criteria because
%% that means the user didn't specify any constraint on that criteria for this job.
subset_values({_RunnerProperty, undefined}) ->
    true;
subset_values({RunnerProperty, JobCriterium}) ->
    lists:member(RunnerProperty, JobCriterium).
