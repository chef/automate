-module(jobs_runner_json).

-include("jobs_types.hrl").

-export([
         to_json/1
        ]).

-spec to_json(list(jobs_runner()) | jobs_runner()) -> json().
to_json(#runner{id = Id,
                hostname = Hostname,
                health_status = HealthStatus,
                health_output = HealthOutput,
                job = Job,
                public_key = PubKey,
                os = Os,
                platform = Platform,
                platform_family = PlatformFamily,
                platform_version = PlatformVersion}) ->
    {[{<<"id">>, Id},
      {<<"hostname">>, Hostname},
      {<<"job">>, job_ejson(Job)},
      {<<"openssh_public_key">>, PubKey},
      {<<"health">>, health_ejson(HealthStatus, HealthOutput)},
      {<<"os">>, Os},
      {<<"platform">>, Platform},
      {<<"platform_family">>, PlatformFamily},
      {<<"platform_version">>, PlatformVersion}
    ]};
to_json(Runners) when is_list(Runners) ->
    [to_json(Runner) || Runner <- Runners].

-spec health_ejson(undefined | ok | error, undefined | binary()) -> json().
health_ejson(undefined, undefined) -> {[]};
health_ejson(HealthStatus, HealthOutput) ->
    {[{<<"status">>, chef_utils:to_bin(HealthStatus)},
      {<<"command_output">>, HealthOutput}]}.

-spec job_ejson(undefined | job() | health_job()) -> json().
job_ejson(undefined) -> {[]};
job_ejson(#health_job{}) -> {[]};
job_ejson(#job{deliv_change_info = #deliv_change_info{id = Id,
                                                      title = Title,
                                                      org = Org,
                                                      project = Project}}) ->
    {[{<<"id">>, Id},
      {<<"title">>, Title},
      {<<"org">>, Org},
      {<<"project">>, Project}
    ]}.
