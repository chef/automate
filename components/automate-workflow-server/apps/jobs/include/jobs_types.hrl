-include_lib("public_key/include/public_key.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-record(runner, {id                 :: binary(),
                 pid                :: pid(),
                 enterprise_name    :: binary(),
                 hostname           :: binary(),
                 os                 :: binary(),
                 platform           :: binary(),
                 platform_family    :: binary(),
                 platform_version   :: binary(),
                 health_status      :: ok | pending | error,
                 health_output      :: binary(),
                 job                :: job() | health_job(),
                 public_key         :: binary(),
                 private_key        :: binary()}).

-record(runner_properties, {
          os :: binary(),
          platform_family :: binary(),
          platform :: binary(),
          platform_version :: binary()
         }).

-record(job, {
          id                    :: binary(),
          command               :: binary(),
          job_criteria          :: deliv_ssh_job_criteria(),
          deliv_change_info     :: deliv_change_info(),
          deliv_ssh_job_pid     :: pid(),
          status                :: pending | running,
          started_running_at    :: calendar:datetime(),
          runner_pid            :: pid(),
          jobs_command_pid      :: pid()
         }).

-record(health_job, {}).

-type jobs_runner() :: #runner{}.

-type job() :: #job{}.

-type health_job() :: #health_job{}.

-type runner_properties() :: #runner_properties{}.

-define(ALL_ROLES, [<<"admin">>, <<"committer">>, <<"reviewer">>, <<"shipper">>, <<"observer">>]).

-define(RUNNER_LOOP_INTERVAL, 1000).
