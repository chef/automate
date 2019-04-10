%% In order to feasibly write unit tests for deliv_phase, its state record
%% definition has been extracted to this HRL. If/when it becomes possible to run
%% embedded eunit tests in our development tooling, those tests and this record
%% can be merged back into the deliv_phase module.
-include("deliv_types.hrl").
-include("deliv_events.hrl").

%% The FSM state record
-record(deliv_phase, {
        %% core data, needed to create the change
        change               :: d_change(),
        patchset             :: d_patchset(),
        stage_run            :: d_stage_run(),
        stage_run_pid        :: pid(),
        job_dispatch_version :: binary(),
        timeout              :: integer() | undefined,
        criteria             :: deliv_ssh_job_criteria(),

        %% internal data, created as we go
        phase_run       :: d_phase_run(),
        phase_job_pid   :: pid(),
        job_status      :: binary(),

        %% ent/org/proj/pipe names
        scoping_names   :: [binary()]
    }).

%% Record to reflect the current state of the phase job
-record(phase_job_status, {
        started  :: boolean(),
        node     :: binary(),
        status   :: binary()
    }).

-define(PHASES, [unit, lint, syntax, quality, security,
                 publish, provision, deploy, smoke, functional]).
