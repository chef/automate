%% In order to feasibly write unit tests for deliv_stage, its state record
%% definition has been extracted to this HRL. If/when it becomes possible to run
%% embedded eunit tests in our development tooling, those tests and this record
%% can be merged back into the deliv_stage module.
-include("deliv_types.hrl").

-type phase_group() :: atom() | [atom()].
-type child_event() :: phase_finished | child_stage_finished.
-type child_status() :: failed | not_failed_yet | passed.

%% The FSM state record
-record(deliv_stage, {
        %% core data, needed to create the stage
        change                :: d_change(),

        %% keep track of nested stages
        parent_stage_pid         :: pid(),
        child_stage_pids         :: [pid()],
        consumer_change_ids = [] :: [binary()],

        %% internal data, created as we go
        patchset              :: d_patchset(),
        stage_run             :: d_stage_run(),
        stage_name            :: deliv_stage_name(),
        next_phase_groups     :: [phase_group()],
        %% a list of phases to skip
        skip_phase_groups     :: [binary()],
        %% a list of the current running phases' PIDs
        running_phases        :: [pid()],
        project_config        :: tuple(),
        failed=not_failed_yet :: not_failed_yet | failed,
        %% ent/org/proj/pipe names
        scoping_names         :: [binary()],
        scope                 :: d_common_scope(),

        failed_pipelines = [] :: [non_neg_integer()],
        passed_pipelines = [] :: [non_neg_integer()]
    }).

%% The name of the app config param where to store the phases' names
-define(STAGES_DATA_CONFIG_KEY, stages_data).

-define(STAGES_DATA,
    [
        {verify, {[[unit, lint, syntax]], none}},
        {build, {[[unit, lint, syntax], [quality, security], publish], acceptance}},
        {acceptance, {[provision, deploy, smoke, functional], none}},
        {union, {[provision, deploy, smoke, functional], rehearsal}},
        {rehearsal, {[provision, deploy, smoke, functional], delivered}},
        {delivered, {[provision, deploy, smoke, functional], none}}
    ]
).
