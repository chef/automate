package postgres

var sqlMigrations = []sqlMigration{
	{
		desc: "base_tables",
		upSQL: `
BEGIN;

-- Workflows
--
-- Workflows coordinate a set of tasks. For example, a scan job is a
-- workflow that create and then waits for the completion of a number
-- of inspec scan tasks.

CREATE TABLE recurring_workflow_schedules (
    id BIGSERIAL PRIMARY KEY,

    name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    recurrence TEXT,
    enabled BOOLEAN,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_enqueued_at TIMESTAMPTZ,
    next_run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT say_my_name UNIQUE(name, workflow_name)
);

CREATE OR REPLACE FUNCTION update_recurring_workflow_parameters(
    _id BIGINT,
    _parameters BYTEA)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM recurring_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE recurring_workflow_schedules SET parameters = _parameters WHERE id = _id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION update_recurring_workflow_recurrence(
    _id BIGINT,
    _recurrence TEXT,
    _next_run_at TIMESTAMPTZ)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM recurring_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE recurring_workflow_schedules
    SET
        recurrence = _recurrence,
        next_run_at = _next_run_at WHERE id = _id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION update_recurring_workflow_enabled(
    _id BIGINT,
    _enabled BOOLEAN)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM recurring_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE recurring_workflow_schedules SET enabled = _enabled WHERE id = _id;
$$ LANGUAGE SQL;

CREATE TYPE workflow_instance_status AS ENUM('running', 'abandoned');

CREATE TABLE workflow_instances (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    payload BYTEA,
    start_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    enqueued_tasks INTEGER NOT NULL DEFAULT 0,
    completed_tasks INTEGER NOT NULL DEFAULT 0,
    status workflow_instance_status NOT NULL DEFAULT 'running',

    CONSTRAINT say_my_name1 UNIQUE(name, workflow_name)
);

CREATE TABLE workflow_results (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    start_at TIMESTAMPTZ NOT NULL,
    end_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    result BYTEA
);

-- Tasks
--
-- Tasks are units of discrete work that need to be done.
--
-- Workers dequeue tasks from the task table, do the work related to
-- that task, and then post their results back.
--
-- New and Completed tasks also notify a channel that can be
-- subscribed to by the workflows for more timely notification of new
-- task events related to their workflow.
CREATE TYPE task_status AS ENUM('success', 'failed');

CREATE TABLE tasks (
    id BIGSERIAL PRIMARY KEY,
    workflow_instance_id BIGINT NOT NULL,
    try_remaining INT NOT NULL DEFAULT 1,
    enqueued_at   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    start_after   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    task_name     TEXT NOT NULL,
    parameters    BYTEA
);

CREATE TABLE tasks_results (
    id BIGSERIAL PRIMARY KEY,
    workflow_instance_id BIGINT NOT NULL,
    parameters   BYTEA,
    task_name    TEXT NOT NULL,
    enqueued_at  TIMESTAMPTZ NOT NULL,
    completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    status       task_status,
    error        TEXT,
    result       BYTEA
);

CREATE TYPE workflow_event_type AS ENUM('start', 'task_complete', 'cancel', 'tasks_abandoned');

CREATE TABLE workflow_events (
    id BIGSERIAL PRIMARY KEY,
    event_type workflow_event_type NOT NULL,
    workflow_instance_id BIGINT,
    enqueued_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- task_complete members
    task_result_id BIGINT
);

-- Workflow Functions
CREATE OR REPLACE FUNCTION enqueue_workflow(
    name TEXT,
    workflow_name TEXT,
    parameters BYTEA)
RETURNS INTEGER
AS $$
    WITH winst AS (
        INSERT INTO workflow_instances(name, workflow_name, parameters)
            VALUES(name, workflow_name, parameters)
            ON CONFLICT DO NOTHING
            RETURNING id
        )
    INSERT INTO workflow_events(event_type, workflow_instance_id)
    (SELECT 'start', id FROM winst WHERE id IS NOT NULL)
    RETURNING 1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION dequeue_workflow(VARIADIC workflow_names TEXT[])
RETURNS TABLE(workflow_instance_id BIGINT, instance_name TEXT, workflow_name TEXT,
    status workflow_instance_status, parameters BYTEA, payload BYTEA, event_id BIGINT,
    event_type workflow_event_type, task_result_id BIGINT, enqueued_tasks INTEGER,
    completed_tasks INTEGER)
AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks
        FROM workflow_instances a
        INNER JOIN workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cancel_workflow(workflow_instance_id BIGINT)
RETURNS VOID
AS $$
    INSERT INTO workflow_events(event_type, workflow_instance_id)
        VALUES('cancel', workflow_instance_id);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION abandon_workflow(_workflow_instance_id BIGINT, eid BIGINT, _completed_tasks INTEGER)
RETURNS VOID
AS $$
    WITH unclaimed_tasks AS (
        SELECT id FROM tasks
        WHERE workflow_instance_id = _workflow_instance_id
        FOR UPDATE SKIP LOCKED
    ), deleted_tasks AS (
        DELETE FROM tasks WHERE id IN (SELECT id from unclaimed_tasks)
    )
    UPDATE workflow_instances w1 SET updated_at = NOW(), status = 'abandoned',
        completed_tasks = _completed_tasks + (select count(*) from unclaimed_tasks);

    DELETE FROM workflow_events WHERE id = eid;

    INSERT INTO workflow_events(event_type, workflow_instance_id)
        VALUES('tasks_abandoned', _workflow_instance_id);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION complete_workflow(wid BIGINT)
RETURNS VOID
LANGUAGE SQL
AS $$
    WITH 
    done_workflows AS (
        SELECT id, name, workflow_name, parameters, start_at 
        FROM workflow_instances where id = wid
    )
    INSERT INTO workflow_results(name, workflow_name, parameters, start_at)
        (SELECT name, workflow_name, parameters, start_at FROM done_workflows);
    
    DELETE FROM tasks WHERE workflow_instance_id = wid;
    DELETE FROM tasks_results WHERE workflow_instance_id = wid;
    DELETE FROM workflow_events WHERE workflow_instance_id = wid;
    DELETE FROM workflow_instances WHERE id = wid;
$$;

CREATE OR REPLACE FUNCTION continue_workflow(wid BIGINT, eid BIGINT, _payload BYTEA,
    _enqueued_tasks INTEGER, _completed_tasks INTEGER)
RETURNS VOID
LANGUAGE SQL
AS $$
    UPDATE workflow_instances SET updated_at = NOW(), payload = _payload,
        enqueued_tasks = _enqueued_tasks, completed_tasks = _completed_tasks WHERE id = wid;
    -- We've decided there is more to do but are done processing this event.
    DELETE FROM workflow_events WHERE id = eid
$$;

-- Task Functions
CREATE OR REPLACE FUNCTION enqueue_task(
    workflow_instance_id BIGINT,
    try_remaining INT,
    start_after TIMESTAMPTZ,
    task_name TEXT,
    parameters BYTEA)
RETURNS VOID
AS $$
    INSERT INTO tasks(workflow_instance_id, try_remaining, start_after, task_name, parameters)
        VALUES(workflow_instance_id, try_remaining, start_after, task_name, parameters);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION dequeue_task(task_name TEXT)
RETURNS TABLE(id BIGINT, workflow_instance_id BIGINT, parameters BYTEA)
AS $$
    UPDATE tasks t1 SET try_remaining = try_remaining - 1, updated_at = NOW()
    WHERE t1.id = (
        SELECT t2.id FROM tasks t2
        WHERE t2.task_name = task_name AND t2.try_remaining > 0 AND start_after < NOW()
        ORDER BY t2.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ) RETURNING t1.id, t1.workflow_instance_id, t1.parameters
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION complete_task(tid BIGINT, status task_status, error text, result BYTEA)
RETURNS VOID
LANGUAGE SQL
AS $$
    WITH in_vals AS (SELECT
        tid as id,
        status as status,
        error as error,
        result as result),
    tres AS (
        INSERT INTO tasks_results(workflow_instance_id, parameters, task_name, enqueued_at, status, error, result)
            (SELECT workflow_instance_id, parameters, task_name, enqueued_at, in_vals.status, in_vals.error, in_vals.result
            FROM tasks JOIN in_vals ON in_vals.id = tasks.id WHERE tasks.id = tid) RETURNING id, workflow_instance_id
    )
    INSERT INTO workflow_events(event_type, task_result_id, workflow_instance_id)
        VALUES('task_complete', (select id from tres), (select workflow_instance_id from tres));
    ;
    DELETE FROM tasks WHERE id = tid
$$;

COMMIT;
`,
	},
}
