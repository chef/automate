package postgres

var sqlMigrations = []sqlMigration{
	{
		desc: "base_tables",
		upSQL: `
BEGIN;

-- Workflows
--
-- Workflows coordinate a set of tasks.
--
CREATE TABLE cereal_workflow_schedules (
    id BIGSERIAL PRIMARY KEY,

    instance_name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    recurrence TEXT,
    enabled BOOLEAN,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_enqueued_at TIMESTAMPTZ,
    next_run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT cereal_workflow_schedules_name UNIQUE(instance_name, workflow_name)
);

CREATE OR REPLACE FUNCTION cereal_update_workflow_schedule_parameters(
    _id BIGINT,
    _parameters BYTEA)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET parameters = _parameters WHERE id = _id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cereal_update_workflow_schedule_recurrence(
    _id BIGINT,
    _recurrence TEXT,
    _next_run_at TIMESTAMPTZ)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules
    SET
        recurrence = _recurrence,
        next_run_at = _next_run_at WHERE id = _id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cereal_update_workflow_schedule_enabled(
    _id BIGINT,
    _enabled BOOLEAN)
RETURNS VOID
AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET enabled = _enabled WHERE id = _id;
$$ LANGUAGE SQL;

CREATE TYPE cereal_workflow_instance_status AS ENUM('starting', 'running');

CREATE TABLE cereal_workflow_instances (
    id BIGSERIAL PRIMARY KEY,
    instance_name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    payload BYTEA,
    start_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    enqueued_tasks INTEGER NOT NULL DEFAULT 0,
    completed_tasks INTEGER NOT NULL DEFAULT 0,
    status cereal_workflow_instance_status NOT NULL DEFAULT 'starting',

    CONSTRAINT cereal_workflow_instances_name UNIQUE(instance_name, workflow_name)
);

CREATE TABLE cereal_workflow_results (
    id BIGSERIAL PRIMARY KEY,
    instance_name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters BYTEA,
    start_at TIMESTAMPTZ NOT NULL,
    end_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    error  TEXT,
    result BYTEA
);

-- Tasks
--
-- Tasks are units of discrete work that need to be done.
--
-- Workers dequeue tasks from the task table, do the work related to
-- that task, and then post their results back.
--
CREATE TYPE cereal_task_status AS ENUM('success', 'failed', 'lost');

CREATE TYPE cereal_task_state  AS ENUM('queued', 'running');

CREATE TABLE cereal_tasks (
    id BIGSERIAL PRIMARY KEY,
    workflow_instance_id BIGINT NOT NULL,
    enqueued_at   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    start_after   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    task_name     TEXT NOT NULL,
    parameters    BYTEA,
    task_state    cereal_task_state NOT NULL DEFAULT 'queued'
);

CREATE TABLE cereal_task_results (
    id BIGSERIAL PRIMARY KEY,
    task_id BIGINT UNIQUE NOT NULL,
    workflow_instance_id BIGINT NOT NULL,
    parameters   BYTEA,
    task_name    TEXT NOT NULL,
    enqueued_at  TIMESTAMPTZ NOT NULL,
    completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    status       cereal_task_status,
    error        TEXT DEFAULT '',
    result       BYTEA
);

CREATE TYPE cereal_workflow_event_type AS ENUM('start', 'task_complete', 'cancel');

CREATE TABLE cereal_workflow_events (
    id BIGSERIAL PRIMARY KEY,
    event_type cereal_workflow_event_type NOT NULL,
    workflow_instance_id BIGINT,
    enqueued_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- task_complete members
    task_result_id BIGINT
);

-- Workflow Functions
CREATE OR REPLACE FUNCTION cereal_enqueue_workflow(
    _instance_name TEXT,
    _workflow_name TEXT,
    _parameters BYTEA)
RETURNS INTEGER
AS $$
    WITH winst AS (
        INSERT INTO cereal_workflow_instances(instance_name, workflow_name, parameters)
            VALUES(_instance_name, _workflow_name, _parameters)
            ON CONFLICT DO NOTHING
            RETURNING id
        )
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
    (SELECT 'start', id FROM winst WHERE id IS NOT NULL)
    RETURNING 1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cereal_dequeue_workflow(VARIADIC _workflow_names TEXT[])
RETURNS TABLE(workflow_instance_id BIGINT, instance_name TEXT, workflow_name TEXT,
    status cereal_workflow_instance_status, parameters BYTEA, payload BYTEA, event_id BIGINT,
    event_type cereal_workflow_event_type, task_result_id BIGINT, enqueued_tasks INTEGER,
    completed_tasks INTEGER)
AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.instance_name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks
        FROM cereal_workflow_instances a
        INNER JOIN cereal_workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(_workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE cereal_workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cereal_cancel_workflow(_instance_name TEXT, _workflow_name TEXT)
RETURNS BOOLEAN
AS $$
BEGIN
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
        (SELECT 'cancel', id FROM cereal_workflow_instances WHERE instance_name = _instance_name
            AND workflow_name = _workflow_name);
    RETURN FOUND;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION cereal_complete_workflow(_wid BIGINT, _result BYTEA)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, result)
        (SELECT instance_name, workflow_name, parameters, start_at, _result FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;

CREATE OR REPLACE FUNCTION cereal_fail_workflow(_wid BIGINT, _error TEXT)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, error)
        (SELECT instance_name, workflow_name, parameters, start_at, _error FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;

CREATE OR REPLACE FUNCTION cereal_workflow_clean_workflow_results(_max_size BIGINT, _margin BIGINT)
RETURNS BIGINT
LANGUAGE plpgsql
AS $$
DECLARE
    count BIGINT;
BEGIN
SELECT COUNT(*) FROM cereal_workflow_results INTO count;
IF count > _max_size THEN
    DELETE FROM cereal_workflow_results
    WHERE id IN (
        SELECT id FROM cereal_workflow_results
        ORDER BY end_at DESC OFFSET _max_size - _margin
    );
    GET DIAGNOSTICS count = ROW_COUNT;
    RETURN count;
ELSE
    RETURN 0;
END IF;

END
$$;

CREATE OR REPLACE FUNCTION cereal_continue_workflow(wid BIGINT, _eid BIGINT, _payload BYTEA,
    _enqueued_tasks INTEGER, _completed_tasks INTEGER)
RETURNS VOID
LANGUAGE SQL
AS $$
    UPDATE cereal_workflow_instances SET updated_at = NOW(), payload = _payload, status = 'running',
        enqueued_tasks = _enqueued_tasks, completed_tasks = _completed_tasks WHERE id = wid;
    -- We've decided there is more to do but are done processing this event.
    DELETE FROM cereal_workflow_events WHERE id = _eid
$$;

-- Task Functions
CREATE OR REPLACE FUNCTION cereal_enqueue_task(
    _workflow_instance_id BIGINT,
    _start_after TIMESTAMPTZ,
    _task_name TEXT,
    _parameters BYTEA)
RETURNS VOID
AS $$
    INSERT INTO cereal_tasks(workflow_instance_id, start_after, task_name, parameters)
        VALUES(_workflow_instance_id, _start_after, _task_name, _parameters);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cereal_dequeue_task(_task_name TEXT)
RETURNS TABLE(id BIGINT, parameters BYTEA)
AS $$
DECLARE
    r cereal_tasks%rowtype;
BEGIN
    FOR r IN
        SELECT * FROM cereal_tasks
        WHERE task_name = _task_name AND task_state = 'queued' AND start_after < NOW()
        ORDER BY enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    LOOP
        UPDATE cereal_tasks SET task_state = 'running', updated_at = NOW() WHERE cereal_tasks.id = r.id;

        id := r.id;
        parameters := r.parameters;
        RETURN NEXT;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION cereal_ping_task(_task_id BIGINT)
RETURNS VOID
AS $$
BEGIN
    UPDATE cereal_tasks SET updated_at = NOW() WHERE id = _task_id;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION cereal_expire_tasks(_task_timeout_seconds BIGINT)
RETURNS TABLE(tid BIGINT, workflow_instance_id BIGINT)
AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
        SELECT *
        FROM cereal_tasks
        WHERE
            task_state = 'running' AND
            updated_at < NOW() - (_task_timeout_seconds || ' seconds')::interval
        FOR UPDATE SKIP LOCKED
    LOOP
        DELETE FROM cereal_tasks WHERE id = t.id;

        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status)
            VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, 'lost')
            RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
            VALUES('task_complete', task_results_id, t.workflow_instance_id);

        tid := t.id;
        workflow_instance_id := t.workflow_instance_id;

        RETURN NEXT;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION cereal_complete_task(_tid BIGINT, _status cereal_task_status, _error text, _result BYTEA)
RETURNS SETOF BIGINT
LANGUAGE plpgsql
AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
         SELECT * FROM cereal_tasks WHERE id = _tid FOR UPDATE
    LOOP
        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status, error, result)
        VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, _status, _error, _result)
        RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
        VALUES('task_complete', task_results_id, t.workflow_instance_id);

        DELETE FROM cereal_tasks WHERE id = t.id;

        RETURN NEXT t.id;
    END LOOP;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$;

COMMIT;
`,
	},
}
