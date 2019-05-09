BEGIN;

CREATE TABLE recurring_workflow_schedules (
    id BIGSERIAL PRIMARY KEY,

    name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,

    recurrence TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    next_due TIMESTAMP NOT NULL DEFAULT NOW(),
    parameters JSON,

    CONSTRAINT say_my_name UNIQUE(name, workflow_name)
);

CREATE TABLE workflow_instances (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    workflow_name TEXT NOT NULL,
    parameters JSON,
    payload JSON,

    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),

    CONSTRAINT say_my_name1 UNIQUE(name, workflow_name)
);

CREATE TYPE task_status AS ENUM('success', 'failed', 'abandoned');

CREATE TABLE tasks (
    id BIGSERIAL PRIMARY KEY,
    workflow_instance_id BIGINT NOT NULL REFERENCES workflow_instances(id) ON DELETE CASCADE,
    try_remaining INT NOT NULL DEFAULT 1,
    enqueued_at   TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMP NOT NULL DEFAULT NOW(),
    start_after   TIMESTAMP NOT NULL DEFAULT NOW(),
    task_name     TEXT NOT NULL,
    parameters    JSON
);

CREATE TABLE tasks_results (
    id BIGSERIAL PRIMARY KEY,
    workflow_instance_id BIGINT NOT NULL REFERENCES workflow_instances(id) ON DELETE CASCADE,
    parameters   JSON,
    task_name    TEXT NOT NULL,
    enqueued_at  TIMESTAMP NOT NULL,
    completed_at TIMESTAMP NOT NULL DEFAULT NOW(),
    status       task_status,
    error        TEXT,
    result       JSON
);

CREATE TYPE workflow_event_type AS ENUM('start', 'task_complete', 'cancel');

CREATE TABLE workflow_events (
    id BIGSERIAL PRIMARY KEY,
    event_type workflow_event_type NOT NULL,
    workflow_instance_id BIGINT NOT NULL REFERENCES workflow_instances(id) ON DELETE CASCADE,
    enqueued_at TIMESTAMP NOT NULL DEFAULT NOW(),
    -- task_complete members
    task_result_id BIGINT REFERENCES tasks_results(id)
);

-- 

CREATE OR REPLACE FUNCTION enqueue_workflow(
    name TEXT,
    workflow_name TEXT,
    parameters JSON)
RETURNS VOID
AS $$
    WITH winst AS (
        INSERT INTO workflow_instances(name, workflow_name, parameters)
            VALUES(name, workflow_name, parameters)
            RETURNING id
        )
    INSERT INTO workflow_events(event_type, workflow_instance_id) 
        VALUES('start', (select id from winst));
    SELECT pg_notify('workflow_instance_new', workflow_name);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION dequeue_workflow(VARIADIC workflow_names TEXT[])
RETURNS TABLE(workflow_instance_id BIGINT, instance_name TEXT, workflow_name TEXT, 
    parameters JSON, payload JSON, event_id BIGINT, event_type workflow_event_type,
    task_result_id BIGINT)
AS $$
    WITH nextwinst AS (
        SELECT 
            a.id id,
            a.name instance_name,
            a.workflow_name workflow_name, 
            a.parameters parameters, 
            a.payload payload, 
            b.id event_id, 
            b.event_type event_type, 
            b.task_result_id task_result_id
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
    -- TODO: notify
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION complete_workflow(workflow_instance_id BIGINT)
RETURNS VOID
AS $$
    DELETE FROM workflow_instances WHERE id=workflow_instance_id;
$$ LANGUAGE SQL;

-- Notification channels
--
-- workflow_task_new
-- workflow_task_complete
--
-- (jaym): I think we can just have 1 notification channel for workflow events

-- https://www.postgresql.org/docs/current/xfunc-sql.html
CREATE OR REPLACE FUNCTION enqueue_task(
    workflow_instance_id BIGINT,
    try_remaining INT,
    start_after TIMESTAMP,
    task_name TEXT,
    parameters JSON)
RETURNS VOID
AS $$
    INSERT INTO tasks(workflow_instance_id, try_remaining, start_after, task_name, parameters)
        VALUES(workflow_instance_id, try_remaining, start_after, task_name, parameters);
    SELECT pg_notify('workflow_task_new', task_name);
$$ LANGUAGE SQL;

--https://stackoverflow.com/questions/16609724/using-current-time-in-utc-as-default-value-in-postgresql
CREATE OR REPLACE FUNCTION dequeue_task(task_name TEXT)
RETURNS TABLE(id BIGINT, workflow_instance_id BIGINT, parameters JSON)
AS $$
    UPDATE tasks t1 SET try_remaining = try_remaining - 1, updated_at = NOW()
    WHERE t1.id = (
        SELECT t2.id FROM tasks t2
        WHERE t2.try_remaining > 0 AND start_after < NOW()
        ORDER BY t2.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ) RETURNING t1.id, t1.workflow_instance_id, t1.parameters
$$ LANGUAGE SQL;

-- https://www.postgresql.org/docs/9.1/queries-with.html
-- https://stackoverflow.com/questions/6560447/can-i-use-return-value-of-insert-returning-in-another-insert#6561437
CREATE OR REPLACE FUNCTION complete_task(tid BIGINT, status task_status, error text, result JSON)
RETURNS VOID
LANGUAGE SQL
AS $$
    WITH done_tasks AS (SELECT workflow_instance_id AS id FROM tasks where id = tid)
    SELECT pg_notify('workflow_task_complete', id::text) FROM done_tasks;
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
