package postgres

import (
	"context"
	"database/sql"
	"time"

	"github.com/golang-migrate/migrate"
	"github.com/golang-migrate/migrate/database/postgres"
	"github.com/golang-migrate/migrate/source"
	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

const (
	// defaultMaxIdleConnections is used to configure
	// MaxIdleConnections on the sql.DB object. 4 was chosen
	// without much analysis. The default is 2. Without any
	// registered task executors we know we have the following
	// periodic database accesses:
	//
	// - cleanup goroutine
	// - workflow processing loop
	// - workflow scheduler loop
	//
	defaultMaxIdleConnections = 4

	enqueueTaskQuery       = `SELECT cereal_enqueue_task($1, $2, $3, $4)`
	dequeueTaskQuery       = `SELECT * FROM cereal_dequeue_task($1)`
	completeTaskQuery      = `SELECT cereal_complete_task($1, $2, $3, $4)`
	consumeTaskResultQuery = `DELETE FROM cereal_task_results WHERE id = $1 RETURNING task_name, parameters, status, error, result`

	enqueueWorkflowQuery  = `SELECT cereal_enqueue_workflow($1, $2, $3)`
	dequeueWorkflowQuery  = `SELECT * FROM cereal_dequeue_workflow(VARIADIC $1)`
	cancelWorkflowQuery   = `SELECT cereal_cancel_workflow($1, $2)`
	killWorkflowQuery     = `SELECT cereal_kill_workflow($1, $2, $3)`
	completeWorkflowQuery = `SELECT cereal_complete_workflow($1, $2)`
	failWorkflowQuery     = `SELECT cereal_fail_workflow($1, $2)`
	continueWorkflowQuery = `SELECT cereal_continue_workflow($1, $2, $3, $4, $5)`

	// listWorkflowInstanceQuery is quite a tricky query. It's intent is to select
	// one run of each workflow instance. This means it has to consult the actively
	// running workflow instances from the cereal_workflow_instances table, and the
	// completed workflow results from cereal_workflow_results.
	// Parameters:
	// $1 : workflow_name OR NULL: only workflow_names of the given value will match.
	//                             all will match on NULL
	// $2 : instance_name OR NULL: only instance_names of the given value will match.
	//                             all will match on NULL
	// $3 : is_running OR NULL: if true, only running instances will match. if false,
	//                          only completed instances will match. if NULL, all will
	//                          match.
	listWorkflowInstancesQuery = `
-- Find at most one of each (instance_name, workflow_name) from results,
-- picking at the most recent result if multiple exist
WITH res AS (
	SELECT DISTINCT ON (instance_name, workflow_name) *
    FROM cereal_workflow_results
	ORDER BY instance_name, workflow_name, end_at DESC
)
-- Select the workflow instance data, giving priority to that coming
-- from the cereal_workflow_instances table
SELECT COALESCE(a.id, res.id) id,
       COALESCE(a.status, 'completed') status,
       COALESCE(a.instance_name, res.instance_name) instance_name,
	   COALESCE(a.workflow_name, res.workflow_name) workflow_name,
	   COALESCE(a.parameters, res.parameters) parameters,
	   a.payload payload,
	   -- The result, and error only in the cereal_workflow_results table.
	   -- This means that these could potentially be garbage if the previous
	   -- data (id, status, etc) came from the cereal_workflow_instances.
	   -- This means we need to clean this up after the query
	   res.result result,
	   res.error error
-- We do a full outer join on the two tables on (workflow_name, instance_name).
-- This gives us 1 row for every distinct (workflow_name, instance_name) pair.
FROM cereal_workflow_instances a
FULL OUTER JOIN res ON a.workflow_name = res.workflow_name AND a.instance_name = res.instance_name
WHERE
	-- NOTE: The WHERE clause is processed after the join.
	-- The pattern below allows us to either specify a contraint by giving a value for the column,
	-- or passing in NULL. If a value is given for a column, we make sure the value matches either
	-- of the cereal_workflow_instances table or the cereal_workflow_results table. Passing in NULL
	-- for the value reduces down into saying the value in the column matches the value in the column.
	(a.workflow_name = COALESCE($1, a.workflow_name) OR
	res.workflow_name = COALESCE($1, res.workflow_name)) AND
	(a.instance_name = COALESCE($2, a.instance_name) OR
	res.instance_name = COALESCE($2, res.instance_name)) AND
	-- We'll select the row if is_running is now specified
	(($3::boolean IS NULL) OR
	-- If is_running is true, there must be a status in the cereal_workflow_instances
	-- table.
	 ($3::boolean IS NOT NULL AND $3::boolean AND a.status IS NOT NULL) OR
	-- If is_running is false, there must not be a status
	 ($3::boolean IS NOT NULL AND NOT $3::boolean AND a.status IS NULL));
	`
	listScheduledWorkflowsQuery = `
WITH res AS (
    SELECT DISTINCT ON (instance_name, workflow_name) *
    FROM cereal_workflow_results
    ORDER BY instance_name, workflow_name, end_at DESC
)
SELECT s.id, enabled,
    s.instance_name, s.workflow_name, s.parameters, recurrence, s.last_enqueued_at,
    next_run_at, start_at last_start, end_at last_end
FROM
    cereal_workflow_schedules s
    LEFT JOIN res ON s.instance_name = res.instance_name
        AND s.workflow_name = res.workflow_name`

	getScheduledWorkflowQuery = `
WITH res AS (
    SELECT DISTINCT ON (instance_name, workflow_name) *
    FROM cereal_workflow_results
    ORDER BY workflow_name, instance_name, end_at DESC
)
SELECT s.id, enabled, s.instance_name, s.workflow_name, s.parameters,
    recurrence, next_run_at, start_at last_start, end_at last_end
FROM
    cereal_workflow_schedules s
    LEFT JOIN res ON s.instance_name = res.instance_name
        AND s.workflow_name = res.workflow_name
WHERE
    s.instance_name = $1
    AND s.workflow_name = $2`

	getNextScheduledWorkflowQuery = `
SELECT id, enabled, instance_name, workflow_name,
    parameters, recurrence, next_run_at
FROM cereal_workflow_schedules
WHERE enabled = TRUE
ORDER BY next_run_at
LIMIT 1`

	getDueScheduledWorkflowQuery = `
SELECT id, enabled, instance_name, workflow_name,
    parameters, recurrence, next_run_at
FROM cereal_workflow_schedules
WHERE next_run_at < NOW()
    AND enabled = TRUE
ORDER BY next_run_at
FOR UPDATE SKIP LOCKED
LIMIT 1`

	updateScheduledWorkflowQuery = `
UPDATE
    cereal_workflow_schedules
SET
    next_run_at = $2,
    last_enqueued_at = $3,
    enabled = $4
WHERE
    id = $1`

	updateSlowScheduledWorkflowQuery = `
UPDATE
    cereal_workflow_schedules
SET
    next_run_at = $2,
    enabled = $3
WHERE
    id = $1`
)

type PostgresBackend struct {
	connURI string
	db      *sql.DB

	cleaner *taskCleaner

	taskPingInterval time.Duration
}

type PostgresBackendOpt func(*PostgresBackend)

func WithTaskPingInterval(taskPingInterval time.Duration) PostgresBackendOpt {
	return func(pg *PostgresBackend) {
		pg.taskPingInterval = taskPingInterval
	}
}

type PostgresTaskCompleter struct {
	// tid is the Task's id in our postgresql database. We need
	// this to complete the correct task.
	tid int64

	db *sql.DB

	ctx    context.Context
	cancel context.CancelFunc

	pinger *taskPinger
}

type PostgresWorkflowCompleter struct {
	cancel context.CancelFunc
	// wid is the WorkflowInstance id in our postgresql database. We need
	// this to complete the correct workflow.
	wid int64
	// eid is the WorkflowEvent id.
	eid int64

	// tx is the transaction that is holding our dequeued workflow.
	tx *sql.Tx
	// ctx is the context that the transaction was started with.
	ctx context.Context

	enqueueTaskStmt *sql.Stmt

	enqueuedTaskCount  int
	completedTaskCount int
}

type PostgresScheduledWorkflowCompleter struct {
	tx     *sql.Tx
	ctx    context.Context
	cancel context.CancelFunc
}

var _ cereal.Driver = &PostgresBackend{}
var _ cereal.TaskCompleter = &PostgresTaskCompleter{}
var _ cereal.ScheduledWorkflowCompleter = &PostgresScheduledWorkflowCompleter{}

func NewPostgresBackend(connURI string, opts ...PostgresBackendOpt) *PostgresBackend {
	pg := &PostgresBackend{
		connURI:          connURI,
		taskPingInterval: defaultPingDuration,
	}

	for _, o := range opts {
		o(pg)
	}
	return pg
}

func (pg *PostgresBackend) Init() error {
	err := pg.migrate()
	if err != nil {
		return err
	}

	pg.db, err = pg.newDB()
	if err != nil {
		return err
	}

	pg.cleaner = newTaskCleaner(pg.db)
	pg.cleaner.Start(context.Background())

	return nil
}

func (pg *PostgresBackend) newDB() (*sql.DB, error) {
	db, err := sql.Open("postgres", pg.connURI)
	if err != nil {
		return nil, err
	}

	db.SetMaxIdleConns(defaultMaxIdleConnections)

	return db, nil
}

func (pg *PostgresBackend) migrate() error {
	db, err := pg.newDB()
	if err != nil {
		return err
	}

	var dbName string
	err = db.QueryRow("SELECT CURRENT_DATABASE()").Scan(&dbName)
	if err != nil {
		return errors.Wrap(err, "could not get database name")
	}

	dbInstance, err := postgres.WithInstance(db, &postgres.Config{
		MigrationsTable: "cereal_schema_version",
	})
	if err != nil {
		return err
	}

	sourceDrv, err := source.Open("array://local")
	if err != nil {
		return err
	}

	m, err := migrate.NewWithInstance("built-in schema", sourceDrv, dbName, dbInstance)
	if err != nil {
		return err
	}
	defer m.Close() // nolint: errcheck

	version, dirty, err := m.Version()
	if err != nil && err != migrate.ErrNilVersion {
		return errors.Wrap(err, "init migrator - error getting migration version")
	}

	if dirty {
		// force to prior version to reattempt migration
		err := m.Force(int(version) - 1)
		if err != nil {
			return errors.Wrap(err, "force to working schema version")
		}
		logrus.Infof("Forced to previous version: %v to reattempt migration", int(version)-1)
	} else {
		logrus.Infof("Current workflow schema version: %v", version)
	}

	err = m.Up()
	if err == migrate.ErrNoChange {
		logrus.Info("No database migration required")
	} else if err != nil {
		return errors.Wrap(err, "migration failed")
	}

	return nil
}

func (pg *PostgresBackend) Close() error {
	if pg.db != nil {
		pg.cleaner.Stop()
		logrus.Debug("Closing cereal database")
		err := pg.db.Close()
		logrus.WithError(err).Debug("Closed cereal database")
		return err
	}
	return nil
}

func (pg *PostgresBackend) GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*cereal.Schedule, error) {
	scheduledWorkflow := cereal.Schedule{}
	row := pg.db.QueryRowContext(ctx, getScheduledWorkflowQuery, instanceName, workflowName)
	err := row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.InstanceName,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
		&scheduledWorkflow.NextDueAt,
		&scheduledWorkflow.LastStart,
		&scheduledWorkflow.LastEnd,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, cereal.ErrWorkflowScheduleNotFound
		}
		return nil, errors.Wrap(err, "could not read workflow schedule")
	}

	return &scheduledWorkflow, nil
}

func (pg *PostgresBackend) ListWorkflowSchedules(ctx context.Context) ([]*cereal.Schedule, error) {
	ctx, cancel := context.WithTimeout(ctx, 1*time.Minute)
	defer cancel()

	rows, err := pg.db.QueryContext(ctx, listScheduledWorkflowsQuery)
	if err != nil {
		return nil, errors.Wrap(err, "could not query scheduled workflows")
	}

	defer rows.Close() // nolint: errcheck

	schedules := make([]*cereal.Schedule, 0)
	for rows.Next() {
		var scheduledWorkflow cereal.Schedule
		var lastEnqueuedAt *time.Time
		err := rows.Scan(
			&scheduledWorkflow.ID,
			&scheduledWorkflow.Enabled,
			&scheduledWorkflow.InstanceName,
			&scheduledWorkflow.WorkflowName,
			&scheduledWorkflow.Parameters,
			&scheduledWorkflow.Recurrence,
			&lastEnqueuedAt,
			&scheduledWorkflow.NextDueAt,
			&scheduledWorkflow.LastStart,
			&scheduledWorkflow.LastEnd,
		)
		if err != nil {
			return nil, errors.Wrap(err, "could not read workflow schedules")
		}
		if lastEnqueuedAt != nil {
			scheduledWorkflow.LastEnqueuedAt = *lastEnqueuedAt
		}
		schedules = append(schedules, &scheduledWorkflow)
	}
	if rows.Err() != nil {
		return nil, errors.Wrap(err, "could not read workflow schedules")
	}

	return schedules, nil
}

func (pg *PostgresBackend) GetNextScheduledWorkflow(ctx context.Context) (*cereal.Schedule, error) {
	ctx, cancel := context.WithCancel(ctx) // nolint: govet
	defer cancel()

	row := pg.db.QueryRowContext(ctx, getNextScheduledWorkflowQuery)

	var scheduledWorkflow cereal.Schedule
	err := row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.InstanceName,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
		&scheduledWorkflow.NextDueAt,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, cereal.ErrNoScheduledWorkflows
		}
		return nil, err
	}
	return &scheduledWorkflow, err
}

func (pg *PostgresBackend) GetDueScheduledWorkflow(ctx context.Context) (*cereal.Schedule, cereal.ScheduledWorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx) // nolint: govet

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "could not start GetDueScheduledWorkflow transaction")
	}

	row := tx.QueryRowContext(ctx, getDueScheduledWorkflowQuery)

	var scheduledWorkflow cereal.Schedule
	err = row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.InstanceName,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
		&scheduledWorkflow.NextDueAt,
	)

	if err != nil {
		if err == sql.ErrNoRows {
			err := tx.Commit()
			if err != nil {
				logrus.WithError(err).Warn("Failed to commit transaction in GetDueScheduledWorkflows")
			}
			cancel()
			return nil, nil, cereal.ErrNoDueWorkflows
		}
		cancel()
		return nil, nil, errors.Wrap(err, "error fetching due workflows")
	}

	completer := &PostgresScheduledWorkflowCompleter{
		tx:     tx,
		ctx:    ctx,
		cancel: cancel,
	}

	return &scheduledWorkflow, completer, nil

}

func (pg *PostgresBackend) UpdateWorkflowScheduleByName(
	ctx context.Context, instanceName string, workflowName string, opts cereal.WorkflowScheduleUpdateOptions) error {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}

	// Lock row to update
	r := tx.QueryRow("SELECT id FROM cereal_workflow_schedules WHERE instance_name = $1 AND workflow_name = $2 FOR UPDATE",
		instanceName, workflowName)
	var id int64
	if err := r.Scan(&id); err != nil {
		if err == sql.ErrNoRows {
			return cereal.ErrWorkflowScheduleNotFound
		}
		return err
	}

	if opts.UpdateEnabled {
		_, err := tx.Exec("SELECT cereal_update_workflow_schedule_enabled($1, $2)", id, opts.Enabled)
		if err != nil {
			return err
		}
	}

	if opts.UpdateParameters {
		_, err := tx.Exec("SELECT cereal_update_workflow_schedule_parameters($1, $2)", id, opts.Parameters)
		if err != nil {
			return err
		}
	}

	if opts.UpdateRecurrence {
		_, err := tx.Exec("SELECT cereal_update_workflow_schedule_recurrence($1, $2, $3)",
			id, opts.Recurrence, opts.NextRunAt)
		if err != nil {
			return err
		}
	}

	if err := tx.Commit(); err != nil {
		return err
	}
	return nil
}

func (pg *PostgresBackend) CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string,
	parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error {

	wrapErr := func(err error, msg string) error {
		if isPGConflict(err) {
			return cereal.ErrWorkflowScheduleExists
		}
		return errors.Wrap(err, msg)
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return wrapErr(err, "failed to begin create workflow schedule transaction")
	}

	_, err = pg.db.ExecContext(context.TODO(), `
INSERT INTO cereal_workflow_schedules(instance_name, workflow_name, parameters, recurrence, enabled, next_run_at)
VALUES ($1, $2, $3, $4, $5, $6)`,
		instanceName, workflowName, parameters, recurrence, enabled, nextRunAt)
	if err != nil {
		return wrapErr(err, "could not update workflow schedule")
	}

	return wrapErr(tx.Commit(), "failed to commit workflow schedule update")
}

func (pg *PostgresBackend) GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*cereal.WorkflowInstanceData, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, err
	}

	row := tx.QueryRowContext(ctx,
		"SELECT status, parameters, payload FROM cereal_workflow_instances WHERE workflow_name = $1 AND instance_name = $2",
		workflowName, instanceName,
	)
	workflowInstance := cereal.WorkflowInstanceData{
		WorkflowName: workflowName,
		InstanceName: instanceName,
	}
	err = row.Scan(
		&workflowInstance.Status,
		&workflowInstance.Parameters,
		&workflowInstance.Payload,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			row := tx.QueryRowContext(ctx,
				"SELECT parameters, result, error FROM cereal_workflow_results WHERE workflow_name = $1 AND instance_name = $2 ORDER BY id DESC",
				workflowName, instanceName,
			)
			var errStr sql.NullString
			err := row.Scan(
				&workflowInstance.Parameters,
				&workflowInstance.Result,
				&errStr,
			)
			if err != nil {
				if err == sql.ErrNoRows {
					return nil, cereal.ErrWorkflowInstanceNotFound
				}
				return nil, err
			}
			if errStr.Valid {
				workflowInstance.Err = errors.New(errStr.String)
			}
			workflowInstance.Status = cereal.WorkflowInstanceStatusCompleted
		} else {
			return nil, err
		}
	}

	if err := tx.Commit(); err != nil {
		return nil, err
	}
	return &workflowInstance, nil
}

func (pg *PostgresBackend) ListWorkflowInstances(ctx context.Context, opts cereal.ListWorkflowOpts) ([]*cereal.WorkflowInstanceData, error) {
	rows, err := pg.db.Query(listWorkflowInstancesQuery, opts.WorkflowName, opts.InstanceName, opts.IsRunning)
	if err != nil {
		return nil, err
	}
	defer rows.Close() // nolint: errcheck

	instances := []*cereal.WorkflowInstanceData{}
	for rows.Next() {
		var id int64
		workflowInstance := cereal.WorkflowInstanceData{}
		errText := sql.NullString{}
		err := rows.Scan(
			&id,
			&workflowInstance.Status,
			&workflowInstance.InstanceName,
			&workflowInstance.WorkflowName,
			&workflowInstance.Parameters,
			&workflowInstance.Payload,
			&workflowInstance.Result,
			&errText,
		)

		if err != nil {
			return nil, err
		}

		if workflowInstance.Status == cereal.WorkflowInstanceStatusCompleted {
			workflowInstance.Payload = nil
			if errText.Valid {
				workflowInstance.Err = errors.New(errText.String)
			}
		} else {
			workflowInstance.Result = nil
		}
		instances = append(instances, &workflowInstance)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}
	return instances, nil
}

func (pg *PostgresBackend) EnqueueWorkflow(ctx context.Context, w *cereal.WorkflowInstanceData) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "failed to begin enqueue workflow transaction")
	}

	row := tx.QueryRowContext(ctx, enqueueWorkflowQuery, w.InstanceName, w.WorkflowName, w.Parameters)
	var count sql.NullInt64
	err = row.Scan(&count)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue workflow")
	}
	if err := tx.Commit(); err != nil {
		return errors.Wrap(err, "failed to commit enqueue workflow")
	}
	if count.Int64 == 0 {
		return cereal.ErrWorkflowInstanceExists
	}
	return nil
}

func (pg *PostgresBackend) DequeueWorkflow(ctx context.Context, workflowNames []string) (*cereal.WorkflowEvent, cereal.WorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue workflow")
	}

	stmt, err := tx.Prepare(enqueueTaskQuery)
	if err != nil {
		cancel()
		return nil, nil, err
	}

	row := tx.QueryRowContext(ctx, dequeueWorkflowQuery, pq.Array(workflowNames))
	event := &cereal.WorkflowEvent{}
	workc := &PostgresWorkflowCompleter{
		ctx:             ctx,
		enqueueTaskStmt: stmt,
		tx:              tx,
		cancel:          cancel,
	}
	var taskResultID sql.NullInt64
	err = row.Scan(
		&workc.wid,
		&event.Instance.InstanceName,
		&event.Instance.WorkflowName,
		&event.Instance.Status,
		&event.Instance.Parameters,
		&event.Instance.Payload,
		&workc.eid,
		&event.Type,
		&taskResultID,
		&event.EnqueuedTaskCount,
		&event.CompletedTaskCount,
	)
	if err == sql.ErrNoRows {
		tx.Commit() // nolint: errcheck
		cancel()
		return nil, nil, cereal.ErrNoWorkflowInstances

	} else if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue workflow")
	}

	logctx := logrus.WithFields(logrus.Fields{
		"wid":           workc.wid,
		"eid":           workc.eid,
		"status":        event.Instance.Status,
		"workflow_name": event.Instance.WorkflowName,
		"instance_name": event.Instance.InstanceName,
		"event_type":    event.Type,
	})

	logctx.Debug("Dequeued workflow")

	if event.Instance.Status == cereal.WorkflowInstanceStatusStarting && event.Type == cereal.WorkflowCancel {
		defer cancel()
		logctx.Warn("Cancel received before start")
		// This case should be an anomaly. Because our ordering scheme (time) doesn't guarantee
		// that events get processed sequentially, only causally, it could be possible to have a
		// cancel happen before a start event is processed.
		if err := workc.Fail(errors.New("canceled starting workflow")); err != nil {
			return nil, nil, errors.Wrap(err, "failed to cancel unstarted workflow")
		}

		// TODO (jaym): we could rerun this function, however i'm a little
		// afraid of getting into an infinite recursion. The extra delay by not doing
		// this is probably fine for now.
		return nil, nil, cereal.ErrNoDueWorkflows
	}

	if event.Type == cereal.TaskComplete {
		event.CompletedTaskCount++
		tr, err := consumeTaskResult(ctx, tx, taskResultID)
		if err != nil {
			cancel()
			return nil, nil, errors.Wrap(err, "failed to retrieve task result for task complete event")
		}
		event.TaskResult = tr
	}

	workc.enqueuedTaskCount = event.EnqueuedTaskCount
	workc.completedTaskCount = event.CompletedTaskCount

	return event, workc, nil
}

func consumeTaskResult(ctx context.Context, tx *sql.Tx, taskResultID sql.NullInt64) (*cereal.TaskResultData, error) {
	if !taskResultID.Valid {
		return nil, errors.New("invalid task result id for completed task event")
	}

	row := tx.QueryRowContext(ctx, consumeTaskResultQuery, taskResultID.Int64)
	tr := cereal.TaskResultData{}
	err := row.Scan(
		&tr.TaskName, &tr.Parameters, &tr.Status, &tr.ErrorText, &tr.Result)
	if err != nil {
		if err == sql.ErrNoRows {
			// This is unlikely to be a transient problem
			// that aborting and retrying will fix. Thus,
			// we move on and allow the workflow to handle
			// the error, if they needed this result.
			return &cereal.TaskResultData{
				Status:    cereal.TaskStatusUnusableResult,
				ErrorText: err.Error(),
			}, nil
		}
		return nil, err
	}

	return &tr, nil
}

func (pg *PostgresBackend) CancelWorkflow(ctx context.Context, instanceName string, workflowName string) error {
	row := pg.db.QueryRowContext(ctx, cancelWorkflowQuery, instanceName, workflowName)
	var updated sql.NullBool

	if err := row.Scan(&updated); err != nil {
		return errors.Wrap(err, "failed to cancel workflow")
	}

	if !updated.Bool {
		return cereal.ErrWorkflowInstanceNotFound
	}

	return nil
}

func (pg *PostgresBackend) KillWorkflow(ctx context.Context, instanceName string, workflowName string) error {
	row := pg.db.QueryRowContext(ctx, killWorkflowQuery, instanceName, workflowName, "killed")

	var updated sql.NullBool

	if err := row.Scan(&updated); err != nil {
		return errors.Wrap(err, "failed to kill workflow")
	}

	if !updated.Bool {
		return cereal.ErrWorkflowInstanceNotFound
	}

	return nil
}

func (workc *PostgresWorkflowCompleter) EnqueueTask(task *cereal.TaskData, opts cereal.TaskEnqueueOptions) error {
	if opts.StartAfter.IsZero() {
		opts.StartAfter = time.Now()
	}

	_, err := workc.enqueueTaskStmt.ExecContext(workc.ctx,
		workc.wid, opts.StartAfter, task.Name, task.Parameters)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue task")
	}

	workc.enqueuedTaskCount = workc.enqueuedTaskCount + 1
	return nil
}

func (pg *PostgresBackend) dequeueTask(tx *sql.Tx, taskName string) (int64, *cereal.TaskData, error) {
	row := tx.QueryRow(dequeueTaskQuery, taskName)
	task := &cereal.TaskData{
		Name: taskName,
	}

	var tid int64
	err := row.Scan(&tid, &task.Parameters)
	if err == sql.ErrNoRows {
		return 0, nil, cereal.ErrNoTasks
	} else if err != nil {
		return 0, nil, errors.Wrap(err, "failed to dequeue task")
	}

	return tid, task, nil
}

func (pg *PostgresBackend) DequeueTask(ctx context.Context, taskName string) (*cereal.TaskData, cereal.TaskCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, err
	}

	tid, task, err := pg.dequeueTask(tx, taskName)
	if err != nil {
		if errR := tx.Rollback(); errR != nil {
			logrus.WithError(errR).Warn("Failed to rollback dequeue transaction")
		}
		cancel()
		return nil, nil, err
	}

	if err := tx.Commit(); err != nil {
		cancel()
		return nil, nil, err
	}

	pinger := newTaskPinger(pg.db, tid, pg.taskPingInterval)

	taskc := &PostgresTaskCompleter{
		db:     pg.db,
		tid:    tid,
		ctx:    ctx,
		cancel: cancel,
		pinger: pinger,
	}

	pinger.Start(ctx, cancel)

	return task, taskc, nil
}

func (taskc *PostgresTaskCompleter) Context() context.Context {
	return taskc.ctx
}

func (taskc *PostgresTaskCompleter) Fail(errMsg string) error {
	defer taskc.cancel()

	taskc.pinger.Stop()

	select {
	case <-taskc.ctx.Done():
		return cereal.ErrTaskLost
	default:
	}

	_, err := taskc.db.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, cereal.TaskStatusFailed, errMsg, "")

	if err != nil {
		if isErrTaskLost(err) {
			logrus.WithField("task_id", taskc.tid).Warn("Task not updated")
			return cereal.ErrTaskLost
		}
		return errors.Wrapf(err, "failed to mark task %d as failed", taskc.tid)
	}

	return nil
}

// TODO(ssd) 2019-05-10: Should this and Fail also take a context from the caller? If so, we'll need to
func (taskc *PostgresTaskCompleter) Succeed(results []byte) error {
	defer taskc.cancel()

	taskc.pinger.Stop()

	select {
	case <-taskc.ctx.Done():
		return cereal.ErrTaskLost
	default:
	}

	_, err := taskc.db.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, cereal.TaskStatusSuccess, "", results)

	if err != nil {
		if isErrTaskLost(err) {
			logrus.WithField("task_id", taskc.tid).Warn("Task not updated")
			return cereal.ErrTaskLost
		}
		return errors.Wrapf(err, "failed to mark task %d as successful", taskc.tid)
	}
	return nil
}

func (workc *PostgresWorkflowCompleter) Done(result []byte) error {
	ctx := workc.ctx
	defer workc.cancel()

	_, err := workc.tx.ExecContext(ctx, completeWorkflowQuery, workc.wid, result)
	if err != nil {
		return errors.Wrapf(err, "failed to mark workflow %d as complete", workc.wid)
	}

	return errors.Wrapf(workc.tx.Commit(), "failed to mark workflow %d as complete", workc.wid)
}

func (workc *PostgresWorkflowCompleter) Fail(workflowErr error) error {
	ctx := workc.ctx
	defer workc.cancel()

	_, err := workc.tx.ExecContext(ctx, failWorkflowQuery, workc.wid, workflowErr.Error())
	if err != nil {
		return errors.Wrapf(err, "failed to mark workflow %d as failed", workc.wid)
	}

	return errors.Wrapf(workc.tx.Commit(), "failed to mark workflow %d as failed", workc.wid)
}

func (workc *PostgresWorkflowCompleter) Continue(payload []byte) error {
	ctx := workc.ctx
	defer workc.cancel()

	_, err := workc.tx.ExecContext(ctx, continueWorkflowQuery, workc.wid, workc.eid,
		payload, workc.enqueuedTaskCount, workc.completedTaskCount)
	if err != nil {
		return errors.Wrapf(err, "failed to mark workflow event %d as processed", workc.eid)
	}

	return errors.Wrapf(workc.tx.Commit(), "failed to mark workflow event %d as processed", workc.eid)
}

func (workc *PostgresWorkflowCompleter) Close() error {
	workc.cancel()
	return nil
}

// TODO(ssd) 2019-05-14: We should probably allow bulk insertion of workflows and tasks
func (c *PostgresScheduledWorkflowCompleter) EnqueueAndUpdateScheduledWorkflow(s *cereal.Schedule) error {
	defer c.cancel()
	wrapErr := func(err error, msg string) error {
		if isPGConflict(err) {
			return cereal.ErrWorkflowInstanceExists
		}
		return errors.Wrap(err, msg)
	}

	row := c.tx.QueryRowContext(
		c.ctx,
		enqueueWorkflowQuery,
		s.InstanceName,
		s.WorkflowName,
		s.Parameters)

	var count sql.NullInt64
	err := row.Scan(&count)
	if err != nil {
		return wrapErr(err, "failed to enqueue workflow")
	}

	if count.Int64 == 0 {
		_, err = c.tx.ExecContext(
			c.ctx,
			updateSlowScheduledWorkflowQuery,
			s.ID,
			s.NextDueAt,
			s.Enabled)
		if err != nil {
			return wrapErr(err, "failed to update workflow schedule")
		}

		err := c.tx.Commit()
		if err != nil {
			return errors.Wrap(err, "failed to commit workflow instance")
		}
		return cereal.ErrWorkflowInstanceExists
	} else {
		_, err = c.tx.ExecContext(
			c.ctx,
			updateScheduledWorkflowQuery,
			s.ID,
			s.NextDueAt,
			s.LastEnqueuedAt,
			s.Enabled)
		if err != nil {
			return wrapErr(err, "failed to update workflow schedule")
		}

		return wrapErr(c.tx.Commit(), "failed to commit workflow instance")
	}
}

func (c *PostgresScheduledWorkflowCompleter) DisableSchedule(s *cereal.Schedule) error {
	defer c.cancel()
	_, err := c.tx.ExecContext(
		c.ctx,
		"UPDATE cereal_workflow_schedules SET enabled = FALSE WHERE id = $1",
		s.ID)
	if err != nil {
		return errors.Wrap(err, "failed to update workflow schedule")
	}

	return errors.Wrap(c.tx.Commit(), "failed to commit workflow schedule update")
}

func (c *PostgresScheduledWorkflowCompleter) Close() {
	c.cancel()
}

const (
	pgErrUniqueViolation = "23505"
	pgErrCheckViolation  = "23514"
)

func isPGConflict(err error) bool {
	if pqErr, ok := err.(*pq.Error); ok {
		if pqErr.Code == pgErrUniqueViolation {
			return true
		}
	}
	return false
}

func isErrTaskLost(err error) bool {
	if err, ok := err.(*pq.Error); ok {
		if err.Code == pgErrCheckViolation {
			return true
		}
	}
	return false
}
