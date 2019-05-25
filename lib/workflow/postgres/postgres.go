package postgres

import (
	"context"
	"database/sql"
	"sync"
	"time"

	"github.com/lib/pq"
	"github.com/mattes/migrate"
	"github.com/mattes/migrate/database/postgres"
	"github.com/mattes/migrate/source"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/workflow"
	"github.com/chef/automate/lib/workflow/backend"
)

const (
	enqueueTaskQuery   = `SELECT enqueue_task($1, $2, $3, $4, $5)`
	dequeueTaskQuery   = `SELECT * FROM dequeue_task($1)`
	completeTaskQuery  = `SELECT complete_task($1::bigint, $2::task_status, $3::text, $4)`
	getTaskResultQuery = `SELECT task_name, parameters, status, error, result from tasks_results WHERE id = $1`

	enqueueWorkflowQuery  = `SELECT enqueue_workflow($1, $2, $3)`
	dequeueWorkflowQuery  = `SELECT * FROM dequeue_workflow(VARIADIC $1)`
	completeWorkflowQuery = `SELECT complete_workflow($1)`
	continueWorkflowQuery = `SELECT continue_workflow($1, $2, $3, $4, $5)`
	abandonWorkflowQuery  = `SELECT abandon_workflow($1, $2, $3)`

	listRecurringWorkflowsQuery = `
		WITH res AS
			(SELECT DISTINCT ON (name, workflow_name) * from workflow_results
			ORDER BY name, workflow_name, end_at DESC)
		SELECT s.id, enabled, s.name, s.workflow_name, s.parameters, recurrence,
			next_run_at, start_at last_start, end_at last_end
			FROM recurring_workflow_schedules s LEFT JOIN res
			ON s.name = res.name AND s.workflow_name = res.workflow_name;
		`

	getNextRecurringWorkflowQuery = `
        SELECT id, enabled, name, workflow_name, parameters, recurrence, next_run_at
        FROM recurring_workflow_schedules
        WHERE enabled = TRUE
        ORDER BY next_run_at LIMIT 1
        `
	getDueRecurringWorkflowQuery = `
        SELECT id, enabled, name, workflow_name, parameters, recurrence, next_run_at
        FROM recurring_workflow_schedules
        WHERE next_run_at < NOW() AND enabled = TRUE
        ORDER BY next_run_at
        FOR UPDATE SKIP LOCKED LIMIT 1
        `
	updateRecurringWorkflowQuery = `
        UPDATE recurring_workflow_schedules SET next_run_at = $2, last_enqueued_at = $3 WHERE id = $1
        `

	updateSlowRecurringWorkflowQuery = `
        UPDATE recurring_workflow_schedules SET next_run_at = $2 WHERE id = $1
        `
)

type PostgresBackend struct {
	connURI string
	db      *sql.DB
}

type PostgresTaskCompleter struct {
	// tid is the Task's id in our postgresql database.  We need
	// this to complete the correct task.
	tid int64

	_txLock sync.Mutex
	// _tx is the transaction that is holding our dequeued task.
	_tx *sql.Tx
	// ctx is the context that the transaction was started with.
	ctx context.Context
	// canceling this will abort the transaction
	cancel context.CancelFunc
}

type PostgresWorkflowCompleter struct {
	cancel context.CancelFunc
	// wid is the WorkflowInstance id in our postgresql database.  We need
	// this to complete the correct workflow
	wid int64
	// eid is the WorkflowEvent id
	eid int64

	// tx is the transaction that is holding our dequeued workflow.
	tx *sql.Tx
	// ctx is the context that the transaction was started with.
	ctx context.Context

	enqueueTaskStmt *sql.Stmt

	enqueuedTaskCount  int
	completedTaskCount int
}

type PostgresRecurringWorkflowCompleter struct {
	tx     *sql.Tx
	ctx    context.Context
	cancel context.CancelFunc
}

var _ backend.Driver = &PostgresBackend{}
var _ backend.TaskCompleter = &PostgresTaskCompleter{}
var _ backend.RecurringWorkflowCompleter = &PostgresRecurringWorkflowCompleter{}

func NewPostgresBackend(connURI string) *PostgresBackend {
	return &PostgresBackend{
		connURI: connURI,
	}
}

func (pg *PostgresBackend) Init() error {
	db, err := sql.Open("postgres", pg.connURI)
	if err != nil {
		return err
	}
	pg.db = db

	var dbName string
	err = pg.db.QueryRow("SELECT CURRENT_DATABASE()").Scan(&dbName)
	if err != nil {
		return errors.Wrap(err, "could not get database name")
	}

	dbInstance, err := postgres.WithInstance(pg.db, &postgres.Config{
		MigrationsTable: "workflow_schema_version",
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
	// defer m.Close() I don't think we want to call close here because it'll close our db instance

	err = m.Up()
	if err == migrate.ErrNoChange {
		logrus.Info("No database migration required")
	} else if err != nil {
		return errors.Wrap(err, "migration failed")
	}

	return nil
}

func (pg *PostgresBackend) Close() error {
	return pg.db.Close()
}

func (pg *PostgresBackend) GetScheduledWorkflowParameters(ctx context.Context, scheduleName string, workflowName string) ([]byte, error) {
	row := pg.db.QueryRowContext(ctx, "SELECT parameters FROM recurring_workflow_schedules WHERE name = $1 and workflow_name = $2",
		scheduleName, workflowName)
	var data []byte

	err := row.Scan(&data)
	if err != nil {
		return nil, err
	}

	return data, nil
}

func (pg *PostgresBackend) GetScheduledWorkflowRecurrence(ctx context.Context, scheduleName string, workflowName string) (string, error) {
	row := pg.db.QueryRowContext(ctx, "SELECT recurrence FROM recurring_workflow_schedules WHERE name = $1 and workflow_name = $2",
		scheduleName, workflowName)

	var data string
	err := row.Scan(&data)
	if err != nil {
		return "", err
	}

	return data, nil
}

func (pg *PostgresBackend) ListWorkflowSchedules(ctx context.Context) ([]*backend.Schedule, error) {
	ctx, cancel := context.WithTimeout(ctx, 1*time.Minute)
	defer cancel()

	rows, err := pg.db.QueryContext(ctx, listRecurringWorkflowsQuery)
	if err != nil {
		return nil, errors.Wrap(err, "could not query recurring workflows")
	}

	defer rows.Close() // nolint: errcheck

	schedules := make([]*backend.Schedule, 0)
	for rows.Next() {
		var scheduledWorkflow backend.Schedule
		err := rows.Scan(
			&scheduledWorkflow.ID,
			&scheduledWorkflow.Enabled,
			&scheduledWorkflow.Name,
			&scheduledWorkflow.WorkflowName,
			&scheduledWorkflow.Parameters,
			&scheduledWorkflow.Recurrence,
			&scheduledWorkflow.NextDueAt,
			&scheduledWorkflow.LastStart,
			&scheduledWorkflow.LastEnd,
		)
		if err != nil {
			return nil, errors.Wrap(err, "could not read workflow schedules")
		}
		schedules = append(schedules, &scheduledWorkflow)
	}
	return schedules, nil
}

func (pg *PostgresBackend) GetNextScheduledWorkflow(ctx context.Context) (*backend.Schedule, error) {
	ctx, cancel := context.WithCancel(ctx) // nolint: govet
	defer cancel()

	row := pg.db.QueryRowContext(ctx, getNextRecurringWorkflowQuery)

	var scheduledWorkflow backend.Schedule
	err := row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.Name,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
		&scheduledWorkflow.NextDueAt,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, workflow.ErrNoScheduledWorkflows
		}
		return nil, err
	}
	return &scheduledWorkflow, err
}

func (pg *PostgresBackend) GetDueRecurringWorkflow(ctx context.Context) (*backend.Schedule, backend.RecurringWorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx) // nolint: govet

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "could not start GetDueRecurringWorkflow transaction")
	}

	row := tx.QueryRowContext(ctx, getDueRecurringWorkflowQuery)

	var scheduledWorkflow backend.Schedule
	err = row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.Name,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
		&scheduledWorkflow.NextDueAt,
	)

	if err != nil {
		cancel()
		if err == sql.ErrNoRows {
			return nil, nil, workflow.ErrNoDueWorkflows
		}
		return nil, nil, errors.Wrap(err, "error fetching due workflows")
	}

	completer := &PostgresRecurringWorkflowCompleter{
		tx:     tx,
		ctx:    ctx,
		cancel: cancel,
	}

	return &scheduledWorkflow, completer, nil

}

func (pg *PostgresBackend) UpdateWorkflowScheduleByName(
	ctx context.Context, scheduleName string, workflowName string, opts backend.WorkflowScheduleUpdateOpts) error {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}

	// Lock row to update
	r := tx.QueryRow("SELECT id FROM recurring_workflow_schedules WHERE name = $1 AND workflow_name = $2 FOR UPDATE",
		scheduleName, workflowName)
	var id int64
	if err := r.Scan(&id); err != nil {
		if err != sql.ErrNoRows {
			// TODO(jaym) return not found
		}
		return err
	}

	err = pg.updateWorkflowScheduleByID(tx, id, &opts)
	if err != nil {
		return err
	}

	if err := tx.Commit(); err != nil {
		return err
	}
	return nil
}

func (pg *PostgresBackend) UpdateWorkflowScheduleByID(ctx context.Context, id int64, opts backend.WorkflowScheduleUpdateOpts) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}

	// Lock row to update
	r := tx.QueryRow("SELECT id FROM recurring_workflow_schedules WHERE id = $1 FOR UPDATE", id)
	var throwaway int64
	if err := r.Scan(&throwaway); err != nil {
		if err != sql.ErrNoRows {
			// TODO(jaym) return not found
		}
		return err
	}

	err = pg.updateWorkflowScheduleByID(tx, id, &opts)
	if err != nil {
		return err
	}

	if err := tx.Commit(); err != nil {
		return err
	}
	return nil
}

func (pg *PostgresBackend) updateWorkflowScheduleByID(tx *sql.Tx, id int64, o *backend.WorkflowScheduleUpdateOpts) error {
	if o.UpdateEnabled {
		_, err := tx.Exec("SELECT update_recurring_workflow_enabled($1, $2)", id, o.Enabled)
		if err != nil {
			return err
		}
	}

	if o.UpdateParameters {
		_, err := tx.Exec("SELECT update_recurring_workflow_parameters($1, $2)", id, o.Parameters)
		if err != nil {
			return err
		}
	}

	if o.UpdateRecurrence {
		_, err := tx.Exec("SELECT update_recurring_workflow_recurrence($1, $2, $3)",
			id, o.Recurrence, o.NextRunAt)
		if err != nil {
			return err
		}
	}
	return nil
}

// TODO(ssd) 2019-05-13: We need to decide on what "update" will look like
func (pg *PostgresBackend) CreateWorkflowSchedule(ctx context.Context, scheduleName string, workflowName string,
	parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error {

	wrapErr := func(err error, msg string) error {
		if pqErr, ok := err.(*pq.Error); ok {
			// unique violation
			if pqErr.Code == "23505" {
				return workflow.ErrWorkflowScheduleExists
			}
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
INSERT INTO recurring_workflow_schedules(name, workflow_name, parameters, recurrence, enabled, next_run_at)
VALUES ($1, $2, $3, $4, $5, $6)`,
		scheduleName, workflowName, parameters, recurrence, enabled, nextRunAt)
	if err != nil {
		return wrapErr(err, "could not update workflow schedule")
	}

	return wrapErr(tx.Commit(), "failed to commit workflow schedule update")
}

func (pg *PostgresBackend) EnqueueWorkflow(ctx context.Context, w *backend.WorkflowInstance) error {
	wrapErr := func(err error, msg string) error {
		return errors.Wrap(err, msg)
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return wrapErr(err, "failed to begin enqueue workflow transaction")
	}

	row := tx.QueryRowContext(ctx, enqueueWorkflowQuery, w.InstanceName, w.WorkflowName, w.Parameters)
	var count sql.NullInt64
	err = row.Scan(&count)
	if err != nil {
		return wrapErr(err, "failed to enqueue workflow")
	}
	if err := tx.Commit(); err != nil {
		return wrapErr(err, "failed to commit enqueue workflow")
	}
	if count.Int64 == 0 {
		return workflow.ErrWorkflowInstanceExists
	}
	return nil
}

func (pg *PostgresBackend) DequeueWorkflow(ctx context.Context, workflowNames []string) (*backend.WorkflowEvent, backend.WorkflowCompleter, error) {
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
	// TODO: allow multiple workflow names
	row := tx.QueryRowContext(ctx, dequeueWorkflowQuery, pq.Array(workflowNames))
	event := &backend.WorkflowEvent{}
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
		return nil, nil, workflow.ErrNoWorkflowInstances

	} else if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue workflow")
	}

	event.InstanceID = workc.wid

	if event.Type == backend.TaskComplete {
		event.CompletedTaskCount++
		row := tx.QueryRowContext(ctx, getTaskResultQuery, taskResultID)
		tr := backend.TaskResult{}
		err = row.Scan(
			&tr.TaskName, &tr.Parameters, &tr.Status, &tr.ErrorText, &tr.Result)
		if err != nil {
			cancel()
			// FIXME FIXME FIXME
			// TODO is this an infinite loop (this is panic situation)
			return nil, nil, err
		}
		event.TaskResult = &tr
	}

	workc.enqueuedTaskCount = event.EnqueuedTaskCount
	workc.completedTaskCount = event.CompletedTaskCount

	return event, workc, nil
}

func (workc *PostgresWorkflowCompleter) EnqueueTask(task *backend.Task, opts backend.TaskEnqueueOpts) error {
	if opts.TryRemaining <= 0 {
		opts.TryRemaining = 1
	}
	if opts.StartAfter.IsZero() {
		opts.StartAfter = time.Now()
	}

	_, err := workc.enqueueTaskStmt.ExecContext(workc.ctx,
		task.WorkflowInstanceID, opts.TryRemaining, opts.StartAfter, task.Name, task.Parameters)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue task")
	}

	workc.enqueuedTaskCount = workc.enqueuedTaskCount + 1
	return nil
}

func (pg *PostgresBackend) dequeueTask(tx *sql.Tx, taskName string) (int64, *backend.Task, error) {
	row := tx.QueryRow(dequeueTaskQuery, taskName)
	task := &backend.Task{
		Name: taskName,
	}

	var tid int64
	err := row.Scan(&tid, &task.WorkflowInstanceID, &task.Parameters)
	if err == sql.ErrNoRows {
		if err != nil {
			logrus.WithError(err).Warn("failed to commit dequeue_task transaction after ErrNoRows")
		}
		return 0, nil, workflow.ErrNoTasks

	} else if err != nil {
		return 0, nil, errors.Wrap(err, "failed to dequeue task")
	}
	return tid, task, nil
}
func (pg *PostgresBackend) DequeueTask(ctx context.Context, taskName string) (*backend.Task, backend.TaskCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue task")
	}

	taskc := &PostgresTaskCompleter{
		ctx:    ctx,
		_tx:    tx,
		cancel: cancel,
	}

	tid, task, err := pg.dequeueTask(tx, taskName)
	if err != nil {
		cancel()
		return nil, nil, err
	}
	taskc.tid = tid

	// pinger
	go func() {
		for {
			select {
			case <-taskc.ctx.Done():
				return
			case <-time.After(10 * time.Second):
				err := taskc.useTx(func(tx *sql.Tx) error {
					_, err := tx.ExecContext(taskc.ctx, "")
					return err
				})

				if err != nil {
					logrus.WithError(err).Error("Transaction pinger failed")
					return
				}
			}
		}
	}()

	return task, taskc, nil
}

func (taskc *PostgresTaskCompleter) useTx(f func(tx *sql.Tx) error) error {
	taskc._txLock.Lock()
	defer taskc._txLock.Unlock()
	return f(taskc._tx)
}

func (taskc *PostgresTaskCompleter) Fail(errMsg string) error {
	defer taskc.cancel()

	return taskc.useTx(func(tx *sql.Tx) error {
		_, err := tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, backend.TaskStatusFailed, errMsg, "")
		if err != nil {
			return errors.Wrapf(err, "failed to mark task %d as failed", taskc.tid)
		}

		return errors.Wrapf(tx.Commit(), "failed to mark task %d as failed", taskc.tid)
	})
}

// TODO(ssd) 2019-05-10: Should this and Fail also take a context from the caller? If so, we'll need to
func (taskc *PostgresTaskCompleter) Succeed(results []byte) error {
	defer taskc.cancel()

	return taskc.useTx(func(tx *sql.Tx) error {
		_, err := tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, backend.TaskStatusSuccess, "", results)
		if err != nil {
			return errors.Wrapf(err, "failed to mark task %d as successful", taskc.tid)
		}

		return errors.Wrapf(tx.Commit(), "failed to mark task %d as successful", taskc.tid)
	})
}

func (workc *PostgresWorkflowCompleter) Done() error {
	ctx := workc.ctx
	defer workc.cancel()

	_, err := workc.tx.ExecContext(ctx, completeWorkflowQuery, workc.wid)
	if err != nil {
		return errors.Wrapf(err, "failed to mark workflow %d as complete", workc.wid)
	}

	return errors.Wrapf(workc.tx.Commit(), "failed to mark workflow %d as complete", workc.wid)
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

func (workc *PostgresWorkflowCompleter) Abandon() error {
	ctx := workc.ctx
	defer workc.cancel()

	_, err := workc.tx.ExecContext(ctx, abandonWorkflowQuery, workc.wid, workc.eid, workc.completedTaskCount)
	if err != nil {
		return errors.Wrapf(err, "failed to mark workflow %d as complete", workc.wid)
	}

	return errors.Wrapf(workc.tx.Commit(), "failed to mark workflow %d as complete", workc.wid)
}

func (workc *PostgresWorkflowCompleter) Close() error {
	workc.cancel()
	return nil
}

// TODO(ssd) 2019-05-14: We should probably allow bulk insertion of workflows and tasks
func (c *PostgresRecurringWorkflowCompleter) EnqueueRecurringWorkflow(
	s *backend.Schedule,
	workflowInstanceName string,
	nextDueAt time.Time,
	lastStartedAt time.Time,
) error {
	defer c.cancel()
	wrapErr := func(err error, msg string) error {
		if pqErr, ok := err.(*pq.Error); ok {
			// unique violation
			if pqErr.Code == "23505" {
				return workflow.ErrWorkflowInstanceExists
			}
		}

		return errors.Wrap(err, msg)
	}

	row := c.tx.QueryRowContext(c.ctx, enqueueWorkflowQuery, workflowInstanceName, s.WorkflowName, s.Parameters)

	var count sql.NullInt64
	err := row.Scan(&count)
	if err != nil {
		return wrapErr(err, "failed to enqueue workflow")
	}

	if count.Int64 == 0 {
		_, err = c.tx.ExecContext(c.ctx, updateSlowRecurringWorkflowQuery, s.ID, nextDueAt)
		if err != nil {
			return wrapErr(err, "failed to update workflow schedule")
		}

		err := c.tx.Commit()
		if err != nil {
			return errors.Wrap(err, "failed to commit workflow instance")
		}
		return workflow.ErrWorkflowInstanceExists
	} else {
		_, err = c.tx.ExecContext(c.ctx, updateRecurringWorkflowQuery, s.ID, nextDueAt, lastStartedAt)
		if err != nil {
			return wrapErr(err, "failed to update workflow schedule")
		}

		return wrapErr(c.tx.Commit(), "failed to commit workflow instance")
	}
}

func (c *PostgresRecurringWorkflowCompleter) Close() {
	c.cancel()
}
