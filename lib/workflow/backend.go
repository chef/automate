package workflow

import (
	"context"
	"database/sql"
	"encoding/json"
	"sync"
	"time"

	"github.com/lib/pq"
	_ "github.com/lib/pq"
	"github.com/mattes/migrate"
	"github.com/mattes/migrate/database/postgres"
	"github.com/mattes/migrate/source"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	enqueueTaskQuery   = `SELECT enqueue_task($1, $2, $3, $4, $5)`
	dequeueTaskQuery   = `SELECT * FROM dequeue_task($1)`
	completeTaskQuery  = `SELECT complete_task($1::bigint, $2::task_status, $3::text, $4)`
	getTaskResultQuery = `SELECT task_name, parameters, status, error, result from tasks_results WHERE id = $1`

	enqueueWorkflowQuery  = `SELECT enqueue_workflow($1, $2, $3)`
	dequeueWorkflowQuery  = `SELECT * FROM dequeue_workflow($1)`
	completeWorkflowQuery = `SELECT complete_workflow($1)`
	continueWorkflowQuery = `SELECT continue_workflow($1, $2, $3, $4, $5)`
	abandonWorkflowQuery  = `SELECT abandon_workflow($1, $2, $3)`

	listRecurringWorkflowsQuery = `
	SELECT id, enabled, name, workflow_name, parameters, recurrence, next_run_at
	FROM recurring_workflow_schedules
	`
	getDueRecurringWorkflowQuery = `
        SELECT id, enabled, name, workflow_name, parameters, recurrence
        FROM recurring_workflow_schedules
        WHERE next_run_at < NOW() AND enabled = TRUE
        FOR UPDATE SKIP LOCKED LIMIT 1
        `
	updateRecurringWorkflowQuery = `
        UPDATE recurring_workflow_schedules SET next_run_at = $2, last_enqueued_at = $3 WHERE id = $1
        `
)

type WorkflowEventType string

const (
	WorkflowStart  WorkflowEventType = "start"
	TaskComplete   WorkflowEventType = "task_complete"
	Cancel         WorkflowEventType = "cancel"
	TasksAbandoned WorkflowEventType = "tasks_abandoned"
)

type TaskStatusType string

const (
	taskStatusSuccess TaskStatusType = "success"
	taskStatusFailed  TaskStatusType = "failed"
)

type WorkflowInstanceStatus string

const (
	WorkflowInstanceStatusRunning   WorkflowInstanceStatus = "running"
	WorkflowInstanceStatusAbandoned WorkflowInstanceStatus = "abandoned"
)

type WorkflowInstance struct {
	InstanceName string
	WorkflowName string
	Status       WorkflowInstanceStatus
	Parameters   []byte
	Payload      []byte
}

type WorkflowEvent struct {
	InstanceID         int64
	Instance           WorkflowInstance
	Type               WorkflowEventType
	EnqueuedTaskCount  int
	CompletedTaskCount int

	TaskResult *TaskResult
}

type Task struct {
	Name               string
	WorkflowInstanceID int64
	Parameters         []byte
}

func (t *Task) GetParameters(obj interface{}) error {
	if t.Parameters != nil {
		return json.Unmarshal(t.Parameters, obj)
	}
	return nil
}

type enqueueOptions struct {
	TryRemaining int
	StartAfter   time.Time
}

func WithRetries(numRetries int) EnqueueOpts {
	return func(o *enqueueOptions) {
		o.TryRemaining = numRetries + 1
	}
}

func StartAfter(startAfter time.Time) EnqueueOpts {
	return func(o *enqueueOptions) {
		o.StartAfter = startAfter
	}
}

type EnqueueOpts func(*enqueueOptions)

type TaskCompleter interface {
	Fail(err string) error
	Succeed(result interface{}) error
}

type WorkflowCompleter interface {
	EnqueueTask(task *Task, opts ...EnqueueOpts) error

	Continue(payload interface{}) error
	Abandon() error
	Done() error
	Close() error
}

type RecurringWorkflowCompleter interface {
	EnqueueRecurringWorkflow(s *Schedule, workflowInstanceName string, nextDueAt time.Time, lastStartedAt time.Time) error
	Close()
}

type TaskResult struct {
	taskName   string
	parameters string

	status    TaskStatusType
	errorText string
	result    string
}

func (r *TaskResult) Err() error {
	if r.status == taskStatusFailed {
		return errors.New(r.errorText)
	}
	return nil
}

func (r *TaskResult) Get(interface{}) error {
	// TODO(ssd) 2019-05-14: UNIMPLEMENTED
	return nil
}

func (r *TaskResult) GetParameters(interface{}) error {
	// TODO(ssd) 2019-05-14: UNIMPLEMENTED
	return nil
}

type Backend interface {
	EnqueueWorkflow(ctx context.Context, workflow *WorkflowInstance) error
	DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error)

	DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error)

	CreateWorkflowSchedule(ctx context.Context, scheduleName string, workflowName string, parameters interface{}, enabled bool, recurrence string, nextRunAt time.Time) error
	GetDueRecurringWorkflow(ctx context.Context) (*Schedule, RecurringWorkflowCompleter, error)
	ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error)

	Init() error
}

type PostgresBackend struct {
	db *sql.DB
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

	enqueuedTaskCount  int
	completedTaskCount int
}

type PostgresRecurringWorkflowCompleter struct {
	tx     *sql.Tx
	ctx    context.Context
	cancel context.CancelFunc
}

var _ Backend = &PostgresBackend{}
var _ TaskCompleter = &PostgresTaskCompleter{}
var _ RecurringWorkflowCompleter = &PostgresRecurringWorkflowCompleter{}

func NewPostgresBackend(connInfo string) (*PostgresBackend, error) {
	db, err := sql.Open("postgres", connInfo)
	if err != nil {
		return nil, err
	}

	return &PostgresBackend{
		db: db,
	}, nil
}

func (pg *PostgresBackend) Init() error {
	var dbName string
	err := pg.db.QueryRow("SELECT CURRENT_DATABASE()").Scan(&dbName)
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

var (
	ErrNoTasks                = errors.New("no tasks in queue")
	ErrNoWorkflowInstances    = errors.New("no workflow instances in queue")
	ErrWorkflowScheduleExists = errors.New("workflow schedule already exists")
	ErrWorkflowInstanceExists = errors.New("workflow instance already exists")
	ErrNoDueWorkflows         = errors.New("No due workflows")
)

func (pg *PostgresBackend) ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error) {
	ctx, cancel := context.WithTimeout(ctx, 1*time.Minute)
	defer cancel()

	rows, err := pg.db.QueryContext(ctx, listRecurringWorkflowsQuery)
	if err != nil {
		return nil, errors.Wrap(err, "could not query recurring workflows")
	}

	defer rows.Close() // nolint: errcheck

	schedules := make([]*Schedule, 0)
	for rows.Next() {
		var scheduledWorkflow Schedule
		err := rows.Scan(
			&scheduledWorkflow.ID,
			&scheduledWorkflow.Enabled,
			&scheduledWorkflow.Name,
			&scheduledWorkflow.WorkflowName,
			&scheduledWorkflow.Parameters,
			&scheduledWorkflow.Recurrence,
			&scheduledWorkflow.NextDueAt,
		)
		if err != nil {
			logrus.WithError(err).Error("could not scan workflow schedule from database, skipping")
			// TODO(ssd) 2019-05-13: Should we return here?
			continue
		}
		schedules = append(schedules, &scheduledWorkflow)
	}
	return schedules, nil
}

func (pg *PostgresBackend) GetDueRecurringWorkflow(ctx context.Context) (*Schedule, RecurringWorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx) // nolint: govet

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "could not start GetDueRecurringWorkflow transaction")
	}

	row := tx.QueryRowContext(ctx, getDueRecurringWorkflowQuery)

	var scheduledWorkflow Schedule
	err = row.Scan(
		&scheduledWorkflow.ID,
		&scheduledWorkflow.Enabled,
		&scheduledWorkflow.Name,
		&scheduledWorkflow.WorkflowName,
		&scheduledWorkflow.Parameters,
		&scheduledWorkflow.Recurrence,
	)

	if err != nil {
		cancel()
		if err == sql.ErrNoRows {
			return nil, nil, ErrNoDueWorkflows
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

type workflowScheduleUpdateOpts struct {
	updateEnabled bool
	enabled       bool

	updateParameters bool
	parameters       []byte

	updateRecurrence bool
	recurrence       string
	nextRunAt        time.Time
}

type WorkflowScheduleUpdateOpts func(*workflowScheduleUpdateOpts) error

func UpdateEnabled(enabled bool) WorkflowScheduleUpdateOpts {
	return func(o *workflowScheduleUpdateOpts) error {
		o.enabled = enabled
		o.updateEnabled = true
		return nil
	}
}

func UpdateParameters(parameters interface{}) WorkflowScheduleUpdateOpts {
	return func(o *workflowScheduleUpdateOpts) error {
		paramsData, err := jsonify(parameters)
		o.updateParameters = true
		o.parameters = paramsData
		return err
	}
}

func UpdateRecurrence(recurrence string, nextRunAt time.Time) WorkflowScheduleUpdateOpts {
	return func(o *workflowScheduleUpdateOpts) error {
		o.updateRecurrence = true
		o.recurrence = recurrence
		o.nextRunAt = nextRunAt
		return nil
	}
}

func (pg *PostgresBackend) UpdateWorkflowScheduleByID(ctx context.Context, id int64, opts ...WorkflowScheduleUpdateOpts) error {
	o := workflowScheduleUpdateOpts{}
	for _, opt := range opts {
		err := opt(&o)
		if err != nil {
			return err
		}
	}

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

	if o.updateEnabled {
		_, err := tx.Exec("SELECT update_recurring_workflow_enabled($1, $2)", id, o.enabled)
		if err != nil {
			return err
		}
	}

	if o.updateParameters {
		_, err := tx.Exec("SELECT update_recurring_workflow_parameters($1, $2)", id, o.parameters)
		if err != nil {
			return err
		}
	}

	if o.updateRecurrence {
		_, err := tx.Exec("SELECT update_recurring_workflow_recurrence($1, $2, $3)",
			id, o.recurrence, o.nextRunAt)
		if err != nil {
			return err
		}
	}

	if err := tx.Commit(); err != nil {
		return err
	}
	return nil
}

// TODO(ssd) 2019-05-13: We need to decide on what "update" will look like
func (pg *PostgresBackend) CreateWorkflowSchedule(ctx context.Context, scheduleName string, workflowName string,
	parameters interface{}, enabled bool, recurrence string, nextRunAt time.Time) error {

	wrapErr := func(err error, msg string) error {
		if pqErr, ok := err.(*pq.Error); ok {
			// unique violation
			if pqErr.Code == "23505" {
				return ErrWorkflowScheduleExists
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

	js, err := jsonify(parameters)
	if err != nil {
		return wrapErr(err, "failed to convert parameters to JSON")
	}
	_, err = pg.db.ExecContext(context.TODO(), `
INSERT INTO recurring_workflow_schedules(name, workflow_name, parameters, recurrence, enabled, next_run_at)
VALUES ($1, $2, $3, $4, $5, $6)`,
		scheduleName, workflowName, js, recurrence, enabled, nextRunAt)
	if err != nil {
		return wrapErr(err, "could not update workflow schedule")
	}

	return wrapErr(tx.Commit(), "failed to commit workflow schedule update")
}

func (pg *PostgresBackend) EnqueueWorkflow(ctx context.Context, w *WorkflowInstance) error {
	wrapErr := func(err error, msg string) error {
		if pqErr, ok := err.(*pq.Error); ok {
			// unique violation
			if pqErr.Code == "23505" {
				return ErrWorkflowInstanceExists
			}
		}

		return errors.Wrap(err, msg)
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return wrapErr(err, "failed to begin enqueue workflow transaction")
	}

	_, err = tx.ExecContext(ctx, enqueueWorkflowQuery, w.InstanceName, w.WorkflowName, w.Parameters)
	if err != nil {
		return wrapErr(err, "failed to enqueue workflow")
	}

	if err := tx.Commit(); err != nil {
		return wrapErr(err, "failed to commit enqueue workflow")
	}
	return nil
}

func (pg *PostgresBackend) DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue workflow")
	}

	// TODO: allow multiple workflow names
	row := tx.QueryRowContext(ctx, dequeueWorkflowQuery, workflowNames[0])
	event := &WorkflowEvent{}
	workc := &PostgresWorkflowCompleter{
		ctx:    ctx,
		tx:     tx,
		cancel: cancel,
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
		cancel()
		return nil, nil, ErrNoWorkflowInstances

	} else if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue workflow")
	}

	event.InstanceID = workc.wid

	if event.Type == TaskComplete {
		event.CompletedTaskCount++
		row := tx.QueryRowContext(ctx, getTaskResultQuery, taskResultID)
		tr := TaskResult{}
		err = row.Scan(
			&tr.taskName, &tr.parameters, &tr.status, &tr.errorText, &tr.result)
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

func (workc *PostgresWorkflowCompleter) EnqueueTask(task *Task, opts ...EnqueueOpts) error {
	o := mergeEnqueueOpts(opts)

	_, err := workc.tx.ExecContext(workc.ctx, enqueueTaskQuery,
		task.WorkflowInstanceID, o.TryRemaining, o.StartAfter, task.Name, task.Parameters)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue task")
	}

	workc.enqueuedTaskCount = workc.enqueuedTaskCount + 1
	return nil
}

func (pg *PostgresBackend) DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)

	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue task")
	}

	row := tx.QueryRowContext(ctx, dequeueTaskQuery, taskName)
	task := &Task{
		Name: taskName,
	}
	taskc := &PostgresTaskCompleter{
		ctx:    ctx,
		_tx:    tx,
		cancel: cancel,
	}

	err = row.Scan(&taskc.tid, &task.WorkflowInstanceID, &task.Parameters)
	if err == sql.ErrNoRows {
		cancel()
		return nil, nil, ErrNoTasks

	} else if err != nil {
		cancel()
		return nil, nil, errors.Wrap(err, "failed to dequeue task")
	}

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
		_, err := tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, taskStatusFailed, errMsg, "")
		if err != nil {
			return errors.Wrapf(err, "failed to mark task %d as failed", taskc.tid)
		}

		return errors.Wrapf(tx.Commit(), "failed to mark task %d as failed", taskc.tid)
	})
}

// TODO(ssd) 2019-05-10: Should this and Fail also take a context from the caller? If so, we'll need to
func (taskc *PostgresTaskCompleter) Succeed(results interface{}) error {
	defer taskc.cancel()

	jsonResults, err := jsonify(results)
	if err != nil {
		// NOTE(ssd) 2019-05-08: This kills the transacation,
		// do we want that?
		return errors.Wrap(err, "could not convert results to JSON")
	}

	return taskc.useTx(func(tx *sql.Tx) error {
		_, err = tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, taskStatusSuccess, "", jsonResults)
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

func (workc *PostgresWorkflowCompleter) Continue(payload interface{}) error {
	ctx := workc.ctx
	defer workc.cancel()

	jsonPayload, err := jsonify(payload)
	if err != nil {
		return errors.Wrap(err, "could not convert payload to JSON")
	}
	_, err = workc.tx.ExecContext(ctx, continueWorkflowQuery, workc.wid, workc.eid,
		jsonPayload, workc.enqueuedTaskCount, workc.completedTaskCount)
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
	s *Schedule,
	workflowInstanceName string,
	nextDueAt time.Time,
	lastStartedAt time.Time,
) error {
	defer c.cancel()
	wrapErr := func(err error, msg string) error {
		if pqErr, ok := err.(*pq.Error); ok {
			// unique violation
			if pqErr.Code == "23505" {
				return ErrWorkflowInstanceExists
			}
		}

		return errors.Wrap(err, msg)
	}
	_, err := c.tx.ExecContext(c.ctx, enqueueWorkflowQuery, workflowInstanceName, s.WorkflowName, s.Parameters)
	if err != nil {
		return wrapErr(err, "failed to enqueue workflow")
	}

	_, err = c.tx.ExecContext(c.ctx, updateRecurringWorkflowQuery, s.ID, nextDueAt, lastStartedAt)
	if err != nil {
		return wrapErr(err, "failed to update workflow schedule")
	}

	return wrapErr(c.tx.Commit(), "failed to commit workflow instance")
}

func (c *PostgresRecurringWorkflowCompleter) Close() {
	c.cancel()
}

func jsonify(data interface{}) ([]byte, error) {
	if data == nil {
		return nil, nil
	}
	return json.Marshal(data)
}

func mergeEnqueueOpts(opts []EnqueueOpts) enqueueOptions {
	o := enqueueOptions{
		TryRemaining: 1,
		StartAfter:   time.Now(),
	}
	for _, f := range opts {
		f(&o)
	}
	return o
}
