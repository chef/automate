package workflow

import (
	"context"
	"database/sql"
	"encoding/json"
	"time"

	_ "github.com/lib/pq"
	"github.com/mattes/migrate"
	"github.com/mattes/migrate/database/postgres"
	_ "github.com/mattes/migrate/source/file" // make source available
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	enqueueTaskQuery   = `SELECT enqueue_task($1, $2, $3, $4, $5)`
	dequeueTaskQuery   = `SELECT * FROM dequeue_task($1)`
	completeTaskQuery  = `SELECT complete_task($1::bigint, $2::task_status, $3::text, $4::JSON)`
	getTaskResultQuery = `SELECT task_name, parameters, status, error, result from tasks_results WHERE id = $1`

	enqueueWorkflowQuery  = `SELECT enqueue_workflow($1, $2, $3)`
	dequeueWorkflowQuery  = `SELECT * FROM dequeue_workflow($1)`
	completeWorkflowQuery = `SELECT complete_workflow($1)`
	continueWorkflowQuery = `SELECT continue_workflow($1, $2, $3, $4, $5)`
	abandonWorkflowQuery  = `SELECT abandon_workflow($1, $2, $3)`
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
	Parameters   interface{}
	Payload      interface{}
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
	Parameters         interface{}
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

type TaskResult struct {
	taskName   string
	parameters string

	status    TaskStatusType
	errorText string
	result    string
}
type Backend interface {
	EnqueueWorkflow(ctx context.Context, workflow *WorkflowInstance) error
	DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error)

	DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error)
	Init() error
}

type PostgresBackend struct {
	db *sql.DB
}

type PostgresTaskCompleter struct {
	// tid is the Task's id in our postgresql database.  We need
	// this to complete the correct task.
	tid int64

	// tx is the transaction that is holding our dequeued task.
	tx *sql.Tx
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

var _ Backend = &PostgresBackend{}
var _ TaskCompleter = &PostgresTaskCompleter{}

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
	// TODO(ssd) 2019-05-08: FIXME FIXME
	migrationPath := "file://lib/workflow/postgres/sql"

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

	m, err := migrate.NewWithDatabaseInstance(migrationPath, dbName, dbInstance)
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
	ErrNoTasks             = errors.New("no tasks in queue")
	ErrNoWorkflowInstances = errors.New("no workflow instances in queue")
)

func (pg *PostgresBackend) EnqueueWorkflow(ctx context.Context, w *WorkflowInstance) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "failed to begin enqueue workflow transaction")
	}

	js, err := jsonify(w.Parameters)
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx, enqueueWorkflowQuery, w.InstanceName, w.WorkflowName, js)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue workflow")
	}

	if err := tx.Commit(); err != nil {
		return errors.Wrap(err, "failed to commit enqueue workflow")
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

	js, err := jsonify(task.Parameters)
	if err != nil {
		return err
	}
	_, err = workc.tx.ExecContext(workc.ctx, enqueueTaskQuery,
		task.WorkflowInstanceID, o.TryRemaining, o.StartAfter, task.Name, js)
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
		tx:     tx,
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

	return task, taskc, nil
}

func (taskc *PostgresTaskCompleter) Fail(errMsg string) error {
	defer taskc.cancel()

	_, err := taskc.tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, taskStatusFailed, errMsg, "")
	if err != nil {
		return errors.Wrapf(err, "failed to mark task %d as failed", taskc.tid)
	}

	return errors.Wrapf(taskc.tx.Commit(), "failed to mark task %d as failed", taskc.tid)
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

	_, err = taskc.tx.ExecContext(taskc.ctx, completeTaskQuery, taskc.tid, taskStatusSuccess, "", jsonResults)
	if err != nil {
		return errors.Wrapf(err, "failed to mark task %d as successful", taskc.tid)
	}

	return errors.Wrapf(taskc.tx.Commit(), "failed to mark task %d as successful", taskc.tid)
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

func jsonify(data interface{}) (string, error) {
	b, err := json.Marshal(data)
	if err != nil {
		return "", err
	}
	return string(b), nil
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
