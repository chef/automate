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
	enqueueTaskQuery  = `SELECT enqueue_task($1, $2, $3, $4, $5)`
	dequeueTaskQuery  = `SELECT * FROM dequeue_task($1)`
	completeTaskQuery = `SELECT complete_task($1::bigint, $2::task_status, $3::text, $4::JSON)`

	taskStatusSuccess   = "success"
	taskStatusFailed    = "failed"
	taskStatusAbandoned = "abanonded"
)

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

type Backend interface {
	EnqueueTask(ctx context.Context, task *Task, opts ...EnqueueOpts) error
	DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error)
	Init() error
}

type PostgresBackend struct {
	db *sql.DB
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

	// FIX ME
	_, err = pg.db.Exec("INSERT INTO workflow_instances(workflow_name, name) VALUES ('workflow_name', 'bob') ON CONFLICT DO NOTHING")
	if err != nil {
		logrus.WithError(err).Error("could not insert fake workflow")
		return err
	}
	return nil
}

var ErrNoTasks = errors.New("no tasks in queue")

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
		ctx: ctx,
		tx:  tx,
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

func (pg *PostgresBackend) EnqueueTask(ctx context.Context, task *Task, opts ...EnqueueOpts) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "failed to begin enqueue task transaction")
	}

	o := mergeEnqueueOpts(opts)

	js, err := jsonify(task.Parameters)
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx, enqueueTaskQuery,
		task.WorkflowInstanceID, o.TryRemaining, o.StartAfter, task.Name, js)
	if err != nil {
		return errors.Wrap(err, "failed to enqueue task")
	}

	if err := tx.Commit(); err != nil {
		return errors.Wrap(err, "failed to commit enqueue task")
	}
	return nil
}

type PostgresTaskCompleter struct {
	// tid is the Task's id in our postgresql database.  We need
	// this to complete the correct task.
	tid int

	// tx is the transaction that is holding our dequeued task.
	tx *sql.Tx
	// ctx is the context that the transaction was started with.
	ctx context.Context
}

func (taskc PostgresTaskCompleter) Fail(errMsg string) error {
	ctx, cancel := context.WithCancel(taskc.ctx)
	defer cancel()

	_, err := taskc.tx.ExecContext(ctx, completeTaskQuery, taskc.tid, taskStatusFailed, errMsg, "")
	if err != nil {
		return errors.Wrapf(err, "failed to mark task %d as failed", taskc.tid)
	}

	return errors.Wrapf(taskc.tx.Commit(), "failed to mark task %d as failed", taskc.tid)
}

func (taskc PostgresTaskCompleter) Succeed(results interface{}) error {
	ctx, cancel := context.WithCancel(taskc.ctx)
	defer cancel()

	jsonResults, err := jsonify(results)
	if err != nil {
		// NOTE(ssd) 2019-05-08: This kills the transacation,
		// do we want that?
		return errors.Wrap(err, "could not convert results to JSON")
	}

	_, err = taskc.tx.ExecContext(ctx, completeTaskQuery, taskc.tid, taskStatusSuccess, "", jsonResults)
	if err != nil {
		return errors.Wrapf(err, "failed to mark task %d as successful", taskc.tid)
	}

	return errors.Wrapf(taskc.tx.Commit(), "failed to mark task %d as successful", taskc.tid)
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
