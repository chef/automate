package workflow

import (
	"context"
	"database/sql"
	"encoding/json"
	"time"

	"github.com/pkg/errors"
)

const (
	enqueueTaskQuery  = `SELECT enqueue_task($1, $2, $3, $4, $5)`
	dequeueTaskQuery  = `SELECT dequeue_task($1)`
	completeTaskQuery = `SELECT completed_task($1, $2, $3, $4)`
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
	Fail(err string)
	Succeed(result interface{})
}

type Backend interface {
	EnqueueTask(ctx context.Context, task *Task, opts ...EnqueueOpts) error
	DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error)
}

type PostgresBackend struct {
	db *sql.DB
}

func NewPostgresBackend(connInfo string) (*PostgresBackend, error) {
	db, err := sql.Open("postgres", connInfo)
	if err != nil {
		return nil, err
	}
	return &PostgresBackend{
		db: db,
	}, nil
}

func (pg *PostgresBackend) EnqueueTask(ctx context.Context, task *Task, opts ...EnqueueOpts) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := pg.db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "failed to begin enqueue task transaction")
	}

	o := mergeEnqueueOpts(opts)

	js, err := jsonifyParams(task.Parameters)
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
func jsonifyParams(params interface{}) (string, error) {
	b, err := json.Marshal(params)
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
