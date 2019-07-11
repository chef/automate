// build +integration
package postgres

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/sirupsen/logrus"

	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
	"github.com/chef/automate/lib/platform/pg"
)

const (
	defaultTestDatabaseName  = "cereal_test"
	defaultAdminDatabaseName = "template1"
)

var defaultA2ConnInfo = pg.A2ConnInfo{
	Host:  "localhost",
	Port:  5432,
	User:  "automate",
	Certs: pg.A2SuperuserCerts,
}

func adminDBURL() string {
	if os.Getenv("PG_ADMIN_URL") != "" {
		return os.Getenv("PG_ADMIN_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultAdminDatabaseName)
}

func testDBURL() string {
	if os.Getenv("PG_URL") != "" {
		return os.Getenv("PG_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultTestDatabaseName)
}

func runResetDB() error {
	db, err := sql.Open("postgres", adminDBURL())
	if err != nil {
		return errors.Wrap(err, "could not initialize db connection")
	}
	defer db.Close()
	_, err = db.Exec(pg.DropDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not drop database")
	}
	_, err = db.Exec(pg.CreateDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not create database")
	}
	return nil
}

func TestNoAvailableTasks(t *testing.T) {
	taskName := "task_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	_, _, err = b1.DequeueTask(ctx, taskName)
	require.Equal(t, cereal.ErrNoTasks, err)
}

func TestEnqueueWorkflowInstanceNameUnique(t *testing.T) {
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.Error(t, cereal.ErrWorkflowInstanceExists, err)

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	err = completer.Done(nil)
	require.NoError(t, err)

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance1",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")
}

func TestMultipleTasksCanDequeueConcurrently(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	b2 := NewPostgresBackend(testDBURL())
	err = b2.Init()
	require.NoError(t, err)
	defer b2.Close()

	b3 := NewPostgresBackend(testDBURL())
	err = b3.Init()
	require.NoError(t, err)
	defer b3.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	tx1, err := b1.db.BeginTx(ctx, nil)
	require.NoError(t, err)

	tx2, err := b2.db.BeginTx(ctx, nil)
	require.NoError(t, err)

	tx3, err := b3.db.BeginTx(ctx, nil)
	require.NoError(t, err)

	b1TID, _, err := b1.dequeueTask(tx1, taskName)
	require.NoError(t, err)

	b2TID, _, err := b2.dequeueTask(tx2, taskName)
	require.NoError(t, err)

	_, _, err = b3.dequeueTask(tx3, taskName)
	require.Equal(t, cereal.ErrNoTasks, err)

	assert.NotZero(t, b1TID)
	assert.NotZero(t, b2TID)
	assert.NotEqual(t, b1TID, b2TID)

	err = tx1.Commit()
	require.NoError(t, err)

	err = tx2.Commit()
	require.NoError(t, err)
}

func TestTaskComplete(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed([]byte("foo"))

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)
	require.Equal(t, []byte("foo"), wevt.TaskResult.Result)
	require.Equal(t, "", wevt.TaskResult.ErrorText)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)
}

func TestTaskFail(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Fail("foo")

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusFailed, wevt.TaskResult.Status)
	require.Empty(t, wevt.TaskResult.Result)
	require.Equal(t, "foo", wevt.TaskResult.ErrorText)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)
}

func TestWorkflowsRerun(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	b2 := NewPostgresBackend(testDBURL())
	err = b2.Init()
	require.NoError(t, err)
	defer b2.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})
	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed([]byte("foo"))

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)
	require.Equal(t, 1, wevt.CompletedTaskCount)
	require.Equal(t, 2, wevt.EnqueuedTaskCount)
	require.Equal(t, []byte("foo"), wevt.TaskResult.Result)
	require.Equal(t, "", wevt.TaskResult.ErrorText)

	err = completer.Close()
	require.NoError(t, err)

	wevt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)
	require.Equal(t, 1, wevt.CompletedTaskCount)
	require.Equal(t, 2, wevt.EnqueuedTaskCount)
	require.Equal(t, []byte("foo"), wevt.TaskResult.Result)
	require.Equal(t, "", wevt.TaskResult.ErrorText)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)
}

func TestLostWorkOnTaskSuccess(t *testing.T) {
	// This test creates a workflow that launches a task that will succeed.
	// Before it can succeed, the task is expired. We expect that the result
	// from the task is not written back.

	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	// start work on task
	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	// it's taken too long an has been marked expired
	err = b1.cleaner.expireDeadTasks(ctx, 0)
	require.NoError(t, err)

	// task finishes, but cant write back because the task was expired
	err = taskCompleter.Succeed([]byte("foo"))
	require.Equal(t, cereal.ErrTaskLost, err)

	// Make sure we get a message indicating failure because of lost work
	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusLost, wevt.TaskResult.Status)
	require.Nil(t, wevt.TaskResult.Result)
	require.Equal(t, "", wevt.TaskResult.ErrorText)
}

func TestLostWorkOnTaskFail(t *testing.T) {
	// This test creates a workflow that launches a task that will fail.
	// Before it can fail, the task is expired. We expect that the error
	// from the task is not written back.

	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	// start work on task
	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	// it's taken too long an has been marked expired
	err = b1.cleaner.expireDeadTasks(ctx, 0)
	require.NoError(t, err)

	// task finishes, but cant write back because the task was expired
	err = taskCompleter.Fail("fail")
	require.Equal(t, cereal.ErrTaskLost, err)

	// Make sure we get a message indicating failure because of lost work
	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusLost, wevt.TaskResult.Status)
	require.Equal(t, "", wevt.TaskResult.ErrorText)
}

func TestNoLostWorkOnTaskSuccess(t *testing.T) {
	// This test creates a successful workflow and then tries to
	// expire work. Because the workflow and all its tasks will
	// have finished when we check for expired work, there should
	// be no new workflows due after.

	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	// start work on task
	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	// task finishes, expect write back
	err = taskCompleter.Succeed(nil)
	require.NoError(t, err)

	// Make sure we get a message indicating failure because of lost work
	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)

	err = b1.cleaner.expireDeadTasks(ctx, 0)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Error(t, cereal.ErrNoWorkflowInstances)
}

func TestTaskPinger(t *testing.T) {
	// This test creates a workflow that launches a task that will succeed.
	// The task is slow, and we make sure pinging it keeps it alive

	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	// start work on task
	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	pgCompleter := (taskCompleter).(*PostgresTaskCompleter)

	for i := 0; i < 5; i++ {
		pgCompleter.pinger.ping(ctx)
		err = b1.cleaner.expireDeadTasks(ctx, 2)
		require.NoError(t, err)
		time.Sleep(1 * time.Second)
	}

	err = taskCompleter.Succeed([]byte("foo"))
	require.NoError(t, err)

	// Make sure we get a message indicating failure because of lost work
	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)
}

func TestTaskCompleteWithPendingTasks(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed(nil)

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueTask(ctx, taskName)
	require.Error(t, cereal.ErrNoTasks)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCompleteWithCompletingTask(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed(nil)

	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)

	err = taskCompleter.Succeed(nil)
	require.NoError(t, err)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCompleteWithUnprocessedTask(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	err = taskCompleter.Succeed(nil)
	require.NoError(t, err)

	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	err = taskCompleter.Succeed(nil)
	require.NoError(t, err)

	wevt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)

	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Equal(t, cereal.ErrNoWorkflowInstances, err)

	assertNoPendingWork(t, b1)
}

func assertNoPendingWork(t *testing.T, b *PostgresBackend) {
	t.Helper()

	tables := []string{
		"cereal_tasks",
		"cereal_workflow_events",
		"cereal_workflow_instances",
		"cereal_task_results",
	}
	for _, table := range tables {
		rows := b.db.QueryRow(fmt.Sprintf("SELECT count(*) from %s;", table))
		var count int64
		err := rows.Scan(&count)
		require.NoError(t, err, "failed to get count for %s", table)
		require.Zero(t, count, "table %s has unexpected entries", table)
	}

}

func init() {
	logrus.SetLevel(logrus.DebugLevel)
}
