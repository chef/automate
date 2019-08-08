// +build integration

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

func TestDelayedTask(t *testing.T) {
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

	expectedTime := time.Now().Add(3 * time.Second)
	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{
		StartAfter: expectedTime,
	})
	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{
		StartAfter: expectedTime.In(time.FixedZone("AEST", 10*60*60)),
	})
	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{
		StartAfter: expectedTime.In(time.FixedZone("HST", -10*60*60)),
	})
	completer.Continue(nil)

	_, taskCompleter, err := b1.DequeueTask(ctx, taskName)
	require.Equal(t, cereal.ErrNoTasks, err)

	time.Sleep(4 * time.Second)

	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed([]byte("foo"))

	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	taskCompleter.Succeed([]byte("foo"))
	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed([]byte("foo"))

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

func TestWorkflowCompleteWithPendingTasks(t *testing.T) {
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

	// make sure we can run it again. There was a bug where we couldn't
	// run any more workflows correctly after one completed early
	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: "workflow-instance",
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	_, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})
	completer.Continue(nil)

	_, taskCompleter, err = b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	taskCompleter.Succeed(nil)

	wevt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.TaskComplete, wevt.Type)
	require.Equal(t, backend.TaskStatusSuccess, wevt.TaskResult.Status)
	err = completer.Done(nil)
	require.NoError(t, err)

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

func TestWorkflowCancellationWithPendingTasks(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	instanceName := "workflow-instance"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	evt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowStart, evt.Type)
	require.Equal(t, backend.WorkflowInstanceStatusStarting, evt.Instance.Status)

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	err = completer.Continue(nil)
	require.NoError(t, err)

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	evt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowCancel, evt.Type)
	require.Equal(t, backend.WorkflowInstanceStatusRunning, evt.Instance.Status)
	err = completer.Done(nil)
	require.NoError(t, err)

	_, _, err = b1.DequeueTask(ctx, taskName)
	require.Error(t, cereal.ErrNoTasks)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCancellationWithRunningTasks(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	instanceName := "workflow-instance"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL(), WithTaskPingInterval(3*time.Second))
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	evt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowStart, evt.Type)
	require.Equal(t, backend.WorkflowInstanceStatusStarting, evt.Instance.Status)

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	err = completer.Continue(nil)
	require.NoError(t, err)

	_, taskCompleter1, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	_, taskCompleter2, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	evt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowCancel, evt.Type)
	err = completer.Done(nil)
	require.NoError(t, err)

	<-taskCompleter1.Context().Done()
	err = taskCompleter1.Succeed(nil)
	require.Equal(t, cereal.ErrTaskLost, err)

	<-taskCompleter2.Context().Done()
	err = taskCompleter2.Fail("err")
	require.Equal(t, cereal.ErrTaskLost, err)

	_, _, err = b1.DequeueTask(ctx, taskName)
	require.Error(t, cereal.ErrNoTasks)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCancellationWithCompletedTasks(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	instanceName := "workflow-instance"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL(), WithTaskPingInterval(3*time.Second))
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	evt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowStart, evt.Type)
	require.Equal(t, backend.WorkflowInstanceStatusStarting, evt.Instance.Status)

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	err = completer.Continue(nil)
	require.NoError(t, err)

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	_, taskCompleter1, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	err = taskCompleter1.Succeed(nil)
	require.NoError(t, err)

	_, taskCompleter2, err := b1.DequeueTask(ctx, taskName)
	require.NoError(t, err)
	err = taskCompleter2.Fail("err")
	require.NoError(t, err)

	evt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	// It's possible the next line fails, but it will be very rare if it does.
	// cancel might not be delivered in the expected order as it is an external
	// event.
	require.Equal(t, backend.WorkflowCancel, evt.Type)
	err = completer.Done(nil)
	require.NoError(t, err)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCancellationDedup(t *testing.T) {
	taskName := "task_name"
	workflowName := "workflow_name"
	instanceName := "workflow-instance"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL(), WithTaskPingInterval(3*time.Second))
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})

	require.NoError(t, err, "failed to enqueue workflow")

	evt, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowStart, evt.Type)
	require.Equal(t, backend.WorkflowInstanceStatusStarting, evt.Instance.Status)

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	completer.EnqueueTask(&backend.Task{
		Name: taskName,
	}, backend.TaskEnqueueOpts{})

	err = completer.Continue(nil)
	require.NoError(t, err)

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	evt, completer, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err, "failed to dequeue workflow")
	require.Equal(t, backend.WorkflowCancel, evt.Type)
	err = completer.Done(nil)

	require.NoError(t, err)

	assertNoPendingWork(t, b1)
}

func TestWorkflowCancellationBeforeStart(t *testing.T) {
	workflowName := "workflow_name"
	instanceName := "workflow-instance"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL(), WithTaskPingInterval(3*time.Second))
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})
	require.NoError(t, err, "failed to enqueue workflow")

	err = b1.CancelWorkflow(ctx, instanceName, workflowName)
	require.NoError(t, err)

	_, err = b1.db.ExecContext(ctx, "UPDATE cereal_workflow_events SET enqueued_at = NOW() WHERE event_type = 'start'")
	require.NoError(t, err)

	_, _, err = b1.DequeueWorkflow(ctx, []string{workflowName})
	require.Error(t, cereal.ErrNoDueWorkflows)

	assertNoPendingWork(t, b1)
}

func TestListWorkflowInstancesEmpty(t *testing.T) {
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	instances, err := b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
	require.NoError(t, err)
	assert.Len(t, instances, 0)
}

func TestListWorkflowInstancesMultipleInstances(t *testing.T) {
	workflowName := "workflow_name"
	err := runResetDB()
	require.NoError(t, err)
	b1 := NewPostgresBackend(testDBURL())
	err = b1.Init()
	require.NoError(t, err)
	defer b1.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	instance1Name := "instance1"
	instance1Parameters := []byte("instance1Parameters")
	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instance1Name,
		WorkflowName: workflowName,
		Parameters:   instance1Parameters,
	})
	require.NoError(t, err)
	instance2Name := "instance2"
	instance2Parameters := []byte("instance2Parameters")
	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instance2Name,
		WorkflowName: workflowName,
		Parameters:   instance2Parameters,
	})
	require.NoError(t, err)

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instance1Name,
		WorkflowName: "someotherworkflow",
		Parameters:   nil,
	})
	require.NoError(t, err)

	instances, err := b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
	require.NoError(t, err)
	assert.Len(t, instances, 3)

	for _, instance := range instances {
		assert.Equal(t, backend.WorkflowInstanceStatusStarting, instance.Status)
		if instance.WorkflowName == workflowName {
			if instance.InstanceName == instance1Name {
				assert.Equal(t, instance1Parameters, instance.Parameters)
			} else {
				assert.Equal(t, instance2Parameters, instance.Parameters)
			}
		}
	}

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
	})
	require.NoError(t, err)
	assert.Len(t, instances, 2)

	for _, instance := range instances {
		assert.Equal(t, backend.WorkflowInstanceStatusStarting, instance.Status)
		if instance.InstanceName == instance1Name {
			assert.Equal(t, instance1Parameters, instance.Parameters)
		} else {
			assert.Equal(t, instance2Parameters, instance.Parameters)
		}
	}

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &instance1Name,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusStarting, instances[0].Status)
	assert.Equal(t, instance1Parameters, instances[0].Parameters)

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &instance2Name,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusStarting, instances[0].Status)
	assert.Equal(t, instance2Parameters, instances[0].Parameters)

	isRunning := true
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		IsRunning:    &isRunning,
	})
	require.NoError(t, err)
	assert.Len(t, instances, 2)

	for _, instance := range instances {
		assert.Equal(t, backend.WorkflowInstanceStatusStarting, instance.Status)
		if instance.InstanceName == instance1Name {
			assert.Equal(t, instance1Parameters, instance.Parameters)
		} else {
			assert.Equal(t, instance2Parameters, instance.Parameters)
		}
	}

	isRunning = false
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		IsRunning:    &isRunning,
	})
	require.NoError(t, err)
	assert.Len(t, instances, 0)

	w1, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	err = completer.Done([]byte("result"))
	require.NoError(t, err)
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		IsRunning:    &isRunning,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusCompleted, instances[0].Status)
	assert.Equal(t, w1.Instance.WorkflowName, instances[0].WorkflowName)
	assert.Equal(t, w1.Instance.InstanceName, instances[0].InstanceName)
	assert.Equal(t, w1.Instance.Parameters, instances[0].Parameters)
	assert.Equal(t, []byte("result"), instances[0].Result)

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &w1.Instance.InstanceName,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusCompleted, instances[0].Status)
	assert.Equal(t, w1.Instance.WorkflowName, instances[0].WorkflowName)
	assert.Equal(t, w1.Instance.InstanceName, instances[0].InstanceName)
	assert.Equal(t, w1.Instance.Parameters, instances[0].Parameters)
	assert.Nil(t, instances[0].Payload)
	assert.Equal(t, []byte("result"), instances[0].Result)

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
	})
	require.NoError(t, err)
	require.Len(t, instances, 2)

	isRunning = false
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		IsRunning: &isRunning,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)

	isRunning = true
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		IsRunning: &isRunning,
	})
	require.NoError(t, err)
	require.Len(t, instances, 2)

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
	require.NoError(t, err)
	require.Len(t, instances, 3)

	w2, completer, err := b1.DequeueWorkflow(ctx, []string{workflowName})
	require.NoError(t, err)
	err = completer.Fail(errors.New("fail"))
	require.NoError(t, err)

	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &w2.Instance.InstanceName,
	})

	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusCompleted, instances[0].Status)
	assert.Equal(t, w2.Instance.WorkflowName, instances[0].WorkflowName)
	assert.Equal(t, w2.Instance.InstanceName, instances[0].InstanceName)
	assert.Equal(t, w2.Instance.Parameters, instances[0].Parameters)
	assert.Nil(t, instances[0].Result)
	assert.Nil(t, instances[0].Payload)
	assert.Equal(t, "fail", instances[0].Err.Error())

	isRunning = false
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		IsRunning: &isRunning,
	})
	require.NoError(t, err)
	require.Len(t, instances, 2)

	isRunning = true
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		IsRunning: &isRunning,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)

	instance1Parameters = []byte("instance1Parameters-new")
	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instance1Name,
		WorkflowName: workflowName,
		Parameters:   instance1Parameters,
	})
	require.NoError(t, err)
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &instance1Name,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusStarting, instances[0].Status)
	assert.Equal(t, instance1Parameters, instances[0].Parameters)
	assert.Nil(t, instances[0].Result)
	assert.Nil(t, instances[0].Err)

	err = b1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: instance2Name,
		WorkflowName: workflowName,
		Parameters:   instance2Parameters,
	})
	require.NoError(t, err)
	instances, err = b1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{
		WorkflowName: &workflowName,
		InstanceName: &instance2Name,
	})
	require.NoError(t, err)
	require.Len(t, instances, 1)
	assert.Equal(t, backend.WorkflowInstanceStatusStarting, instances[0].Status)
	assert.Equal(t, instance2Parameters, instances[0].Parameters)
	assert.Nil(t, instances[0].Result)
	assert.Nil(t, instances[0].Err)
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
