// +build integration

package integration

import (
	"context"
	"testing"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	grpccereal "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/components/cereal-service/pkg/server"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
	libgrpc "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestDomains(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	logrus.SetLevel(logrus.DebugLevel)
	require.NoError(t, runResetDB())
	pgBackend := postgres.NewPostgresBackend(testDBURL(), postgres.WithTaskPingInterval(3*time.Second))
	require.NoError(t, pgBackend.Init())

	grpcServer := grpc.NewServer()
	svc := server.NewCerealService(ctx, pgBackend)
	grpccereal.RegisterCerealServer(grpcServer, svc)
	g := grpctest.NewServer(grpcServer)
	cereal.MaxWakeupInterval = 2 * time.Second
	defer pgBackend.Close()
	defer g.Close()

	conn, err := grpc.Dial(g.URL, grpc.WithInsecure(), grpc.WithMaxMsgSize(64*1024*1024))
	require.NoError(t, err)

	t.Run("RPCs fail if domain is missing", func(t *testing.T) {
		workflowName := "mytestworkflow"
		instanceName := "mytestinstance"
		taskName := "mytaskname"
		grpcBackend := libgrpc.NewGrpcBackendFromConn("", conn)
		rpcs := []struct {
			Name string
			Call func() error
		}{
			{
				Name: "EnqueueWorkflow",
				Call: func() error {
					return grpcBackend.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
						WorkflowName: workflowName,
						InstanceName: instanceName,
					})
				},
			},
			{
				Name: "DequeueWorkflow",
				Call: func() error {
					_, completer, err := grpcBackend.DequeueWorkflow(ctx, []string{workflowName})
					if err != nil {
						return err
					}
					completer.Close()
					return nil
				},
			},
			{
				Name: "CancelWorkflow",
				Call: func() error {
					return grpcBackend.CancelWorkflow(ctx, instanceName, workflowName)
				},
			},
			{
				Name: "DequeueTask",
				Call: func() error {
					_, completer, err := grpcBackend.DequeueTask(ctx, taskName)
					if err != nil {
						return err
					}
					completer.Fail("should not happen")
					return nil
				},
			},
			{
				Name: "CreateWorkflowSchedule",
				Call: func() error {
					return grpcBackend.CreateWorkflowSchedule(ctx, instanceName,
						workflowName, nil, false, "FREQ=HOURLY;INTERVAL=1", time.Now())
				},
			},
			{
				Name: "ListWorkflowSchedules",
				Call: func() error {
					_, err := grpcBackend.ListWorkflowSchedules(ctx)
					return err
				},
			},
			{
				Name: "GetWorkflowScheduleByName",
				Call: func() error {
					_, err := grpcBackend.GetWorkflowScheduleByName(ctx, instanceName, workflowName)
					return err
				},
			},
			{
				Name: "UpdateWorkflowScheduleByName",
				Call: func() error {
					err := grpcBackend.UpdateWorkflowScheduleByName(ctx, instanceName, workflowName, backend.WorkflowScheduleUpdateOpts{})
					return err
				},
			},
			{
				Name: "GetWorkflowInstanceByName",
				Call: func() error {
					_, err := grpcBackend.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
					return err
				},
			},
			{
				Name: "ListWorkflowInstances",
				Call: func() error {
					_, err := grpcBackend.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
					return err
				},
			},
		}
		for _, rpc := range rpcs {
			t.Run(rpc.Name, func(t *testing.T) {
				err := rpc.Call()
				require.Error(t, err)
				s, ok := status.FromError(err)
				require.True(t, ok, "did not get a grpc status")
				require.Equal(t, codes.InvalidArgument, s.Code())
				assert.Contains(t, s.Err().Error(), "must specify domain")
			})
		}
	})

	t.Run("Workflow Domain Usage", func(t *testing.T) {
		workflowName := "mytestworkflow"
		instanceName := "mytestinstance"
		taskName := "mytaskname"
		grpcBackendDomain1 := libgrpc.NewGrpcBackendFromConn("domain1", conn)
		grpcBackendDomain2 := libgrpc.NewGrpcBackendFromConn("domain2", conn)
		domain1Params := []byte("domain1workflow")
		domain2Params := []byte("domain2workflow")
		domain1TaskParams := []byte("domain1task")
		domain2TaskParams := []byte("domain2task")

		// Enqueue workflow with same name in different domains
		err := grpcBackendDomain1.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
			WorkflowName: workflowName,
			InstanceName: instanceName,
			Parameters:   domain1Params,
		})
		require.NoError(t, err)

		domain2Instances, err := grpcBackendDomain2.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
		require.NoError(t, err)
		require.Len(t, domain2Instances, 0)

		err = grpcBackendDomain2.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
			WorkflowName: workflowName,
			InstanceName: instanceName,
			Parameters:   domain2Params,
		})
		require.NoError(t, err)

		domain1Instances, err := grpcBackendDomain1.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
		require.NoError(t, err)
		require.Len(t, domain1Instances, 1)
		validateInstanceMatches(t, domain1Instances[0], workflowName, instanceName, domain1Params)

		domain2Instances, err = grpcBackendDomain2.ListWorkflowInstances(ctx, backend.ListWorkflowOpts{})
		require.NoError(t, err)
		require.Len(t, domain2Instances, 1)
		validateInstanceMatches(t, domain2Instances[0], workflowName, instanceName, domain2Params)

		domain2Instance, err := grpcBackendDomain2.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
		require.NoError(t, err)
		validateInstanceMatches(t, domain2Instance, workflowName, instanceName, domain2Params)

		// Dequeue workflows and enqueue tasks
		domain2Evt, workflowCompleter2, err := grpcBackendDomain2.DequeueWorkflow(ctx, []string{workflowName})
		require.NoError(t, err)
		validateInstanceMatches(t, &domain2Evt.Instance, workflowName, instanceName, domain2Params)
		err = workflowCompleter2.EnqueueTask(&backend.Task{
			Name:       taskName,
			Parameters: domain2TaskParams,
		}, backend.TaskEnqueueOpts{})
		require.NoError(t, err)
		err = workflowCompleter2.Continue(nil)
		require.NoError(t, err)
		_, _, err = grpcBackendDomain2.DequeueWorkflow(ctx, []string{workflowName})
		require.Equal(t, cereal.ErrNoWorkflowInstances, err)

		domain1Evt, workflowCompleter1, err := grpcBackendDomain1.DequeueWorkflow(ctx, []string{workflowName})
		require.NoError(t, err)
		validateInstanceMatches(t, &domain1Evt.Instance, workflowName, instanceName, domain1Params)
		err = workflowCompleter1.EnqueueTask(&backend.Task{
			Name:       taskName,
			Parameters: domain1TaskParams,
		}, backend.TaskEnqueueOpts{})
		require.NoError(t, err)
		err = workflowCompleter1.Continue(nil)
		require.NoError(t, err)
		_, _, err = grpcBackendDomain1.DequeueWorkflow(ctx, []string{workflowName})
		require.Equal(t, cereal.ErrNoWorkflowInstances, err)

		// Dequeue Tasks
		task1, taskCompleter1, err := grpcBackendDomain1.DequeueTask(ctx, taskName)
		require.NoError(t, err)
		validateTaskMatches(t, task1, taskName, domain1TaskParams)
		err = taskCompleter1.Succeed(nil)
		require.NoError(t, err)
		_, _, err = grpcBackendDomain1.DequeueTask(ctx, taskName)
		require.Equal(t, cereal.ErrNoTasks, err)

		task2, taskCompleter2, err := grpcBackendDomain2.DequeueTask(ctx, taskName)
		require.NoError(t, err)
		validateTaskMatches(t, task2, taskName, domain2TaskParams)
		err = taskCompleter2.Succeed(nil)
		require.NoError(t, err)
		_, _, err = grpcBackendDomain2.DequeueTask(ctx, taskName)
		require.Equal(t, cereal.ErrNoTasks, err)

		// Finish workflows
		domain2Evt, workflowCompleter2, err = grpcBackendDomain2.DequeueWorkflow(ctx, []string{workflowName})
		require.NoError(t, err)
		validateInstanceMatches(t, &domain2Evt.Instance, workflowName, instanceName, domain2Params)
		require.Equal(t, domain2TaskParams, domain2Evt.TaskResult.Parameters)
		err = workflowCompleter2.Done(nil)
		require.NoError(t, err)

		domain1Evt, workflowCompleter1, err = grpcBackendDomain1.DequeueWorkflow(ctx, []string{workflowName})
		require.NoError(t, err)
		validateInstanceMatches(t, &domain1Evt.Instance, workflowName, instanceName, domain1Params)
		require.Equal(t, domain1TaskParams, domain1Evt.TaskResult.Parameters)
		err = workflowCompleter1.Done(nil)
		require.NoError(t, err)
	})

	t.Run("Schedules", func(t *testing.T) {
		workflowName := "mytestschedulesworkflow"
		instanceName := "mytestinstance"
		grpcBackendDomain1 := libgrpc.NewGrpcBackendFromConn("domain1", conn)
		grpcBackendDomain2 := libgrpc.NewGrpcBackendFromConn("domain2", conn)
		domain1Params := []byte("domain1workflow")
		domain2Params := []byte("domain2workflow")

		err := grpcBackendDomain1.CreateWorkflowSchedule(ctx, instanceName,
			workflowName, domain1Params, true, "FREQ=HOURLY;INTERVAL=1", time.Now())
		require.NoError(t, err)

		err = grpcBackendDomain2.CreateWorkflowSchedule(ctx, instanceName,
			workflowName, domain2Params, true, "FREQ=HOURLY;INTERVAL=1", time.Now())
		require.NoError(t, err)

		schedulesDomain1, err := grpcBackendDomain1.ListWorkflowSchedules(ctx)
		require.NoError(t, err)
		require.Len(t, schedulesDomain1, 1)
		validateScheduleMatches(t, schedulesDomain1[0], workflowName, instanceName, domain1Params)

		schedulesDomain2, err := grpcBackendDomain2.ListWorkflowSchedules(ctx)
		require.NoError(t, err)
		require.Len(t, schedulesDomain2, 1)
		validateScheduleMatches(t, schedulesDomain2[0], workflowName, instanceName, domain2Params)

		// Test updating the schedule
		domain1Params = []byte("domain1workflow-updated")
		err = grpcBackendDomain1.UpdateWorkflowScheduleByName(ctx, instanceName, workflowName, backend.WorkflowScheduleUpdateOpts{
			UpdateParameters: true,
			Parameters:       domain1Params,
		})
		require.NoError(t, err)

		domain2Params = []byte("domain2workflow-updated")
		err = grpcBackendDomain2.UpdateWorkflowScheduleByName(ctx, instanceName, workflowName, backend.WorkflowScheduleUpdateOpts{
			UpdateParameters: true,
			Parameters:       domain2Params,
		})
		require.NoError(t, err)

		schedulesDomain1, err = grpcBackendDomain1.ListWorkflowSchedules(ctx)
		require.NoError(t, err)
		require.Len(t, schedulesDomain1, 1)
		validateScheduleMatches(t, schedulesDomain1[0], workflowName, instanceName, domain1Params)

		schedulesDomain2, err = grpcBackendDomain2.ListWorkflowSchedules(ctx)
		require.NoError(t, err)
		require.Len(t, schedulesDomain2, 1)
		validateScheduleMatches(t, schedulesDomain2[0], workflowName, instanceName, domain2Params)
	})
}

func validateInstanceMatches(t *testing.T, instance *backend.WorkflowInstance, workflowName string, instanceName string, params []byte) {
	t.Helper()
	require.Equal(t, workflowName, instance.WorkflowName)
	require.Equal(t, instanceName, instance.InstanceName)
	require.Equal(t, params, instance.Parameters)
}

func validateTaskMatches(t *testing.T, task *backend.Task, taskName string, taskParams []byte) {
	t.Helper()
	require.Equal(t, taskName, task.Name)
	require.Equal(t, taskParams, task.Parameters)
}

func validateScheduleMatches(t *testing.T, schedule *backend.Schedule, workflowName string, instanceName string, params []byte) {
	t.Helper()
	require.Equal(t, workflowName, schedule.WorkflowName)
	require.Equal(t, instanceName, schedule.InstanceName)
	require.Equal(t, params, schedule.Parameters)
}
