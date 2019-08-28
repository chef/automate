package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/server"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// CONFIGURE PERIODIC DELETION OF DISCONNECTED SERVICES:
// * this config lives with the cereal service in the job config or something like that
// The way it's updated is:
// (m *Manager) UpdateWorkflowScheduleByName(ctx context.Context,
// 	instanceName string, workflowName string, opts ...WorkflowScheduleUpdateOpts)
// need to ensure it exists on startup I guess...

const (
	DisconnectedServicesJobName      = "disconnected_services"
	DisconnectedServicesScheduleName = "periodic_disconnected_services"
)

const habConfigDir = "/hab/svc/applications-service/config"

func TestPeriodicDisconnectedServices(t *testing.T) {

	ctx := context.Background()
	sched, err := suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)

	require.NoError(t, err)
	assert.Equal(t, "disconnected_services", sched.WorkflowName)
	assert.True(t, sched.Enabled)

	time.Sleep(2 * time.Second)

	t.Run("disable and enable disconnected_services job", func(t *testing.T) {
		req := &applications.UpdateDisconnectedServicesConfigReq{Threshold: "5m", Running: false}
		_, err := suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		sched, err := suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)
		assert.False(t, sched.Enabled)

		req.Running = true
		_, err = suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		sched, err = suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)
		assert.True(t, sched.Enabled)

	})
	t.Run("update disconnected_services job params", func(t *testing.T) {
		req := &applications.UpdateDisconnectedServicesConfigReq{Threshold: "23s", Running: true}
		_, err := suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		sched, err := suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)

		var returnedParams server.DisconnectedServicesParamsV0
		sched.GetParameters(&returnedParams)
		assert.Equal(t, 23, returnedParams.ThresholdSeconds)

		// Update the params again to ensure we didn't accidentally pass the test due to leftover state somewhere:
		req = &applications.UpdateDisconnectedServicesConfigReq{Threshold: "42s", Running: true}
		_, err = suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		sched, err = suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)

		sched.GetParameters(&returnedParams)
		assert.Equal(t, 42, returnedParams.ThresholdSeconds)
	})

	t.Run("running the job runner makes the jobs run", func(t *testing.T) {
		err := suite.JobScheduler.RunAllJobsConstantly(ctx)
		require.NoError(t, err)

		// * have a way to track number of job runs (prometheus (?))
		defer suite.DeleteDataFromStorage()

		event := NewHabitatEvent(
			withSupervisorId("abcd"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withStrategyAtOnce("testchannel"),
			withFqdn("mytest.example.com"),
			withSite("testsite"),
		)

		// Patch event timestamp to mock an old service message and mack it as disconnected
		event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 10))
		require.NoError(t, err)
		suite.IngestService(event)

		runners := server.NewJobRunnerSet(suite.ApplicationsServer)
		err = runners.Start(suite.JobScheduler.CerealSvc)
		require.NoError(t, err)

		runsThusFar := runners.MarkDisconnectedServicesExecutor.TotalRuns()
		detectedJobRun := false
		for i := 0; i <= 100; i++ {
			if runners.MarkDisconnectedServicesExecutor.TotalRuns() > runsThusFar {
				detectedJobRun = true
				break
			}
			time.Sleep(50 * time.Millisecond)
		}
		if !detectedJobRun {
			assert.Fail(t, "disconnected_services runner didn't run in the alloted time")
		}

		request := &applications.DisconnectedServicesReq{ThresholdSeconds: 180}
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assert.Equal(t, 1, len(response.GetServices()))
		assert.True(t, response.GetServices()[0].Disconnected)
	})

}
