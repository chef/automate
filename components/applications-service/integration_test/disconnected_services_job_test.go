package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/server"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

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

	t.Run("disable and enable disconnected_services job", func(t *testing.T) {
		req := &applications.UpdateDisconnectedServicesConfigReq{Threshold: "5m", Running: false}
		_, err := suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err := suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.False(t, conf.Running)

		req.Running = true
		_, err = suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err = suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.True(t, conf.Running)

	})
	t.Run("update disconnected_services job params", func(t *testing.T) {
		req := &applications.UpdateDisconnectedServicesConfigReq{Threshold: "23s", Running: true}
		_, err := suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err := suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "23s", conf.Threshold)

		// Update the params again to ensure we didn't accidentally pass the test due to leftover state somewhere:
		req = &applications.UpdateDisconnectedServicesConfigReq{Threshold: "42s", Running: true}
		_, err = suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err = suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "42s", conf.Threshold)
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

		logrus.Info("Starting check for job runner")
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
