package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/golang/protobuf/ptypes"
	w "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/server"
)

const habConfigDir = "/hab/svc/applications-service/config"

func TestPeriodicDisconnectedServices(t *testing.T) {

	ctx := context.Background()

	err := suite.JobScheduler.ResetParams()
	require.NoError(t, err)

	// Re-enable
	suite.JobScheduler.EnableDisconnectedServicesJob(ctx)
	defer suite.JobScheduler.DisableDisconnectedServicesJob(ctx)

	// We can only start the job runners with cereal once so we do it here.
	runners := server.NewJobRunnerSet(suite.ApplicationsServer)
	err = runners.Start(suite.JobScheduler.CerealSvc)
	require.NoError(t, err)

	t.Run("workflow schedule should exist for disconnected_services", func(t *testing.T) {
		sched, err := suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, server.DisconnectedServicesScheduleName, server.DisconnectedServicesWorkflowName)

		require.NoError(t, err)
		assert.Equal(t, "disconnected_services", sched.WorkflowName)
		assert.True(t, sched.Enabled)
	})

	t.Run("update disconnected_services job params", func(t *testing.T) {
		defer suite.JobScheduler.ResetParams()

		req := &applications.PeriodicMandatoryJobConfig{
			Threshold:  "23s",
			Running:    &w.BoolValue{Value: true},
			Recurrence: "FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=61",
		}
		_, err := suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err := suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "23s", conf.Threshold)
		assert.Equal(t, "FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=61", conf.Recurrence)
		assert.True(t, conf.Running.Value)

		// Update the params again to ensure we didn't accidentally pass the test due to leftover state somewhere:
		req = &applications.PeriodicMandatoryJobConfig{
			Threshold:  "42s",
			Running:    &w.BoolValue{Value: false},
			Recurrence: "FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=62",
		}
		_, err = suite.ApplicationsServer.UpdateDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err = suite.ApplicationsServer.GetDisconnectedServicesConfig(ctx, &applications.GetDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "42s", conf.Threshold)
		assert.Equal(t, "FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=62", conf.Recurrence)
		assert.False(t, conf.Running.Value)
	})

	t.Run("running the job runner makes the disconnected_services job run", func(t *testing.T) {
		suite.JobScheduler.ResetParams()
		err := suite.JobScheduler.RunAllJobsConstantly(ctx)
		require.NoError(t, err)
		defer suite.JobScheduler.ResetParams()

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

		// Patch event timestamp to mock an old service message and mark it as disconnected
		event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 10))
		require.NoError(t, err)
		suite.IngestService(event)

		runsThusFar := runners.MarkDisconnectedServicesExecutor.TotalRuns()
		detectedJobRun := false
		for i := 0; i <= 100; i++ {
			// Wait for the job to run twice. There is race condition potential with
			// this test reconfiguring the job constantly and cereal concurrently
			// starting tasks.
			// We can be pretty sure things are good if the job runs twice.
			if runners.MarkDisconnectedServicesExecutor.TotalRuns() > (runsThusFar + 1) {
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
		require.Equal(t, 1, len(response.GetServices()))
		assert.True(t, response.GetServices()[0].Disconnected)
	})

	t.Run("workflow schedule should exist for delete_disconnected_services", func(t *testing.T) {
		sched, err := suite.JobScheduler.CerealSvc.GetWorkflowScheduleByName(ctx, server.DeleteDisconnectedServicesScheduleName, server.DeleteDisconnectedServicesWorkflowName)

		require.NoError(t, err)
		assert.Equal(t, "delete_disconnected_services", sched.WorkflowName)
		assert.True(t, sched.Enabled)
	})

	t.Run("disable and enable delete_disconnected_services job", func(t *testing.T) {
		req := &applications.PeriodicJobConfig{Threshold: "7d", Running: false}
		_, err := suite.ApplicationsServer.UpdateDeleteDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err := suite.ApplicationsServer.GetDeleteDisconnectedServicesConfig(ctx, &applications.GetDeleteDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.False(t, conf.Running)

		req.Running = true
		_, err = suite.ApplicationsServer.UpdateDeleteDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err = suite.ApplicationsServer.GetDeleteDisconnectedServicesConfig(ctx, &applications.GetDeleteDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.True(t, conf.Running)
	})

	t.Run("update delete_disconnected_services job params", func(t *testing.T) {
		defer suite.JobScheduler.ResetParams()

		req := &applications.PeriodicJobConfig{Threshold: "23s", Running: true}
		_, err := suite.ApplicationsServer.UpdateDeleteDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err := suite.ApplicationsServer.GetDeleteDisconnectedServicesConfig(ctx, &applications.GetDeleteDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "23s", conf.Threshold)

		// Update the params again to ensure we didn't accidentally pass the test due to leftover state somewhere:
		req = &applications.PeriodicJobConfig{Threshold: "42s", Running: true}
		_, err = suite.ApplicationsServer.UpdateDeleteDisconnectedServicesConfig(ctx, req)
		require.NoError(t, err)

		conf, err = suite.ApplicationsServer.GetDeleteDisconnectedServicesConfig(ctx, &applications.GetDeleteDisconnectedServicesConfigReq{})
		require.NoError(t, err)
		assert.Equal(t, "42s", conf.Threshold)
	})

	t.Run("running the job runner makes the delete_disconnected_services job run", func(t *testing.T) {
		err := suite.JobScheduler.RunAllJobsConstantly(ctx)
		require.NoError(t, err)
		defer suite.JobScheduler.ResetParams()

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

		// Patch event timestamp to mock an old service message which we'll delete for being disconnected
		event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Hour * 24 * 8))
		require.NoError(t, err)
		suite.IngestService(event)

		logrus.Info("Starting check for job runner")
		runsThusFar := runners.DeleteDisconnectedServicesExecutor.TotalRuns()
		detectedJobRun := false
		for i := 0; i <= 100; i++ {
			if runners.DeleteDisconnectedServicesExecutor.TotalRuns() > runsThusFar {
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
		assert.Equal(t, 0, len(response.GetServices()))
	})

}
