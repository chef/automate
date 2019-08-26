package integration_test

import (
	"context"
	"path"
	"testing"
	"time"

	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/server"
	certs "github.com/chef/automate/lib/tls/certs"
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
	certs := studioCerts(t)

	jobCfg := config.Jobs{
		Host: "localhost",
		Port: 10101,
	}

	jobsMgr, err := server.ConnectToJobsManager(&jobCfg, certs)
	require.NoError(t, err)

	scheduler := server.NewJobScheduler(jobsMgr)
	err = scheduler.Setup()
	require.NoError(t, err)

	ctx := context.Background()
	sched, err := scheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)

	require.NoError(t, err)
	assert.Equal(t, "disconnected_services", sched.WorkflowName)
	assert.True(t, sched.Enabled)

	time.Sleep(2 * time.Second)

	t.Run("disable and enable disconnected_services job", func(t *testing.T) {
		err := scheduler.DisableDisconnectedServicesJob(ctx)
		require.NoError(t, err)

		sched, err := scheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)
		assert.False(t, sched.Enabled)

		err = scheduler.EnableDisconnectedServicesJob(ctx)
		require.NoError(t, err)

		sched, err = scheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)
		assert.True(t, sched.Enabled)

	})
	t.Run("update disconnected_services job params", func(t *testing.T) {
		newParams := server.DisconnectedServicesParamsV0{ThresholdSeconds: 23}
		err := scheduler.UpdateDisconnectedServicesJobParams(ctx, &newParams)
		require.NoError(t, err)

		sched, err := scheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)

		var returnedParams server.DisconnectedServicesParamsV0
		sched.GetParameters(&returnedParams)
		assert.Equal(t, 23, returnedParams.ThresholdSeconds)

		// Update the params again to ensure we didn't accidentally pass the test due to leftover state somewhere:
		newParams = server.DisconnectedServicesParamsV0{ThresholdSeconds: 42}
		err = scheduler.UpdateDisconnectedServicesJobParams(ctx, &newParams)
		require.NoError(t, err)

		sched, err = scheduler.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
		require.NoError(t, err)

		sched.GetParameters(&returnedParams)
		assert.Equal(t, 42, returnedParams.ThresholdSeconds)
	})

	t.Run("running the job runner makes the jobs run", func(t *testing.T) {
		// TODO:
		// * update the schedule so it runs every second
		// * have a way to track number of job runs (prometheus (?))
	})

}

func studioCerts(t *testing.T) *certs.ServiceCerts {
	conf := certs.TLSConfig{
		CertPath:       path.Join(habConfigDir, "service.crt"),
		KeyPath:        path.Join(habConfigDir, "service.key"),
		RootCACertPath: path.Join(habConfigDir, "root_ca.crt"),
	}
	certs, err := conf.ReadCerts()
	require.NoError(t, err)
	return certs
}
