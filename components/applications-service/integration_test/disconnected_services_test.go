//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
)

func TestDisconnectedServicesMustProvideThresholdError(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.DisconnectedServicesReq)
		expected = new(applications.ServicesRes)
	)
	t.Run("GetDisconnectedServices with no params", func(t *testing.T) {
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "InvalidArgument")
		assert.Contains(t, err.Error(), "threshold must be greater than zero")
		assert.Equal(t, expected, response)
	})
	t.Run("DeleteDisconnectedServices with no params", func(t *testing.T) {
		response, err := suite.ApplicationsServer.DeleteDisconnectedServices(ctx, request)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "InvalidArgument")
		assert.Contains(t, err.Error(), "threshold must be greater than zero")
		assert.Equal(t, expected, response)
	})
}

func TestDisconnectedServicesBasicNoData(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 5}
		expected = &applications.ServicesRes{Services: []*applications.Service{}}
	)
	t.Run("GetDisconnectedServices with no data", func(t *testing.T) {
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assert.Equal(t, expected, response)
	})
	t.Run("DeleteDisconnectedServices with no data", func(t *testing.T) {
		response, err := suite.ApplicationsServer.DeleteDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assert.Equal(t, expected, response)
	})
}

func TestDisconnectedServicesBasicSingleServiceMockedAsDisconnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		err      error
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId:        "abcd",
					Group:               "postgres.default",
					Release:             "core/postgres/0.1.0/20190101121212",
					Status:              applications.ServiceStatus_RUNNING,
					HealthCheck:         applications.HealthStatus_OK,
					Fqdn:                "mytest.example.com",
					Channel:             "testchannel",
					Site:                "testsite",
					PreviousHealthCheck: applications.HealthStatus_NONE,
					Application:         a, Environment: e,
				},
			},
		}
	)

	event := NewHabitatEvent(
		withSupervisorId("abcd"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withStrategyAtOnce("testchannel"),
		withFqdn("mytest.example.com"),
		withSite("testsite"),
	)

	// Patch event timestamp to mock an old service message and mack it as disconnected
	event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 5))
	require.NoError(t, err)
	suite.IngestService(event)

	t.Run("GetDisconnectedServices with one disconnected service", func(t *testing.T) {
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	})
	t.Run("DeleteDisconnectedServices with one disconnected service", func(t *testing.T) {
		response, err := suite.ApplicationsServer.DeleteDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
		// verify we deleted it:
		response, err = suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, []*applications.Service{}, response.GetServices())

		stats, err := suite.ApplicationsServer.GetServicesStats(ctx, &applications.ServicesStatsReq{})
		require.NoError(t, err)

		expectedStats := &applications.ServicesStatsRes{
			TotalServices:      0,
			TotalServiceGroups: 0,
			TotalSupervisors:   0,
			TotalDeployments:   0,
		}
		assert.Equal(t, expectedStats, stats)
	})
}

func TestDisconnectedServicesMultiServicesMixedConnectedAndDisconnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		err      error
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId:        "efgh",
					Group:               "postgres.default",
					Release:             "core/postgres/0.1.0/20190101121212",
					Status:              applications.ServiceStatus_RUNNING,
					HealthCheck:         applications.HealthStatus_OK,
					Fqdn:                "mytest.example.com",
					Channel:             "testchannel",
					Site:                "testsite",
					PreviousHealthCheck: applications.HealthStatus_NONE,
					Application:         a, Environment: e,
				},
			},
		}
	)

	// Ingest service number one
	event := NewHabitatEvent(
		withSupervisorId("abcd"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withStrategyAtOnce("testchannel"),
		withFqdn("mytest.example.com"),
		withSite("testsite"),
	)
	// Patch event timestamp to mock an old service message, but one that is within the disconnected threshold
	event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 2))
	suite.IngestService(event)

	// Ingest service number two, different service because the supervisor id changes
	UpdateHabitatEvent(event,
		withSupervisorId("efgh"),
	)
	// Patch event timestamp to mock an old service message and mock it as disconnected
	event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 5))
	assert.Nil(t, err)
	suite.IngestService(event)

	t.Run("GetDisconnectedServices with mixed connected and not", func(t *testing.T) {
		// We should only have a single disconnected service back
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	})
	t.Run("DeleteDisconnectedServices with mixed connected and not", func(t *testing.T) {
		// We should only have a single disconnected service deleted
		response, err := suite.ApplicationsServer.DeleteDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())

		// After it's deleted, we shouldn't see the deleted one in GetDisconnectedServices
		response, err = suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assertServicesEqual(t, []*applications.Service{}, response.GetServices())

		// We should see the other service if we use a threshold of 0
		req2 := &applications.DisconnectedServicesReq{ThresholdMinutes: 1}
		response, err = suite.ApplicationsServer.GetDisconnectedServices(ctx, req2)
		require.NoError(t, err)
		notDisconnectedSvcs := expected.GetServices()
		notDisconnectedSvcs[0].SupervisorId = "abcd"
		assertServicesEqual(t, notDisconnectedSvcs, response.GetServices())

		stats, err := suite.ApplicationsServer.GetServicesStats(ctx, &applications.ServicesStatsReq{})
		require.NoError(t, err)

		expectedStats := &applications.ServicesStatsRes{
			TotalServices:      1,
			TotalServiceGroups: 1,
			TotalSupervisors:   1,
			TotalDeployments:   1,
		}
		assert.Equal(t, expectedStats, stats)
	})
}

func TestDisconnectedServicesMultiServicesAllConnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{Services: []*applications.Service{}}
	)

	suite.IngestServices(habServicesMatrix())

	t.Run("GetDisconnectedServices with several connected services", func(t *testing.T) {
		// We should have no disconnected services
		response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assert.Equal(t, expected, response)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	})
	t.Run("DeleteDisconnectedServices with several connected services", func(t *testing.T) {
		// Nothing should be deleted:
		response, err := suite.ApplicationsServer.DeleteDisconnectedServices(ctx, request)
		require.NoError(t, err)
		assert.Equal(t, expected, response)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
		// the services should be there:
		sgList := suite.GetServiceGroups()
		assert.Len(t, sgList, 4)

		stats, err := suite.ApplicationsServer.GetServicesStats(ctx, &applications.ServicesStatsReq{})
		require.NoError(t, err)

		expectedStats := &applications.ServicesStatsRes{
			TotalServices:      10,
			TotalServiceGroups: 4,
			TotalSupervisors:   4,
			TotalDeployments:   1,
		}
		assert.Equal(t, expectedStats, stats)
	})
}
