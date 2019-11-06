//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestServiceGroupsMultiServiceFilterStatusOk(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"STATUS:OK"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "redis.default",
					Package:          "core/redis",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
					Application:      "app",
					Environment:      "test-env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total: 3,
						Ok:    3,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsMultiServiceFilterStatusCritical(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"status:CRITICAL"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "postgres.default",
					Package:          "core/postgres",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 33,
					Application:      "app",
					Environment:      "test-env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    3,
						Ok:       1,
						Critical: 1,
						Unknown:  1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsMultiServiceFilterStatusWarning(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"status:warning"}, // NOTE: We are not case sensitive, we accept any status that matches the word
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "myapp.default",
					Package:          "core/myapp",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_WARNING,
					HealthPercentage: 67,
					Application:      "app",
					Environment:      "test-env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   3,
						Ok:      2,
						Warning: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsMultiServiceFilterStatusUnknown(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"STATUS:unknown"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "test.default",
					Package:          "core/test",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "app",
					Environment:      "test-env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestDisconnectedStatusFilter(t *testing.T) {
	ctx := context.Background()
	oldEventTime, err := ptypes.TimestampProto(time.Now().Add(-time.Hour * 24))

	t.Run("when no services are disconnected the response is an empty set", func(t *testing.T) {
		suite.IngestServices(habServicesMatrix())
		defer suite.DeleteDataFromStorage()
		req := &applications.ServiceGroupsReq{
			Filter: []string{"status:disconnected"},
		}
		response, err := suite.ApplicationsServer.GetServiceGroups(ctx, req)
		assert.Nil(t, err)
		assert.Len(t, response.ServiceGroups, 0)
	})
	t.Run("when all services are disconnected, all service groups are returned", func(t *testing.T) {
		servicesToIngest := habServicesMatrix()
		require.NoError(t, err)

		for _, e := range servicesToIngest {
			e.EventMetadata.OccurredAt = oldEventTime
		}
		suite.IngestServices(servicesToIngest)
		defer suite.DeleteDataFromStorage()
		_, err = suite.ApplicationsServer.MarkDisconnectedServices(300)
		require.NoError(t, err)

		req := &applications.ServiceGroupsReq{
			Filter: []string{"status:disconnected"},
		}
		response, err := suite.ApplicationsServer.GetServiceGroups(ctx, req)
		assert.Nil(t, err)
		assert.Len(t, response.ServiceGroups, 4)
	})
	t.Run("when all service groups have some disconnected services all service groups are returned", func(t *testing.T) {
		servicesToIngest := habServicesMatrix()
		require.NoError(t, err)

		for _, i := range []int{0, 4, 7, 9} {
			s := servicesToIngest[i]
			s.EventMetadata.OccurredAt = oldEventTime
		}
		suite.IngestServices(servicesToIngest)
		defer suite.DeleteDataFromStorage()
		_, err = suite.ApplicationsServer.MarkDisconnectedServices(300)
		require.NoError(t, err)

		req := &applications.ServiceGroupsReq{
			Filter: []string{"status:disconnected"},
		}
		response, err := suite.ApplicationsServer.GetServiceGroups(ctx, req)
		assert.Nil(t, err)
		assert.Len(t, response.ServiceGroups, 4)
	})
	t.Run("when some service groups have disconnected services only those service groups are returned", func(t *testing.T) {
		servicesToIngest := habServicesMatrix()
		require.NoError(t, err)

		for _, i := range []int{0, 4} {
			s := servicesToIngest[i]
			s.EventMetadata.OccurredAt = oldEventTime
		}
		suite.IngestServices(servicesToIngest)
		defer suite.DeleteDataFromStorage()
		_, err = suite.ApplicationsServer.MarkDisconnectedServices(300)
		require.NoError(t, err)

		req := &applications.ServiceGroupsReq{
			Filter: []string{"status:disconnected"},
		}
		response, err := suite.ApplicationsServer.GetServiceGroups(ctx, req)
		assert.Nil(t, err)
		assert.Len(t, response.ServiceGroups, 2)
	})
	// TODO: Re-enable this test when we support multiple selected filters
	//
	// t.Run("when a status filter and disconnected status filter are both applied and a service group meets both criteria", func(t *testing.T) {
	// 	servicesToIngest := habServicesMatrix()
	// 	require.NoError(t, err)

	// 	for _, i := range []int{0, 4, 7, 9} {
	// 		s := servicesToIngest[i]
	// 		s.EventMetadata.OccurredAt = oldEventTime
	// 	}
	// 	suite.IngestServices(servicesToIngest)
	// 	defer suite.DeleteDataFromStorage()
	// 	_, err = suite.ApplicationsServer.MarkDisconnectedServices(300)
	// 	require.NoError(t, err)

	// 	req := &applications.ServiceGroupsReq{
	// 		Filter: []string{"status:disconnected", "status:critical"},
	// 	}
	// 	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, req)
	// 	assert.Nil(t, err)
	// 	assert.Len(t, response.ServiceGroups, 1)
	// })

}

func TestServiceGroupsFilterStatusWrongParameter(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"status:not-valid-status"},
		}
		expected    *applications.ServiceGroups = nil
		expectedErr                             = "invalid status filter 'not-valid-status'"
	)

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assertServiceGroupsEqual(t, expected, response)

	assert.NotNil(t, err)
	if assert.Error(t, err) {
		assert.Contains(t, err.Error(), expectedErr)
	}
}

func TestServiceGroupsFilterWrongType(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"foo:bar"},
		}
		expected    *applications.ServiceGroups = nil
		expectedErr                             = "invalid filter. (foo:[bar])"
	)

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assertServiceGroupsEqual(t, expected, response)

	assert.NotNil(t, err)
	if assert.Error(t, err) {
		assert.Contains(t, err.Error(), expectedErr)
	}
}

func TestServiceGroupsFilterEnvironment(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a_env"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterApp(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"application:a_app"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsMultiServiceFilterAppEnv(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"application:a_app", "environment:a_env"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterAppEnvStatus(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"application:a_app", "environment:a_env", "status:UNKNOWN"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterServiceGroupName(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"group:default"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.test"),
				withPackageIdent("core/b/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

// This tests out the or-ing together of the same field.
func TestServiceGroupsFilterServiceGroupNameDouble(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"group:default", "group:test"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
				{
					Name:             "a.test",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.test"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	//Should return both service groups
	if assert.Equal(t, 2, len(response.GetServiceGroups())) {
		svcList := response.GetServiceGroups()
		svcIdList := make([]string, 2)
		for i, svc := range svcList {
			svcIdList[i] = svc.Name
		}
		assert.Contains(t, svcIdList, expected.GetServiceGroups()[0].Name,
			"the service group name is not the expected one")
		assert.Contains(t, svcIdList, expected.GetServiceGroups()[1].Name,
			"the service group name is not the expected one")
	}
}

func TestServiceGroupsFilterServiceOrigin(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"origin:core"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.test"),
				withPackageIdent("chef/b/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

// This test tests what happens when a service group contains two different origins
// and is filtered by origin.
func TestServiceGroupsFilterSGPartOrigin(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"origin:core"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.default"),
				withPackageIdent("chef/a/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterService(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"service:a"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterSite(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"site:downtown"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
				withSite("downtown"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("b.test"),
				withPackageIdent("core/b/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("b_app"),
				withEnvironment("b_env"),
				withSite("uptown"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterChannel(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"channel:stable"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
				withStrategyAtOnce("stable"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("b.test"),
				withPackageIdent("core/b/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("b_app"),
				withEnvironment("b_env"),
				withStrategyAtOnce("unstable"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterVersion(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"version:0.2.2"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.2.2/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.2.2/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("b.test"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("b_app"),
				withEnvironment("b_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterPartSGVersion(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"version:0.2.2"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.2.2/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in the same service group, with different package versions
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.2.2/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterBuildstamp(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"buildstamp:20190101121212"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in different service groups
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("b.test"),
				withPackageIdent("core/a/0.1.0/00000000000000"),
				withHealth("OK"),
				withApplication("b_app"),
				withEnvironment("b_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterPartSGVersionOrigin(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"version:0.2.2", "origin:core"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.2.2/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		//2 services in the same service group, with different package versions
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.2.2/20190101121212"),
				withHealth("UNKNOWN"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("a.default"),
				withPackageIdent("core/a/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("a.default"),
				withPackageIdent("chef/a/0.2.2/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("a.default"),
				withPackageIdent("chef/a/0.1.0/20190101121212"),
				withHealth("OK"),
				withApplication("a_app"),
				withEnvironment("a_env"),
			),
		}
	)

	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsFilterEnvironmentWildcard(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a*"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

// Single letter wildcard
func TestServiceGroupsFilterEnvironmentQuestion(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a_?nv"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

// Wild card in one field and not wild card in another
func TestServiceGroupsFilterEnAppWildAndNot(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a*", "application:a_app"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

// Same field has one wildcard and one not wild card
func TestServiceGroupsFilterEnvWildAndNot(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a*", "environment:a_env"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

//Two wild cards in different fields
func TestServiceGroupsFilterDoubleWild(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Filter: []string{"environment:a*", "application:a*"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "a.default",
					Package:          "core/a",
					Release:          "0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      "a_app",
					Environment:      "a_env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
			},
		}
		mockHabServicesMatrix = habServicesABCD()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}
