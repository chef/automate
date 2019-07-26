//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/stretchr/testify/assert"
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
