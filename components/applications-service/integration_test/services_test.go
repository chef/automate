//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/habitat"
)

func TestGetServicesBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServicesReq)
		expected = new(applications.ServicesRes)
	)
	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}

func TestGetServicesSortParameterError(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{
			Sorting: &query.Sorting{
				Field: "not-valid-sort-field",
			},
		}
		expected = new(applications.ServicesRes)
	)
	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.NotNil(t, err)
	assert.Contains(t, err.Error(), "InvalidArgument")
	assert.Contains(t, err.Error(), "Invalid sort field 'not-valid-sort-field'.")
	assert.Equal(t, expected, response)
}

func TestGetServicesSingleService(t *testing.T) {
	mockHabService := NewHabitatEvent(
		withSupervisorId("sup1234"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withFqdn("mytest.example.com"),
		withStrategyAtOnce("testchannel"),
		withSite("testsite"),
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx      = context.Background()
			request  = &applications.ServicesReq{}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup1234",
						Group:          "postgres.default",
						Release:        "core/postgres/0.1.0/20190101121212",
						Status:         applications.ServiceStatus_RUNNING,
						HealthCheck:    applications.HealthStatus_OK,
						Application:    a,
						Environment:    e,
						Fqdn:           "mytest.example.com",
						Channel:        "testchannel",
						UpdateStrategy: "AT-ONCE",
						Site:           "testsite",
					},
				},
			}
		)

		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiService(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{}
		// The expected response should have the services ordered with the default
		// health status order.  ["CRITICAL", "UNKNOWN", "WARNING", "OK"]
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId: "sup1",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_CRITICAL,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup2",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "test-1.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup3",
					Group:        "temp.default",
					Release:      "core/temp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "temp.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup1",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup4",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "test-2.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetServicesMultiServicaSortDESC(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{
			Sorting: &query.Sorting{
				Field: "health",
				Order: query.SortOrder_DESC,
			},
		}
		// The expected response should have the services ordered by
		// health status in descending order ["OK", "WARN", "UNKNOWN", "CRITICAL"]
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId: "sup1",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup4",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "test-2.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup2",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "test-1.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup3",
					Group:        "temp.default",
					Release:      "core/temp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "temp.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup1",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_CRITICAL,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetServicesMultiServicaPagination(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{
			Pagination: &query.Pagination{
				Page: 2,
				Size: 1,
			},
		}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId: "sup2",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "test-1.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

// TODO @afiune How do we handle OutOfRange pages???
// Shouldn't we return an error here?
func TestGetServicesMultiServicaPaginationOutOfRange(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{
			Pagination: &query.Pagination{
				Page: 9,
				Size: 1,
			},
		}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetServicesMultiServicePaginationAndSorting(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	var (
		ctx     = context.Background()
		request = &applications.ServicesReq{
			Pagination: &query.Pagination{
				Page: 3,
				Size: 1,
			},
			Sorting: &query.Sorting{
				Field: "health",
				Order: query.SortOrder_DESC,
			},
		}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetServicesMultiServiceWithServiceGroupIDFilter(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 5, len(sgList), "There should be five service_groups in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{"service_group_id:" + sgID},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId: "sup1",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_WARNING,
						Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithHealthFilter(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 5, len(sgList), "There should be five service_groups in the db") {

		var (
			ctx     = context.Background()
			request = &applications.ServicesReq{
				Filter: []string{"health:WARNING"},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId: "sup1",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_WARNING,
						Application:  a, Environment: e, Fqdn: "myapp-us.example.com",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithHealthAndServiceGroupIdFilter(t *testing.T) {
	suite.IngestServices(habServicesMatrixAllHealthStatusDifferent())
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 5, len(sgList), "There should be five service_groups in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[3].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"health:UNKNOWN",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId: "sup3",
						Group:        "temp.default",
						Release:      "core/temp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_UNKNOWN,
						Application:  a, Environment: e, Fqdn: "temp.example.com",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithOriginAndServiceGroupIdFilter(t *testing.T) {
	// Same service group, different origins
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("core/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/00000000000000"),
			withHealth("OK"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"origin:chef",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup3",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/00000000000000",
						HealthCheck:    applications.HealthStatus_OK,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithSiteAndServiceGroupIdFilter(t *testing.T) {
	// Same service group, different sites
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
			withSite("chernobyl"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/00000000000000"),
			withHealth("OK"),
			withApplication("a_app"),
			withEnvironment("a_env"),
			withSite("fukushima"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"site:fukushima",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup3",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/00000000000000",
						HealthCheck:    applications.HealthStatus_OK,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           "fukushima",
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithChannelAndServiceGroupIdFilter(t *testing.T) {
	// Same service group, different channels
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
			withStrategyAtOnce("Q13"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/00000000000000"),
			withHealth("OK"),
			withApplication("a_app"),
			withEnvironment("a_env"),
			withStrategyAtOnce("fox"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"channel:fox",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup3",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/00000000000000",
						HealthCheck:    applications.HealthStatus_OK,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: "AT-ONCE",
						Site:           s,
						Channel:        "fox",
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithVersionAndServiceGroupIdFilter(t *testing.T) {
	// Same service group, different versions
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.2.2/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/00000000000000"),
			withHealth("OK"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"version:0.2.2",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.2.2/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithBuildstampAndServiceGroupIdFilter(t *testing.T) {
	// Same service group, different buildstamps
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/00000000000000"),
			withHealth("OK"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"buildstamp:20190101121212",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithEnvironmentAndServiceGroupIdFilter(t *testing.T) {
	// Same service group
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"environment:a_env",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithApplicationAndServiceGroupIdFilter(t *testing.T) {
	// Same service group
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"application:a_app",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithGroupNameAndServiceGroupIdFilter(t *testing.T) {
	// Same service group
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"group:test",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesMultiServiceWithServiceNameAndServiceGroupIdFilter(t *testing.T) {
	// Same service group
	mockHabServices := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.test"),
			withPackageIdent("chef/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
	}
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			sgID    = sgList[0].ID
			request = &applications.ServicesReq{
				Filter: []string{
					"service_group_id:" + sgID,
					"service:a",
				},
			}
			expected = &applications.ServicesRes{
				Services: []*applications.Service{
					{
						SupervisorId:   "sup2",
						Group:          "a.test",
						Release:        "chef/a/0.1.0/20190101121212",
						HealthCheck:    applications.HealthStatus_UNKNOWN,
						Application:    "a_app",
						Environment:    "a_env",
						UpdateStrategy: none,
						Site:           s,
					},
				},
			}
		)
		response, err := suite.ApplicationsServer.GetServices(ctx, request)
		assert.Nil(t, err)
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}
