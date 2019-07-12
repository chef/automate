//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/common/query"
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
			sgID    = fmt.Sprintf("%d", sgList[0].ID)
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
			sgID    = fmt.Sprintf("%d", sgList[3].ID)
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
