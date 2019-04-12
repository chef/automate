//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/common/query"
	"github.com/stretchr/testify/assert"
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
	mockHabService := NewHabServiceMsg("sup1234", a, e, "default", "core",
		"postgres", "0.1.0", "20190101121212", "OK")
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
					&applications.Service{
						SupervisorId: "sup1234",
						Group:        "postgres.default",
						Release:      "core/postgres/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_OK,
						Application:  a, Environment: e, Fqdn: "",
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
		ctx      = context.Background()
		request  = &applications.ServicesReq{}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_CRITICAL,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup2",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
			},
		}
	)

	response, err := suite.ApplicationsServer.GetServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func assertServicesEqual(t *testing.T, expected, actual []*applications.Service) {
	if assert.Equal(t, len(expected), len(actual), "The number of services are not the same") {
		for i, svc := range expected {
			assert.Equal(t,
				svc.SupervisorId,
				actual[i].SupervisorId,
				"The supervisor_id of a service is not the expected one")
			assert.Equal(t,
				svc.Release,
				actual[i].Release,
				"The release of a service is not the expected one")
			assert.Equal(t,
				svc.Group,
				actual[i].Group,
				"The group of a service is not the expected one")
			assert.Equal(t,
				svc.HealthCheck,
				actual[i].HealthCheck,
				"The health_check of a service is not the expected one")
			assert.Equal(t,
				svc.Status,
				actual[i].Status,
				"The status of a service is not the expected one")
			assert.Equal(t,
				svc.Application,
				actual[i].Application,
				"The application of a service is not the expected one")
			assert.Equal(t,
				svc.Environment,
				actual[i].Environment,
				"The environment of a service is not the expected one")
			assert.Equal(t,
				svc.Fqdn,
				actual[i].Fqdn,
				"The fqdn of a service is not the expected one")
		}
	}
}

func habServicesMatrixAllHealthStatusDifferent() []*applications.HabService {
	return []*applications.HabService{
		// service_group 1 <-> With a Health Status = 'OK'
		NewHabServiceMsg("sup1", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "OK"),

		// service_group 2 <-> With a Health Status = 'WARNING'
		NewHabServiceMsg("sup1", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "WARNING"),

		// service_group 3 <-> With a Health Status = 'CRITICAL'
		NewHabServiceMsg("sup1", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "CRITICAL"),

		// service_group 4 <-> With a Health Status = 'UNKNOWN'
		NewHabServiceMsg("sup2", a, e, "default", "core", "test", "0.1.0", "20190101121212", "UNKNOWN"),
	}
}
