//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/common/query"
	"github.com/stretchr/testify/assert"
)

func TestGetServicesBySGRequestMustHaveAServiceGroupID(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServicesBySGReq)
		expected = new(applications.ServicesBySGRes)
	)
	response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
	if assert.NotNil(t, err,
		"if the service_group_id is not specified, it should return an error") {
		assert.Contains(t, err.Error(), "InvalidArgument")
		assert.Contains(t, err.Error(), "Missing service_group_id parameter.")
	}

	assert.Equal(t, expected, response)
}

func TestGetServicesBySGSortParameterError(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServicesBySGReq{
			Sorting: &query.Sorting{
				Field: "not-valid-sort-field",
			},
		}
		expected = new(applications.ServicesBySGRes)
	)
	response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
	assert.NotNil(t, err)
	assert.Contains(t, err.Error(), "InvalidArgument")
	assert.Contains(t, err.Error(), "Invalid sort field 'not-valid-sort-field'.")
	assert.Equal(t, expected, response)
}

func TestGetServicesBySGSingleService(t *testing.T) {
	mockHabService := NewHabServiceMsg("sup1234", a, e, "default", "core",
		"postgres", "0.1.0", "20190101121212", "OK")
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {

		var (
			ctx     = context.Background()
			request = &applications.ServicesBySGReq{
				ServiceGroupId: sgList[0].ID,
			}
			expected = &applications.ServicesBySGRes{
				Group: "postgres.default",
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

		response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
		assert.Nil(t, err)
		assert.Equal(t, expected.GetGroup(), response.GetGroup())
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesBySGMultiService(t *testing.T) {
	suite.IngestServices(habServicesMatrix())
	defer suite.DeleteDataFromStorage()

	expectedResponses := []*applications.ServicesBySGRes{
		&applications.ServicesBySGRes{
			Group: "myapp.default",
			Services: []*applications.Service{
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup2",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup3",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
			},
		},
		&applications.ServicesBySGRes{
			Group: "postgres.default",
			Services: []*applications.Service{
				&applications.Service{
					SupervisorId: "sup3",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_CRITICAL,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup2",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
			},
		},
		&applications.ServicesBySGRes{
			Group: "redis.default",
			Services: []*applications.Service{
				&applications.Service{
					SupervisorId: "sup1",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup2",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
				&applications.Service{
					SupervisorId: "sup3",
					Group:        "redis.default",
					Release:      "core/redis/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
				},
			},
		},
		&applications.ServicesBySGRes{
			Group: "test.default",
			Services: []*applications.Service{
				&applications.Service{
					SupervisorId: "sup4",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "",
				},
			},
		},
	}

	// Get the service groups and iterate over to test every service within
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, len(expectedResponses), len(sgList),
		fmt.Sprintf("There should be %d service_group in the db", len(expectedResponses))) {

		for i, sg := range sgList {

			t.Run(fmt.Sprintf("verifying service group %d", sg.ID), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{ServiceGroupId: sg.ID}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedResponses[i].GetGroup(), response.GetGroup())
				assertServicesEqual(t, expectedResponses[i].GetServices(), response.GetServices())
			})

		}
	}
}
