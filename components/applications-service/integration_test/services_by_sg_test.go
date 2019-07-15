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

func TestGetServicesBySGErrorRequestMustHaveAServiceGroupID(t *testing.T) {
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

func TestGetServicesBySGErrorServiceGroupNotFound(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServicesBySGReq{
			ServiceGroupId: 999,
		}
		expected = new(applications.ServicesBySGRes)
	)
	response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
	if assert.NotNil(t, err,
		"if the service_group_id is not found, it should return an error") {
		assert.Contains(t, err.Error(), "NotFound")
		assert.Contains(t, err.Error(), "service-group not found")
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
	mockHabService := NewHabitatEvent(
		withSupervisorId("sup1234"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withFqdn("db.example.com"),
	)
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
					{
						SupervisorId: "sup1234",
						Group:        "postgres.default",
						Release:      "core/postgres/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_OK,
						Application:  a, Environment: e, Fqdn: "db.example.com",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
				ServicesHealthCounts: &applications.HealthCounts{Total: 1, Ok: 1},
			}
		)

		response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
		assert.Nil(t, err)
		assert.Equal(t, expected.GetGroup(), response.GetGroup())
		assert.Equal(t, expected.GetServicesHealthCounts(), response.GetServicesHealthCounts())
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesBySGWithPaginationParameter(t *testing.T) {
	suite.IngestServices(habServicesMatrix())
	defer suite.DeleteDataFromStorage()

	// Get the ID of the service-groups
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 4, len(sgList), "There should be four service_groups in the db") {

		var (
			ctx     = context.Background()
			request = &applications.ServicesBySGReq{
				ServiceGroupId: sgList[0].ID,
				Pagination: &query.Pagination{
					Page: 1,
					Size: 1,
				},
			}
			expected = &applications.ServicesBySGRes{
				Group: "myapp.default",
				Services: []*applications.Service{
					{
						SupervisorId: "sup1",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_WARNING,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
			}
		)

		response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
		assert.Nil(t, err)
		assert.Equal(t, expected.GetGroup(), response.GetGroup())
		assert.Equal(t, expected.GetServicesHealthCounts(), response.GetServicesHealthCounts())
		assertServicesEqual(t, expected.GetServices(), response.GetServices())
	}
}

func TestGetServicesBySGMultiService(t *testing.T) {
	suite.IngestServices(habServicesMatrix())
	defer suite.DeleteDataFromStorage()

	expectedResponses := []*applications.ServicesBySGRes{
		{
			Group:                "myapp.default",
			ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
			Services: []*applications.Service{
				{
					SupervisorId: "sup1",
					Group:        "myapp.default",
					Release:      "core/myapp/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_WARNING,
					Application:  a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					Group:       "myapp.default",
					Release:     "core/myapp/0.1.0/20190101121212",
					Status:      applications.ServiceStatus_RUNNING,
					HealthCheck: applications.HealthStatus_OK,
					Application: a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					Group:       "myapp.default",
					Release:     "core/myapp/0.1.0/20190101121212",
					Status:      applications.ServiceStatus_RUNNING,
					HealthCheck: applications.HealthStatus_OK,
					Application: a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		},
		{
			Group:                "postgres.default",
			ServicesHealthCounts: &applications.HealthCounts{Total: 3, Critical: 1, Ok: 1, Unknown: 1},
			Services: []*applications.Service{
				{
					SupervisorId: "sup3",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_CRITICAL,
					Application:  a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup2",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
				{
					SupervisorId: "sup1",
					Group:        "postgres.default",
					Release:      "core/postgres/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_OK,
					Application:  a, Environment: e, Fqdn: "",
					Channel: c, UpdateStrategy: none, Site: s,
				},
			},
		},
		{
			Group:                "redis.default",
			ServicesHealthCounts: &applications.HealthCounts{Total: 3, Ok: 3},
			Services: []*applications.Service{
				{
					Group:       "redis.default",
					Release:     "core/redis/0.1.0/20190101121212",
					Status:      applications.ServiceStatus_RUNNING,
					HealthCheck: applications.HealthStatus_OK,
					Application: a, Environment: e, Fqdn: "",
					Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
				},
				{
					Group:       "redis.default",
					Release:     "core/redis/0.1.0/20190101121212",
					Status:      applications.ServiceStatus_RUNNING,
					HealthCheck: applications.HealthStatus_OK,
					Application: a, Environment: e, Fqdn: "",
					Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
				},
				{
					Group:       "redis.default",
					Release:     "core/redis/0.1.0/20190101121212",
					Status:      applications.ServiceStatus_RUNNING,
					HealthCheck: applications.HealthStatus_OK,
					Application: a, Environment: e, Fqdn: "",
					Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
				},
			},
		},
		{
			Group:                "test.default",
			ServicesHealthCounts: &applications.HealthCounts{Total: 1, Unknown: 1},
			Services: []*applications.Service{
				{
					SupervisorId: "sup4",
					Group:        "test.default",
					Release:      "core/test/0.1.0/20190101121212",
					Status:       applications.ServiceStatus_RUNNING,
					HealthCheck:  applications.HealthStatus_UNKNOWN,
					Application:  a, Environment: e, Fqdn: "",
					Channel: "unstable", UpdateStrategy: "AT-ONCE", Site: "",
				},
			},
		},
	}

	// Get the service groups and iterate over to test every service within
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, len(expectedResponses), len(sgList),
		fmt.Sprintf("There should be %d service_group in the db", len(expectedResponses))) {

		for i, sg := range sgList {

			t.Run(fmt.Sprintf("verifying service group %s", sg.Name), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{ServiceGroupId: sg.ID}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedResponses[i].GetGroup(), response.GetGroup())
				assert.Equal(t, expectedResponses[i].GetServicesHealthCounts(), response.GetServicesHealthCounts())
				assertServicesEqual(t, expectedResponses[i].GetServices(), response.GetServices())
			})

		}
	}
}

func TestGetServicesBySGMultiServiceWithHealthFilter(t *testing.T) {
	suite.IngestServices(habServicesMatrix())
	defer suite.DeleteDataFromStorage()

	var (
		expectedOKResponses = []*applications.ServicesBySGRes{
			{
				Group:                "myapp.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
				Services: []*applications.Service{
					{
						SupervisorId: "sup2",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_OK,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
					{
						SupervisorId: "sup3",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_OK,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			},
			{
				Group:                "postgres.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Critical: 1, Ok: 1, Unknown: 1},
				Services: []*applications.Service{
					{
						SupervisorId: "sup1",
						Group:        "postgres.default",
						Release:      "core/postgres/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_OK,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			},
			{
				Group:                "redis.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Ok: 3},
				Services: []*applications.Service{
					{
						Group:       "redis.default",
						Release:     "core/redis/0.1.0/20190101121212",
						Status:      applications.ServiceStatus_RUNNING,
						HealthCheck: applications.HealthStatus_OK,
						Application: a, Environment: e, Fqdn: "",
						Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
					},
					{
						Group:       "redis.default",
						Release:     "core/redis/0.1.0/20190101121212",
						Status:      applications.ServiceStatus_RUNNING,
						HealthCheck: applications.HealthStatus_OK,
						Application: a, Environment: e, Fqdn: "",
						Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
					},
					{
						Group:       "redis.default",
						Release:     "core/redis/0.1.0/20190101121212",
						Status:      applications.ServiceStatus_RUNNING,
						HealthCheck: applications.HealthStatus_OK,
						Application: a, Environment: e, Fqdn: "",
						Channel: "stable", UpdateStrategy: "ROLLING", Site: s,
					},
				},
			},
			{
				Group:                "test.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 1, Unknown: 1},
			},
		}

		expectedCRITICALResponses = []*applications.ServicesBySGRes{
			{
				Group:                "myapp.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
			},
			{
				Group:                "postgres.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Critical: 1, Ok: 1, Unknown: 1},
				Services: []*applications.Service{
					{
						SupervisorId: "sup3",
						Group:        "postgres.default",
						Release:      "core/postgres/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_CRITICAL,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			},
			{
				Group:                "redis.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Ok: 3},
			},
			{
				Group:                "test.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 1, Unknown: 1},
			},
		}

		expectedWARNINGResponses = []*applications.ServicesBySGRes{
			{
				Group:                "myapp.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
				Services: []*applications.Service{
					{
						SupervisorId: "sup1",
						Group:        "myapp.default",
						Release:      "core/myapp/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_WARNING,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			},
			{
				Group:                "postgres.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Critical: 1, Ok: 1, Unknown: 1},
			},
			{
				Group:                "redis.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Ok: 3},
			},
			{
				Group:                "test.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 1, Unknown: 1},
			},
		}

		expectedUNKNOWNResponses = []*applications.ServicesBySGRes{
			{
				Group:                "myapp.default",
				Services:             []*applications.Service{},
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Warning: 1, Ok: 2},
			},
			{
				Group:                "postgres.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Critical: 1, Ok: 1, Unknown: 1},
				Services: []*applications.Service{
					{
						SupervisorId: "sup2",
						Group:        "postgres.default",
						Release:      "core/postgres/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_UNKNOWN,
						Application:  a, Environment: e, Fqdn: "",
						Channel: c, UpdateStrategy: none, Site: s,
					},
				},
			},
			{
				Group:                "redis.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 3, Ok: 3},
				Services:             []*applications.Service{},
			},
			{
				Group:                "test.default",
				ServicesHealthCounts: &applications.HealthCounts{Total: 1, Unknown: 1},
				Services: []*applications.Service{
					{
						SupervisorId: "sup4",
						Group:        "test.default",
						Release:      "core/test/0.1.0/20190101121212",
						Status:       applications.ServiceStatus_RUNNING,
						HealthCheck:  applications.HealthStatus_UNKNOWN,
						Application:  a, Environment: e, Fqdn: "",
						Channel: "unstable", UpdateStrategy: "AT-ONCE", Site: "",
					},
				},
			},
		}
	)

	// Get the service groups and iterate over to test every service within
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, len(expectedOKResponses), len(sgList),
		fmt.Sprintf("There should be %d service_group in the db", len(expectedOKResponses))) {

		for i, sg := range sgList {

			// OK Health
			t.Run(fmt.Sprintf("verifying service group %s with health:OK", sg.Name), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{
						ServiceGroupId: sg.ID,
						Health:         "OK", // This is the filter we are testing in this function
					}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedOKResponses[i].GetGroup(), response.GetGroup())
				assert.Equal(t, expectedOKResponses[i].GetServicesHealthCounts(), response.GetServicesHealthCounts())
				assertServicesEqual(t, expectedOKResponses[i].GetServices(), response.GetServices())
			})

			// CRITICAL Health
			t.Run(fmt.Sprintf("verifying service group %s with health:CRITICAL", sg.Name), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{
						ServiceGroupId: sg.ID,
						Health:         "CRITICAL", // This is the filter we are testing in this function
					}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedCRITICALResponses[i].GetGroup(), response.GetGroup())
				assert.Equal(t, expectedCRITICALResponses[i].GetServicesHealthCounts(), response.GetServicesHealthCounts())
				assertServicesEqual(t, expectedCRITICALResponses[i].GetServices(), response.GetServices())
			})

			// WARNING Health
			t.Run(fmt.Sprintf("verifying service group %s with health:WARNING", sg.Name), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{
						ServiceGroupId: sg.ID,
						Health:         "WARNING", // This is the filter we are testing in this function
					}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedWARNINGResponses[i].GetGroup(), response.GetGroup())
				assert.Equal(t, expectedWARNINGResponses[i].GetServicesHealthCounts(), response.GetServicesHealthCounts())
				assertServicesEqual(t, expectedWARNINGResponses[i].GetServices(), response.GetServices())
			})

			// UNKNOWN Health
			t.Run(fmt.Sprintf("verifying service group %s with health:UNKNOWN", sg.Name), func(t *testing.T) {
				var (
					ctx     = context.Background()
					request = &applications.ServicesBySGReq{
						ServiceGroupId: sg.ID,
						Health:         "UNKNOWN", // This is the filter we are testing in this function
					}
				)

				response, err := suite.ApplicationsServer.GetServicesBySG(ctx, request)
				assert.Nil(t, err)
				assert.Equal(t, expectedUNKNOWNResponses[i].GetGroup(), response.GetGroup())
				assert.Equal(t, expectedUNKNOWNResponses[i].GetServicesHealthCounts(), response.GetServicesHealthCounts())
				assertServicesEqual(t, expectedUNKNOWNResponses[i].GetServices(), response.GetServices())
			})
		}
	}
}
