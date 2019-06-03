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
	"github.com/chef/automate/api/external/habitat"
	"github.com/stretchr/testify/assert"
)

func TestServiceGroupsBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = new(applications.ServiceGroups)
	)
	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}

func TestGetServiceGroupsOneOk(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    1,
						Ok:       1,
						Warning:  0,
						Critical: 0,
						Unknown:  0,
					},
				},
			},
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
		)
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestGetServiceGroupsOneCritical(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 0,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    1,
						Ok:       0,
						Warning:  0,
						Critical: 1,
						Unknown:  0,
					},
				},
			},
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("CRITICAL"),
		)
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsMultiService(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:             "test.default",
					Release:          "core/test/0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   1,
						Unknown: 1,
					},
				},
				{
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 33,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    3,
						Ok:       1,
						Critical: 1,
						Unknown:  1,
					},
				},
				{
					Name:             "myapp.default",
					Release:          "core/myapp/0.1.0/20190101121212",
					Status:           applications.HealthStatus_WARNING,
					HealthPercentage: 67,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   3,
						Ok:      2,
						Warning: 1,
					},
				},
				{
					Name:             "redis.default",
					Release:          "core/redis/0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
					Application:      a,
					Environment:      e,
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

func TestGetServiceGroupsOneWarning(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_WARNING,
					HealthPercentage: 0,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    1,
						Ok:       0,
						Warning:  1,
						Critical: 0,
						Unknown:  0,
					},
				},
			},
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("WARNING"),
		)
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestGetServiceGroupsOneUnknown(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    1,
						Ok:       0,
						Warning:  0,
						Critical: 0,
						Unknown:  1,
					},
				},
			},
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
		)
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}
func TestGetServiceGroupsOneEach(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 25,
					Application:      a,
					Environment:      e,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    4,
						Ok:       1,
						Warning:  1,
						Critical: 1,
						Unknown:  1,
					},
				},
			},
		}
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assertServiceGroupsEqual(t, expected, response)
}

func TestGetServiceGroupsInvalidPageNumberReturnsDefaultPageValues(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Pagination: &query.Pagination{
				Page: -2,
				Size: 1,
			},
		}
		mockHabServices = habServicesABCD()
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	// d.default should be returned since it is a Critical service-group and by default
	// we return page number one and we sort by percent_ok
	if assert.Equal(t, 1, len(response.ServiceGroups)) {
		assert.Equal(t, "d.default", response.ServiceGroups[0].Name)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[0].Status)
	}
}

func TestGetServiceGroupsPage(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Pagination: &query.Pagination{
				Page: 2,
				Size: 1,
			},
		}
		mockHabServices = habServicesABCD()
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	// a.default should be on the second page with default sorting (Unknown Health)
	if assert.Equal(t, 1, len(response.ServiceGroups)) {
		assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
		assert.Equal(t, applications.HealthStatus_UNKNOWN, response.ServiceGroups[0].Status)
	}
}

// This test is verifying that when users specify a filter the paginator works as expected
func TestGetServiceGroupsMultiplePagesAndFilters(t *testing.T) {
	var (
		ctx = context.Background()
		// For this test we are adding:
		//  * 3 OK       service-groups
		//  * 1 UNKNOWN  service-groups
		//  * 1 WARNING  service-groups
		//  * 1 CRITICAL service-groups
		mockHabServices = append(habServicesABCD(),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("e.default"),
				withPackageIdent("core/e/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("f.default"),
				withPackageIdent("core/f/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		)
		// This request is asking only for service groups that have an OK status
		// plus, showing only the page two with a page size of one and they are all
		// ordered by name, therefor this test should return only the following service:
		//
		// => name:"c.default" release:"core/c/0.1.0/20190101121212" status:OK
		request = &applications.ServiceGroupsReq{
			Filter: []string{"STATUS:OK"},
			Pagination: &query.Pagination{
				Page: 2,
				Size: 1,
			},
			Sorting: &query.Sorting{Field: "name"},
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:                 "e.default",
					Release:              "core/e/0.1.0/20190101121212",
					Status:               applications.HealthStatus_OK,
					HealthPercentage:     100,
					Application:          a,
					Environment:          e,
					ServicesHealthCounts: &applications.HealthCounts{Total: 1, Ok: 1},
				},
			},
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}
