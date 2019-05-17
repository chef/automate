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
					Application:      "app",
					Environment:      "test-env",
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
		mockHabService = NewHabServiceMsg("sup2", a, e, "default", "core",
			"postgres", "0.1.0", "20190101121212", "OK", "", "")
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
					Application:      "app",
					Environment:      "test-env",
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
		mockHabService = NewHabServiceMsg("sup2", a, e, "default", "core",
			"postgres", "0.1.0", "20190101121212", "CRITICAL", "", "")
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
					Name:             "myapp.default",
					Release:          "core/myapp/0.1.0/20190101121212",
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
				{
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
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
				{
					Name:             "redis.default",
					Release:          "core/redis/0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
					Application:      "app",
					Environment:      "test-env",
					ServicesHealthCounts: &applications.HealthCounts{
						Total: 3,
						Ok:    3,
					},
				},
				{
					Name:             "test.default",
					Release:          "core/test/0.1.0/20190101121212",
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
					Application:      "app",
					Environment:      "test-env",
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
		mockHabService = NewHabServiceMsg("sup2", a, e, "default", "core",
			"postgres", "0.1.0", "20190101121212", "WARNING", "", "")
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
					Application:      "app",
					Environment:      "test-env",
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
		mockHabService = NewHabServiceMsg("sup2", a, e, "default", "core",
			"postgres", "0.1.0", "20190101121212", "UNKNOWN", "", "")
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
					Application:      "app",
					Environment:      "test-env",
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
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assertServiceGroupsEqual(t, expected, response)
}

func TestGetServiceGroupsSortedDesc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "name",
				Order: query.SortOrder_DESC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, "d.default", response.ServiceGroups[0].Name)
	assert.Equal(t, "c.default", response.ServiceGroups[1].Name)
	assert.Equal(t, "b.default", response.ServiceGroups[2].Name)
	assert.Equal(t, "a.default", response.ServiceGroups[3].Name)
}

func TestGetServiceGroupsSortedAsc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "name",
				Order: query.SortOrder_ASC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
	assert.Equal(t, "b.default", response.ServiceGroups[1].Name)
	assert.Equal(t, "c.default", response.ServiceGroups[2].Name)
	assert.Equal(t, "d.default", response.ServiceGroups[3].Name)
}
func TestGetServiceGroupsSortedApplicationAsc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "app_name",
				Order: query.SortOrder_ASC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", "a", e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", "b", e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", "c", e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", "d", e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
	assert.Equal(t, "b.default", response.ServiceGroups[1].Name)
	assert.Equal(t, "c.default", response.ServiceGroups[2].Name)
	assert.Equal(t, "d.default", response.ServiceGroups[3].Name)
}

func TestGetServiceGroupsSortedEnvironmentAsc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "environment",
				Order: query.SortOrder_ASC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, "a", "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, "b", "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, "c", "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, "d", "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
	assert.Equal(t, "b.default", response.ServiceGroups[1].Name)
	assert.Equal(t, "c.default", response.ServiceGroups[2].Name)
	assert.Equal(t, "d.default", response.ServiceGroups[3].Name)
}

func TestGetServiceGroupsSortedPercent(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "percent_ok",
				Order: query.SortOrder_DESC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "OK", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, int32(100), response.ServiceGroups[0].HealthPercentage)
	assert.Equal(t, int32(50), response.ServiceGroups[1].HealthPercentage)
	assert.Equal(t, int32(0), response.ServiceGroups[2].HealthPercentage)
}

func TestGetServiceGroupsSortedPercentAsc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "percent_ok",
				Order: query.SortOrder_ASC,
			},
		}
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup6", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup7", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup8", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup9", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assert.Equal(t, int32(0), response.ServiceGroups[0].HealthPercentage)
	assert.Equal(t, int32(25), response.ServiceGroups[1].HealthPercentage)
	assert.Equal(t, int32(50), response.ServiceGroups[2].HealthPercentage)
	assert.Equal(t, int32(100), response.ServiceGroups[3].HealthPercentage)
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
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	// a.default should be returned since we default to page number one
	assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
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
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	// b.default should be on the second page with default sorting
	assert.Equal(t, "b.default", response.ServiceGroups[0].Name)
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
		mockHabServices = []*applications.HabService{
			NewHabServiceMsg("sup1", a, e, "default", "core",
				"a", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup2", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "UNKNOWN", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"e", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"f", "0.1.0", "20190101121212", "CRITICAL", "", ""),
		}
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
		}
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				{
					Name:                 "c.default",
					Release:              "core/c/0.1.0/20190101121212",
					Status:               applications.HealthStatus_OK,
					HealthPercentage:     100,
					Application:          "app",
					Environment:          "test-env",
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
