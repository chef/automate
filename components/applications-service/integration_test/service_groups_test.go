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

func TestServiceGroupsHealthOneOk(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				&applications.ServiceGroup{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
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
			"postgres", "0.1.0", "20190101121212", "OK")
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsHealthOneCritical(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				&applications.ServiceGroup{
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 0,
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
			"postgres", "0.1.0", "20190101121212", "CRITICAL")
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
				&applications.ServiceGroup{
					Name:             "myapp.default",
					Release:          "core/myapp/0.1.0/20190101121212",
					Status:           applications.HealthStatus_WARNING,
					HealthPercentage: 67,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:   3,
						Ok:      2,
						Warning: 1,
					},
				},
				&applications.ServiceGroup{
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 33,
					ServicesHealthCounts: &applications.HealthCounts{
						Total:    3,
						Ok:       1,
						Critical: 1,
						Unknown:  1,
					},
				},
				&applications.ServiceGroup{
					Name:             "redis.default",
					Release:          "core/redis/0.1.0/20190101121212",
					Status:           applications.HealthStatus_OK,
					HealthPercentage: 100,
					ServicesHealthCounts: &applications.HealthCounts{
						Total: 3,
						Ok:    3,
					},
				},
				&applications.ServiceGroup{
					Name:             "test.default",
					Release:          "core/test/0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
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

func TestServiceGroupsHealthOneWarning(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				&applications.ServiceGroup{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_WARNING,
					HealthPercentage: 0,
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
			"postgres", "0.1.0", "20190101121212", "WARNING")
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsHealthOneUnknown(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				&applications.ServiceGroup{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_UNKNOWN,
					HealthPercentage: 0,
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
			"postgres", "0.1.0", "20190101121212", "UNKNOWN")
	)
	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)
	assertServiceGroupsEqual(t, expected, response)
}
func TestServiceGroupsHealthOneEach(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsReq)
		expected = &applications.ServiceGroups{
			ServiceGroups: []*applications.ServiceGroup{
				&applications.ServiceGroup{
					Id:               "number",
					Name:             "postgres.default",
					Release:          "core/postgres/0.1.0/20190101121212",
					Status:           applications.HealthStatus_CRITICAL,
					HealthPercentage: 25,
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
				"postgres", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"postgres", "0.1.0", "20190101121212", "CRITICAL"),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	assertServiceGroupsEqual(t, expected, response)
}

func TestServiceGroupsHealthSortedDesc(t *testing.T) {
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
				"a", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL"),
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

func TestServiceGroupsHealthSortedAsc(t *testing.T) {
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
				"a", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL"),
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

func TestServiceGroupsHealthSortedPercent(t *testing.T) {
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
				"a", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "OK"),
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

func TestServiceGroupsHealthSortedPercentAsc(t *testing.T) {
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
				"a", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup6", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup7", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup8", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup9", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL"),
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

func TestServiceGroupsHealthPage(t *testing.T) {
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
				"a", "0.1.0", "20190101121212", "UNKNOWN"),
			NewHabServiceMsg("sup3", a, e, "default", "core",
				"b", "0.1.0", "20190101121212", "OK"),
			NewHabServiceMsg("sup4", a, e, "default", "core",
				"c", "0.1.0", "20190101121212", "WARNING"),
			NewHabServiceMsg("sup5", a, e, "default", "core",
				"d", "0.1.0", "20190101121212", "CRITICAL"),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	// b.default should be on the second page with default sorting
	assert.Equal(t, "b.default", response.ServiceGroups[0].Name)
}

func assertServiceGroupsEqual(t *testing.T, expected, actual *applications.ServiceGroups) {
	for i := range expected.ServiceGroups {
		assert.Equal(t,
			expected.ServiceGroups[i].Name,
			actual.ServiceGroups[i].Name,
			"The service_group name is not the expected one")
		assert.Equal(t,
			expected.ServiceGroups[i].Release,
			actual.ServiceGroups[i].Release,
			"The service_group release is not the expected one")
		assert.Equal(t,
			expected.ServiceGroups[i].HealthPercentage,
			actual.ServiceGroups[i].HealthPercentage,
			"The service_group health percentage is not the expected one")
		assert.Equal(t,
			expected.ServiceGroups[i].Status,
			actual.ServiceGroups[i].Status,
			"The service_group status is not the expected one")
		assert.Equal(t,
			expected.ServiceGroups[i].ServicesHealthCounts,
			actual.ServiceGroups[i].ServicesHealthCounts,
			"The services health counts from the service_group is not the expected one")
	}
}
