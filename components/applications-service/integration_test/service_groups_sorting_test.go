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
	"github.com/chef/automate/api/external/habitat"
	"github.com/stretchr/testify/assert"
)

func TestGetServiceGroupsSortedDesc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "name",
				Order: query.SortOrder_DESC,
			},
		}
		mockHabServices = habServicesABCD()
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		assert.Equal(t, "d.default", response.ServiceGroups[0].Name)
		assert.Equal(t, "c.default", response.ServiceGroups[1].Name)
		assert.Equal(t, "b.default", response.ServiceGroups[2].Name)
		assert.Equal(t, "a.default", response.ServiceGroups[3].Name)
	}
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
		mockHabServices = habServicesABCD()
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		assert.Equal(t, "a.default", response.ServiceGroups[0].Name)
		assert.Equal(t, "b.default", response.ServiceGroups[1].Name)
		assert.Equal(t, "c.default", response.ServiceGroups[2].Name)
		assert.Equal(t, "d.default", response.ServiceGroups[3].Name)
	}
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
		mockHabServices = habServicesABCD()
		expectedOrder   = []string{"a", "b", "c", "d"}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		for i, sg := range response.ServiceGroups {
			assert.Equal(t, fmt.Sprintf("%s.default", expectedOrder[i]), sg.Name,
				"the service_group name is not the expected one")
			assert.Equal(t, fmt.Sprintf("%s_app", expectedOrder[i]), sg.Application,
				"the service_group application name is not the expected one")
		}
	}
}

func TestGetServiceGroupsSortedApplicationDesc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "app_name",
				Order: query.SortOrder_DESC,
			},
		}
		mockHabServices = habServicesABCD()
		expectedOrder   = []string{"d", "c", "b", "a"}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		for i, sg := range response.ServiceGroups {
			assert.Equal(t, fmt.Sprintf("%s.default", expectedOrder[i]), sg.Name,
				"the service_group name is not the expected one")
			assert.Equal(t, fmt.Sprintf("%s_app", expectedOrder[i]), sg.Application,
				"the service_group application name is not the expected one")
		}
	}
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
		mockHabServices = habServicesABCD()
		expectedOrder   = []string{"a", "b", "c", "d"}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		for i, sg := range response.ServiceGroups {
			assert.Equal(t, fmt.Sprintf("%s.default", expectedOrder[i]), sg.Name,
				"the service_group name is not the expected one")
			assert.Equal(t, fmt.Sprintf("%s_env", expectedOrder[i]), sg.Environment,
				"the service_group environment name is not the expected one")
		}
	}
}

func TestGetServiceGroupsSortedEnvironmentDesc(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "environment",
				Order: query.SortOrder_DESC,
			},
		}
		mockHabServices = habServicesABCD()
		expectedOrder   = []string{"d", "c", "b", "a"}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		for i, sg := range response.ServiceGroups {
			assert.Equal(t, fmt.Sprintf("%s.default", expectedOrder[i]), sg.Name,
				"the service_group name is not the expected one")
			assert.Equal(t, fmt.Sprintf("%s_env", expectedOrder[i]), sg.Environment,
				"the service_group environment name is not the expected one")
		}
	}
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
		mockHabServices = append(habServicesABCD(),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("c.default"),
				withPackageIdent("core/c/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup6"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup7"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
			),
			NewHabitatEvent(
				withSupervisorId("sup8"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
		)
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		assert.Equal(t, int32(100), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(50), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(25), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[3].HealthPercentage)
	}
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
		mockHabServices = append(habServicesABCD(),
			NewHabitatEvent(
				withSupervisorId("sup5"),
				withServiceGroup("c.default"),
				withPackageIdent("core/c/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup6"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup7"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
			),
			NewHabitatEvent(
				withSupervisorId("sup8"),
				withServiceGroup("d.default"),
				withPackageIdent("core/d/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
		)
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		assert.Equal(t, int32(0), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(25), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(50), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(100), response.ServiceGroups[3].HealthPercentage)
	}
}

func TestGetServiceGroupsDoubleSortingPercentOkASC(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "percent_ok",
				Order: query.SortOrder_ASC,
			},
		}
		// There are four groups here, three of them are 0% OK
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("ok.default"),
				withPackageIdent("core/ok/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("unknown.default"),
				withPackageIdent("core/unknown/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("warning.default"),
				withPackageIdent("core/warning/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("critical.default"),
				withPackageIdent("core/critical/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		// The service groups should be ordered by 'percent_ok'
		assert.Equal(t, int32(0), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(100), response.ServiceGroups[3].HealthPercentage)

		// But within the same percent they should be ordered by health criticality
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[0].Status)
		assert.Equal(t, applications.HealthStatus_UNKNOWN, response.ServiceGroups[1].Status)
		assert.Equal(t, applications.HealthStatus_WARNING, response.ServiceGroups[2].Status)
		assert.Equal(t, applications.HealthStatus_OK, response.ServiceGroups[3].Status)
	}
}

func TestGetServiceGroupsDoubleSortingPercentOkDESC(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "percent_ok",
				Order: query.SortOrder_DESC,
			},
		}
		// There are four groups here, three of them are 0% OK
		mockHabServices = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("ok.default"),
				withPackageIdent("core/ok/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("unknown.default"),
				withPackageIdent("core/unknown/0.1.0/20190101121212"),
				withHealth("UNKNOWN"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("warning.default"),
				withPackageIdent("core/warning/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("critical.default"),
				withPackageIdent("core/critical/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		// The service groups should be ordered by 'percent_ok'
		assert.Equal(t, int32(100), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[3].HealthPercentage)

		// But within the same percent they should be ordered by health criticality
		assert.Equal(t, applications.HealthStatus_OK, response.ServiceGroups[0].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[1].Status)
		assert.Equal(t, applications.HealthStatus_UNKNOWN, response.ServiceGroups[2].Status)
		assert.Equal(t, applications.HealthStatus_WARNING, response.ServiceGroups[3].Status)
	}
}

func TestGetServiceGroupsDoubleSortingHealthASC(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "health",
				Order: query.SortOrder_ASC,
			},
		}
		// There are four groups here, three of them are CRITICAL
		mockHabServices = []*habitat.HealthCheckEvent{
			// 100% OK
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("ok.default"),
				withPackageIdent("core/ok/0.1.0/20190101121212"),
			),
			// 50% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s1.default"),
				withPackageIdent("core/s1/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("s1.default"),
				withPackageIdent("core/s1/0.1.0/20190101121212"),
			),
			// 75% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			// 0% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s3.default"),
				withPackageIdent("core/s3/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		// The service groups should be ordered by 'health'
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[0].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[1].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[2].Status)
		assert.Equal(t, applications.HealthStatus_OK, response.ServiceGroups[3].Status)

		// But within the same health criticality they should be ordered by group percent ok
		assert.Equal(t, int32(0), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(50), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(75), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(100), response.ServiceGroups[3].HealthPercentage)
	}
}

func TestGetServiceGroupsDoubleSortingHealthDESC(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsReq{
			Sorting: &query.Sorting{
				Field: "health",
				Order: query.SortOrder_DESC,
			},
		}
		// There are four groups here, three of them are CRITICAL
		mockHabServices = []*habitat.HealthCheckEvent{
			// 100% OK
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("ok.default"),
				withPackageIdent("core/ok/0.1.0/20190101121212"),
			),
			// 50% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s1.default"),
				withPackageIdent("core/s1/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("s1.default"),
				withPackageIdent("core/s1/0.1.0/20190101121212"),
			),
			// 75% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("s2.default"),
				withPackageIdent("core/s2/0.1.0/20190101121212"),
			),
			// 0% CRITICAL
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("s3.default"),
				withPackageIdent("core/s3/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
	)
	suite.IngestServices(mockHabServices)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroups(ctx, request)
	assert.Nil(t, err)

	if assert.Equal(t, 4, len(response.ServiceGroups)) {
		// The service groups should be ordered by 'health'
		assert.Equal(t, applications.HealthStatus_OK, response.ServiceGroups[0].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[1].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[2].Status)
		assert.Equal(t, applications.HealthStatus_CRITICAL, response.ServiceGroups[3].Status)

		// But within the same health criticality they should be ordered by group percent ok
		assert.Equal(t, int32(100), response.ServiceGroups[0].HealthPercentage)
		assert.Equal(t, int32(0), response.ServiceGroups[1].HealthPercentage)
		assert.Equal(t, int32(50), response.ServiceGroups[2].HealthPercentage)
		assert.Equal(t, int32(75), response.ServiceGroups[3].HealthPercentage)
	}
}
