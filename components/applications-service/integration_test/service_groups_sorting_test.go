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
