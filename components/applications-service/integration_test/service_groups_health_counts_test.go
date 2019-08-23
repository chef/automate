//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/stretchr/testify/assert"
)

func TestServiceGroupsHealthCountsBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = new(applications.HealthCounts)
	)
	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)

	assert.Equal(t, response.Total, int32(0))
	assert.Equal(t, response.Ok, int32(0))
	assert.Equal(t, response.Warning, int32(0))
	assert.Equal(t, response.Critical, int32(0))
	assert.Equal(t, response.Unknown, int32(0))
}

func TestServiceGroupsHealthCountsSingleOkService(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = &applications.HealthCounts{
			Total: int32(1),
			Ok:    int32(1),
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("1234"),
			withServiceGroup("redis.test"),
			withPackageIdent("core/redis/0.1.0/20190101121212"),
		)
	)

	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)
}

func TestServiceGroupsHealthCountsMultiService(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(1),
			Warning:  int32(1),
			Critical: int32(1),
			Unknown:  int32(1),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)
}

func TestServiceGroupsHealthCountsOnServiceUpdateAllOk(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(1),
			Warning:  int32(1),
			Critical: int32(1),
			Unknown:  int32(1),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)

	// Update all services to be reported as OK
	var (
		updateHabServicesMatrix = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("myapp.default"),
				withPackageIdent("core/myapp/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("test.default"),
				withPackageIdent("core/test/0.1.0/20190101121212"),
			),
		}
		expectedAfterUpdate = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(4),
			Warning:  int32(0),
			Critical: int32(0),
			Unknown:  int32(0),
		}
	)

	suite.IngestServices(updateHabServicesMatrix)

	response, err = suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expectedAfterUpdate)
}

func TestServiceGroupsHealthCountsOnServiceUpdateAllWarning(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(1),
			Warning:  int32(1),
			Critical: int32(1),
			Unknown:  int32(1),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)

	// Update all services to be reported as WARNING
	var (
		updateHabServicesMatrix = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup1"),
				withServiceGroup("redis.default"),
				withPackageIdent("core/redis/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("postgres.default"),
				withPackageIdent("core/postgres/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("test.default"),
				withPackageIdent("core/test/0.1.0/20190101121212"),
				withHealth("WARNING"),
			),
		}
		expectedAfterUpdate = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(0),
			Warning:  int32(4),
			Critical: int32(0),
			Unknown:  int32(0),
		}
	)

	suite.IngestServices(updateHabServicesMatrix)

	response, err = suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expectedAfterUpdate)
}

func TestServiceGroupsHealthCountsOnServiceUpdateAllCritical(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServiceGroupsHealthCountsReq)
		expected = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(1),
			Warning:  int32(1),
			Critical: int32(1),
			Unknown:  int32(1),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)

	// Update all services to be reported as CRITICAL
	var (
		updateHabServicesMatrix = []*habitat.HealthCheckEvent{
			NewHabitatEvent(
				withSupervisorId("sup2"),
				withServiceGroup("redis.default"),
				withPackageIdent("core/redis/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup3"),
				withServiceGroup("myapp.default"),
				withPackageIdent("core/myapp/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
			NewHabitatEvent(
				withSupervisorId("sup4"),
				withServiceGroup("test.default"),
				withPackageIdent("core/test/0.1.0/20190101121212"),
				withHealth("CRITICAL"),
			),
		}
		expectedAfterUpdate = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(0),
			Warning:  int32(0),
			Critical: int32(4),
			Unknown:  int32(0),
		}
	)

	suite.IngestServices(updateHabServicesMatrix)

	response, err = suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expectedAfterUpdate)
}

func TestServiceGroupsHealthCountsFilter(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsHealthCountsReq{
			Filter: []string{"service:myapp"},
		}
		expected = &applications.HealthCounts{
			Total:    int32(1),
			Ok:       int32(0),
			Warning:  int32(1),
			Critical: int32(0),
			Unknown:  int32(0),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)
}

func TestServiceGroupsHealthCountsStatusFilterIgnored(t *testing.T) {
	var (
		ctx     = context.Background()
		request = &applications.ServiceGroupsHealthCountsReq{
			Filter: []string{"status:OK"},
		}
		expected = &applications.HealthCounts{
			Total:    int32(4),
			Ok:       int32(1),
			Warning:  int32(1),
			Critical: int32(1),
			Unknown:  int32(1),
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServiceGroupsHealthCounts(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, response, expected)
}
