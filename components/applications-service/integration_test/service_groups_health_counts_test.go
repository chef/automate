//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/applications"
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
		mockHabService = NewHabServiceMsg("1234", "test", "test", "test",
			"core", "redis", "0.1.0", "20190101121212", "OK", "", "")
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
		updateHabServicesMatrix = []*applications.HabService{
			NewHabServiceMsg("sup1", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup2", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core", "test", "0.1.0", "20190101121212", "OK", "", ""),
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
		updateHabServicesMatrix = []*applications.HabService{
			NewHabServiceMsg("sup1", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup2", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "OK", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "WARNING", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core", "test", "0.1.0", "20190101121212", "WARNING", "", ""),
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
		updateHabServicesMatrix = []*applications.HabService{
			NewHabServiceMsg("sup2", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "CRITICAL", "", ""),
			NewHabServiceMsg("sup3", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "CRITICAL", "", ""),
			NewHabServiceMsg("sup4", a, e, "default", "core", "test", "0.1.0", "20190101121212", "CRITICAL", "", ""),
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
