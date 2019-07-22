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

func TestStatsWhenEmpty(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServicesStatsReq)
		expected = &applications.ServicesStatsRes{
			TotalServices:      0,
			TotalServiceGroups: 0,
			TotalSupervisors:   0,
			TotalDeployments:   0,
		}
	)

	response, err := suite.ApplicationsServer.GetServicesStats(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}

func TestStatsWithOne(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.ServicesStatsReq)
		expected = &applications.ServicesStatsRes{
			TotalServices:      1,
			TotalServiceGroups: 1,
			TotalSupervisors:   1,
			TotalDeployments:   1,
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
		)
	)

	suite.IngestService(mockHabService)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServicesStats(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}

func TestStatsWithSeveral(t *testing.T) {
	var (
		ctx     = context.Background()
		request = new(applications.ServicesStatsReq)
		// FIXME
		expected = &applications.ServicesStatsRes{
			TotalServices:      10,
			TotalServiceGroups: 4,
			TotalSupervisors:   4,
			TotalDeployments:   1,
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServicesStats(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}
