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
			IngestStats: &applications.IngestStats{
				TotalEventsProcessed:  0,
				TotalEventsFailed:     0,
				TotalEventsSuccessful: 0,
			},
		}
	)

	suite.Ingester.ResetStats()
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
			IngestStats: &applications.IngestStats{
				TotalEventsProcessed:  1,
				TotalEventsFailed:     0,
				TotalEventsSuccessful: 1,
			},
		}
		mockHabService = NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
		)
	)

	suite.Ingester.ResetStats()
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
			IngestStats: &applications.IngestStats{
				TotalEventsProcessed:  10,
				TotalEventsFailed:     0,
				TotalEventsSuccessful: 10,
			},
		}
		mockHabServicesMatrix = habServicesMatrix()
	)

	suite.Ingester.ResetStats()
	suite.IngestServices(mockHabServicesMatrix)
	defer suite.DeleteDataFromStorage()

	response, err := suite.ApplicationsServer.GetServicesStats(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}
