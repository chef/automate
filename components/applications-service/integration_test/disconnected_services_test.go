//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/applications"
)

func TestGetDisconnectedServicesMustProvideThresholdError(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = new(applications.DisconnectedServicesReq)
		expected = new(applications.ServicesRes)
	)
	response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
	assert.NotNil(t, err)
	assert.Contains(t, err.Error(), "InvalidArgument")
	assert.Contains(t, err.Error(), "threshold must be greater than zero")
	assert.Equal(t, expected, response)
}

func TestGetDisconnectedServicesBasicNoData(t *testing.T) {
	var (
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 5}
		expected = &applications.ServicesRes{Services: []*applications.Service{}}
	)
	response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
}

func TestGetDisconnectedServicesBasicSingleServiceMockedAsDisconnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		err      error
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId:        "abcd",
					Group:               "postgres.default",
					Release:             "core/postgres/0.1.0/20190101121212",
					Status:              applications.ServiceStatus_RUNNING,
					HealthCheck:         applications.HealthStatus_OK,
					Fqdn:                "mytest.example.com",
					Channel:             "testchannel",
					Site:                "testsite",
					PreviousHealthCheck: applications.HealthStatus_NONE,
					Application:         a, Environment: e,
				},
			},
		}
	)

	event := NewHabitatEvent(
		withSupervisorId("abcd"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withStrategyAtOnce("testchannel"),
		withFqdn("mytest.example.com"),
		withSite("testsite"),
	)

	// Patch event timestamp to mock an old service message and mack it as disconnected
	event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 5))
	assert.Nil(t, err)
	suite.IngestService(event)

	response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetDisconnectedServicesMultiServicesMixedConnectedAndDisconnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		err      error
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{
			Services: []*applications.Service{
				{
					SupervisorId:        "efgh",
					Group:               "postgres.default",
					Release:             "core/postgres/0.1.0/20190101121212",
					Status:              applications.ServiceStatus_RUNNING,
					HealthCheck:         applications.HealthStatus_OK,
					Fqdn:                "mytest.example.com",
					Channel:             "testchannel",
					Site:                "testsite",
					PreviousHealthCheck: applications.HealthStatus_NONE,
					Application:         a, Environment: e,
				},
			},
		}
	)

	// Ingest service number one
	event := NewHabitatEvent(
		withSupervisorId("abcd"),
		withServiceGroup("postgres.default"),
		withPackageIdent("core/postgres/0.1.0/20190101121212"),
		withStrategyAtOnce("testchannel"),
		withFqdn("mytest.example.com"),
		withSite("testsite"),
	)
	suite.IngestService(event)

	// Ingest service number two, different service because the supervisor id changes
	UpdateHabitatEvent(event,
		withSupervisorId("efgh"),
	)
	// Patch event timestamp to mock an old service message and mack it as disconnected
	event.EventMetadata.OccurredAt, err = ptypes.TimestampProto(time.Now().Add(-time.Minute * 5))
	assert.Nil(t, err)
	suite.IngestService(event)

	// We should only have a single disconnected service back
	response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
	assert.Nil(t, err)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}

func TestGetDisconnectedServicesMultiServicesAllConnected(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx      = context.Background()
		request  = &applications.DisconnectedServicesReq{ThresholdMinutes: 3}
		expected = &applications.ServicesRes{Services: []*applications.Service{}}
	)

	suite.IngestServices(habServicesMatrix())

	// We should have no disconnected services
	response, err := suite.ApplicationsServer.GetDisconnectedServices(ctx, request)
	assert.Nil(t, err)
	assert.Equal(t, expected, response)
	assertServicesEqual(t, expected.GetServices(), response.GetServices())
}
