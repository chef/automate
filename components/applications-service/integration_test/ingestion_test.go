//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"testing"

	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/proto"
	"github.com/stretchr/testify/assert"
)

func TestIngestSigleService(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	var (
		eventsProcessed = suite.Ingester.EventsProcessed()
		event           = NewHabitatEvent(
			withSupervisorId("4f1un3"),
			withPackageIdent("test/db/0.1.0/20200101121212"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("us"),
		)
	)

	bytes, err := proto.Marshal(event)
	if assert.Nil(t, err) {
		suite.Ingester.IngestMessage(bytes)
		suite.WaitForEventsToProcess(eventsProcessed + 1)

		svcList := suite.GetServices()
		assert.Equal(t, 1, len(svcList))

		assert.Equal(t, "4f1un3", svcList[0].SupMemberID,
			"the service supervisor_id is not the expected one")
		assert.Equal(t, "test", svcList[0].Origin,
			"the service origin name is not the expected one")
		assert.Equal(t, "db", svcList[0].Name,
			"the service name is not the expected one")
		assert.Equal(t, "0.1.0", svcList[0].Version,
			"the service version is not the expected one")
		assert.Equal(t, "20200101121212", svcList[0].Release,
			"the service release is not the expected one")
		assert.Equal(t, "OK", svcList[0].Health,
			"the service health is not the expected one")
		assert.Equal(t, "db.default", svcList[0].Group,
			"the service service group name is not the expected one")
		assert.Equal(t, "db.example.com", svcList[0].Fqdn,
			"the service fqdn is not the expected one")
		assert.Equal(t, "test-app", svcList[0].Application,
			"the service application name is not the expected one")
		assert.Equal(t, "development", svcList[0].Environment,
			"the service environment name is not the expected one")
		assert.Equal(t, "stable", svcList[0].Channel,
			"the service channel name is not the expected one")
		assert.Equal(t, "us", svcList[0].Site,
			"the service site name is not the expected one")
	}
}

func TestIngestMultiServicesDifferentServiceGroups(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	// Notice we send different supervisor id's
	events := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("s4l1m"),
			withPackageIdent("test/app/0.1.0/20200101121211"),
			withServiceGroup("app.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("app.example.com"),
			withSite("us"),
		),
		NewHabitatEvent(
			withSupervisorId("4f1un3"),
			withPackageIdent("test/web/0.1.0/20200101121212"),
			withServiceGroup("web.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("web.example.com"),
			withSite("us"),
		),
		NewHabitatEvent(
			withSupervisorId("m4y4"),
			withPackageIdent("test/db/0.1.0/20200101121213"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withSite("us"),
		),
	}

	// Ingest messages through the Ingester client
	suite.IngestMessagesViaIngester(events...)

	// Verify there are only three services
	svcList := suite.GetServices()
	assert.Equal(t, len(events), len(svcList), "wrong number of services")

	// Verify there are only three service_groups
	sgList := suite.GetServiceGroups()
	assert.Equal(t, 3, len(sgList), "wrong number of service groups")
}

func TestIngestMultiServicesSameServiceGroup(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	events := []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("s4l1m"),
			withPackageIdent("test/db/0.1.0/20200101121211"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("us"),
		),
		NewHabitatEvent(
			withSupervisorId("4f1un3"),
			withPackageIdent("test/db/0.1.0/20200101121212"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("us"),
		),
		NewHabitatEvent(
			withSupervisorId("m4y4"),
			withPackageIdent("test/db/0.1.0/20200101121213"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("us"),
		),
	}

	// Ingest messages through the Ingester client
	suite.IngestMessagesViaIngester(events...)

	// Verify there are only three services
	svcList := suite.GetServices()
	assert.Equal(t, len(events), len(svcList), "wrong number of services")

	// We have a race condition where if we have different services sending messages
	// to Automate at the same time that belongs to the same service-group we will
	// insert different service-groups and deployments entries instead of a single one.
	//
	// The main reason of this problem is because we process 50 things at a time (worker pool)
	//
	// TODO @afiune fix and uncomment this test!
	//
	// Verify there is only one service_group
	//sgList := suite.GetServiceGroups()
	//assert.Equal(t, 1, len(sgList), "there should be only one single service group")
}

func TestIngestSigleServiceInsertAndUpdate(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	// 1) Insert new service
	var (
		eventsProcessed = suite.Ingester.EventsProcessed()
		event           = NewHabitatEvent(
			withSupervisorId("4f1un3"),
			withPackageIdent("test/db/0.1.0/20200101121212"),
			withServiceGroup("db.default"),
			withStrategyAtOnce("stable"),
			withApplication("test-app"),
			withEnvironment("development"),
			withFqdn("db.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("us"),
		)
	)

	bytes, err := proto.Marshal(event)
	if assert.Nil(t, err) {
		suite.Ingester.IngestMessage(bytes)
		eventsProcessed++
		suite.WaitForEventsToProcess(eventsProcessed)

		svcList := suite.GetServices()
		assert.Equal(t, 1, len(svcList))

		assert.Equal(t, "4f1un3", svcList[0].SupMemberID,
			"the service supervisor_id is not the expected one")
		assert.Equal(t, "test", svcList[0].Origin,
			"the service origin name is not the expected one")
		assert.Equal(t, "db", svcList[0].Name,
			"the service name is not the expected one")
		assert.Equal(t, "0.1.0", svcList[0].Version,
			"the service version is not the expected one")
		assert.Equal(t, "20200101121212", svcList[0].Release,
			"the service release is not the expected one")
		assert.Equal(t, "OK", svcList[0].Health,
			"the service health is not the expected one")
		assert.Equal(t, "db.default", svcList[0].Group,
			"the service service group name is not the expected one")
		assert.Equal(t, "db.example.com", svcList[0].Fqdn,
			"the service fqdn is not the expected one")
		assert.Equal(t, "test-app", svcList[0].Application,
			"the service application name is not the expected one")
		assert.Equal(t, "development", svcList[0].Environment,
			"the service environment name is not the expected one")
		assert.Equal(t, "stable", svcList[0].Channel,
			"the service channel name is not the expected one")
		assert.Equal(t, "us", svcList[0].Site,
			"the service site name is not the expected one")

		sgList := suite.GetServiceGroups()
		assert.Equal(t, 1, len(sgList), "wrong number of service groups")

		assert.Equal(t, "db.default", sgList[0].Name,
			"the service_group name is not the expected one")
		assert.Equal(t, "test/db", sgList[0].Package,
			"the service_group package is not the expected one")
		assert.Equal(t, "0.1.0/20200101121212", sgList[0].Release,
			"the service_group release is not the expected one")
		assert.Equal(t, "OK", sgList[0].HealthStatus,
			"the service_group health status is not the expected one")
		assert.Equal(t, int32(100), sgList[0].HealthPercentage,
			"the service_group health percentage is not the expected one")
		assert.Equal(t, "test-app", sgList[0].Application,
			"the service_group application name is not the expected one")
		assert.Equal(t, "development", sgList[0].Environment,
			"the service_group environment name is not the expected one")
		assert.Equal(t, int32(1), sgList[0].ServicesHealthCounts.Total,
			"the total number of services in this service_group is not the expected one")
		assert.Equal(t, int32(1), sgList[0].ServicesHealthCounts.Ok,
			"the OK number of services in this service_group is not the expected one")
	}

	// 2) Update same service
	//
	// TODO @afiune define what are the fields that we can updated?
	// - Health
	// - Package ident (without name. the name of a service can't be changed!)
	// - Update strategy

	UpdateHabitatEvent(event,
		withHealth(HealthCheckIntToString(2)), // -> CRITICAL
		withStrategyAtOnce("unstable"),
		withPackageIdent("changed/db/3.2.1/20201212000000"),
	)

	bytes, err = proto.Marshal(event)
	if assert.Nil(t, err) {
		suite.Ingester.IngestMessage(bytes)
		eventsProcessed++
		suite.WaitForEventsToProcess(eventsProcessed)

		svcList := suite.GetServices()
		assert.Equal(t, 1, len(svcList))

		assert.Equal(t, "4f1un3", svcList[0].SupMemberID,
			"the service supervisor_id is not the expected one")
		assert.Equal(t, "changed", svcList[0].Origin,
			"the service origin name is not the expected one")
		assert.Equal(t, "db", svcList[0].Name,
			"the service name is not the expected one")
		assert.Equal(t, "3.2.1", svcList[0].Version,
			"the service version is not the expected one")
		assert.Equal(t, "20201212000000", svcList[0].Release,
			"the service release is not the expected one")
		assert.Equal(t, "CRITICAL", svcList[0].Health,
			"the service health is not the expected one")
		assert.Equal(t, "db.default", svcList[0].Group,
			"the service service group name is not the expected one")
		assert.Equal(t, "db.example.com", svcList[0].Fqdn,
			"the service fqdn is not the expected one")
		assert.Equal(t, "test-app", svcList[0].Application,
			"the service application name is not the expected one")
		assert.Equal(t, "development", svcList[0].Environment,
			"the service environment name is not the expected one")
		assert.Equal(t, "unstable", svcList[0].Channel,
			"the service channel name is not the expected one")
		assert.Equal(t, "us", svcList[0].Site,
			"the service site name is not the expected one")

		sgList := suite.GetServiceGroups()
		assert.Equal(t, 1, len(sgList), "wrong number of service groups")

		assert.Equal(t, "db.default", sgList[0].Name,
			"the service_group name is not the expected one")
		assert.Equal(t, "changed/db", sgList[0].Package,
			"the service_group package is not the expected one")
		assert.Equal(t, "3.2.1/20201212000000", sgList[0].Release,
			"the service_group release is not the expected one")
		assert.Equal(t, "CRITICAL", sgList[0].HealthStatus,
			"the service_group health status is not the expected one")
		assert.Equal(t, int32(0), sgList[0].HealthPercentage,
			"the service_group health percentage is not the expected one")
		assert.Equal(t, "test-app", sgList[0].Application,
			"the service_group application name is not the expected one")
		assert.Equal(t, "development", sgList[0].Environment,
			"the service_group environment name is not the expected one")
		assert.Equal(t, int32(1), sgList[0].ServicesHealthCounts.Total,
			"the total number of services in this service_group is not the expected one")
		assert.Equal(t, int32(1), sgList[0].ServicesHealthCounts.Critical,
			"the OK number of services in this service_group is not the expected one")
	}

	// 3) Extra update to verify that the update strategy is updated
	UpdateHabitatEvent(event,
		withoutUpdateStrategy(),
	)

	bytes, err = proto.Marshal(event)
	if assert.Nil(t, err) {
		suite.Ingester.IngestMessage(bytes)
		eventsProcessed++
		suite.WaitForEventsToProcess(eventsProcessed)

		svcList := suite.GetServices()
		assert.Equal(t, 1, len(svcList))
		assert.Equal(t, "", svcList[0].Channel,
			"the service channel name is not the expected one")
	}
}
