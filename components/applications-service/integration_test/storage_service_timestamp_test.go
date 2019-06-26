//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestStorageGetServiceFromUniqueFieldsNotExist(t *testing.T) {
	svc, exist := suite.StorageClient.GetServiceFromUniqueFields("foo", "123")
	assert.False(t, exist)
	assert.Nil(t, svc)
}

func TestStorageGetServiceFromUniqueFieldsEmptyParameters(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	suite.IngestService(
		NewHabitatEvent(
			withSupervisorId("1q2w3e4r"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withStrategyAtOnce("stable"),
			withApplication("cool-app"),
			withEnvironment("development"),
			withFqdn("app.example.com"),
			withHealth("CRITICAL"),
			withSite("us"),
		),
	)

	svc, exist := suite.StorageClient.GetServiceFromUniqueFields("", "")
	assert.False(t, exist)
	assert.Nil(t, svc)

	svc, exist = suite.StorageClient.GetServiceFromUniqueFields("postgres", "")
	assert.False(t, exist)
	assert.Nil(t, svc)

	svc, exist = suite.StorageClient.GetServiceFromUniqueFields("", "1q2w3e4r")
	assert.False(t, exist)
	assert.Nil(t, svc)
}

func TestStorageGetServiceFromUniqueFieldsExist(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	suite.IngestService(
		NewHabitatEvent(
			withSupervisorId("1q2w3e4r"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withStrategyAtOnce("stable"),
			withApplication("cool-app"),
			withEnvironment("development"),
			withFqdn("app.example.com"),
			withHealth("CRITICAL"),
			withSite("us"),
		),
	)

	svc, exist := suite.StorageClient.GetServiceFromUniqueFields("postgres", "1q2w3e4r")
	if assert.True(t, exist) {
		assert.NotEmpty(t, svc.ID)
		assert.Equal(t, "1q2w3e4r", svc.SupMemberID)
		assert.Equal(t, "core", svc.Origin)
		assert.Equal(t, "postgres", svc.Name)
		assert.Equal(t, "0.1.0", svc.Version)
		assert.Equal(t, "20190101121212", svc.Release)
		assert.Equal(t, "CRITICAL", svc.Health)
		assert.Equal(t, "postgres.default", svc.Group)
		assert.Equal(t, "app.example.com", svc.Fqdn)
		assert.Equal(t, "cool-app", svc.Application)
		assert.Equal(t, "development", svc.Environment)
		assert.Equal(t, "stable", svc.Channel)
		assert.Equal(t, "us", svc.Site)
		assert.NotEmpty(t, svc.LastEventOccurredAt)
		assert.True(t, svc.LastEventOccurredAt.Before(time.Now()))
	}
}

func TestStorageGetServiceFromUniqueFieldsExistUpdateLastEventOccurredAt(t *testing.T) {
	defer suite.DeleteDataFromStorage()

	suite.IngestService(
		NewHabitatEvent(
			withSupervisorId("1q2w3e4r"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
		),
	)
	var (
		initialOccurredAtTimestamp time.Time
		updatedOccurredAtTimestamp time.Time
	)

	svc, exist := suite.StorageClient.GetServiceFromUniqueFields("postgres", "1q2w3e4r")
	if assert.True(t, exist) {
		initialOccurredAtTimestamp = svc.LastEventOccurredAt
	}

	// Trigger an update but wait for half a second before creating the new event
	time.Sleep(500 * time.Millisecond)
	suite.IngestService(
		NewHabitatEvent(
			withSupervisorId("1q2w3e4r"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("CRITICAL"),
		),
	)

	svc, exist = suite.StorageClient.GetServiceFromUniqueFields("postgres", "1q2w3e4r")
	if assert.True(t, exist) {
		updatedOccurredAtTimestamp = svc.LastEventOccurredAt

		// the updated timestamp must be after the initial one
		assert.Truef(t, updatedOccurredAtTimestamp.After(initialOccurredAtTimestamp),
			"last_event_occurred_at time was not updated after an update")
		// TODO update testify go package to use this
		// the substraction of the initial and the updated timestamp should be greater than a second
		//assert.LessOrEqual(t, 500*time.Millisecond, updatedOccurredAtTimestamp.Sub(initialOccurredAtTimestamp))
	}
}
