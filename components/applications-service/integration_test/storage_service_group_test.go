//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStorageGetServiceGroupNotExist(t *testing.T) {
	name, exist := suite.StorageClient.ServiceGroupExists("1")
	assert.False(t, exist)
	assert.Equal(t, "", name)
}

func TestStorageGetServiceGroupEmptyID(t *testing.T) {
	name, exist := suite.StorageClient.ServiceGroupExists("")
	assert.False(t, exist)
	assert.Equal(t, "", name)
}

func TestStorageGetServiceGroupExist(t *testing.T) {
	suite.IngestService(
		NewHabServiceChannelMsg("1q2w3e4r", a, e, "default",
			"core", "postgres", "0.1.0", "20190101121212", "OK", "stable", "testsite"),
	)
	defer suite.DeleteDataFromStorage()

	// Get the ID from the service group
	sgList := suite.GetServiceGroups()
	if assert.Equal(t, 1, len(sgList), "There should be one service_group in the db") {
		name, exist := suite.StorageClient.ServiceGroupExists(fmt.Sprintf("%d", sgList[0].ID))
		assert.True(t, exist)
		assert.Equal(t, sgList[0].Name, name)
	}
}
