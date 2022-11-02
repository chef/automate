package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIsBastionHost(t *testing.T) {
	impl := NewBackupFromBashtion()
	bastionHost := impl.isBastionHost()
	assert.True(t, bastionHost)
}

func TestExecuteAndPoolStatusWithInvalidConfig(t *testing.T) {
	impl := NewBackupFromBashtion()
	err := impl.executeOnRemoteAndPoolStatus("chef-automate backup create", &AutomteHAInfraDetails{})
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Invalid deployment config")
}
