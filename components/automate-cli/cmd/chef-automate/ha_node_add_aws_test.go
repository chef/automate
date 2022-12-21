package main

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestAddnodeAWSValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{automateCount: 0}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) { 
			return nil, &SSHConfig{}, nil 
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Either one of automate-count or chef-server-count or opensearch-count or postgresql-count must be more than 0.")
}

func TestFailedToReadConfigInValidation (t *testing.T) {
}

// func TestV