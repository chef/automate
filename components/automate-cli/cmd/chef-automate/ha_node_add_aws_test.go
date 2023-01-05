package main

import (
	"fmt"
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
			return AWS_MODE, nil
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

func TestAddnodeAWSValidate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
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
	assert.NoError(t, err)
	assert.Equal(t, "3", nodeAdd.(*AddNodeAWSImpl).config.Opensearch.Config.InstanceCount)
}

func TestAddnodeAWSValidateErrorIsManagedServicesOn(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return true
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "add"))
}

func TestAddnodeModifyAws(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		chefServerCount: 1,
	}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
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
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
}

func TestAddAwsnodePrompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{automateCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
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
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "6", nodeAdd.(*AddNodeAWSImpl).config.Automate.Config.InstanceCount)
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing node count:
================================================
Automate => 5
Chef-Server => 1
OpenSearch => 3
Postgresql => 4

New node count:
================================================
Automate => 6
Chef-Server => 1
OpenSearch => 3
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestAddnodeDeployWithNewOSNodeInAws(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, deployed,autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigAWSfunc: func(path string) error {
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
		moveAWSAutoTfvarsFileFunc: func (path string) error {
			autoFileMoved = true
			return nil
		},
		modifyTfArchFileFunc: func (path string) error {
			tfArchModified = true
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			filewritten = true
			return nil
		},
	}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "4", nodeAdd.(*AddNodeAWSImpl).config.Opensearch.Config.InstanceCount)
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing node count:
================================================
Automate => 5
Chef-Server => 1
OpenSearch => 3
Postgresql => 4

New node count:
================================================
Automate => 5
Chef-Server => 1
OpenSearch => 4
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeWithExecuteFun(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, deployed,autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigAWSfunc: func(path string) error {
			return nil
		},
		isA2HARBFileExistFunc: func() bool {
			return true
		},
		taintTerraformFunc: func(path string) error {
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
		moveAWSAutoTfvarsFileFunc: func (path string) error {
			autoFileMoved = true
			return nil
		},
		modifyTfArchFileFunc: func (path string) error {
			tfArchModified = true
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			filewritten = true
			return nil
		},
		
	}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing node count:
================================================
Automate => 5
Chef-Server => 1
OpenSearch => 3
Postgresql => 4

New node count:
================================================
Automate => 5
Chef-Server => 1
OpenSearch => 4
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}