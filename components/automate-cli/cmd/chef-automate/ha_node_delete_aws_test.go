package main

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	ipAddressNotPresent = `IP address validation failed: 
Automate Ip 193.0.0.1 is not present in existing list of ip addresses. Please use a different private ip.
Unable to remove node. Chef-Server instance count cannot be less than 1. Final count 0 not allowed.
Chef-Server Ip 193.0.0.1 is not present in existing list of ip addresses. Please use a different private ip.
Unable to remove node. OpenSearch instance count cannot be less than 3. Final count 2 not allowed.
OpenSearch Ip 193.0.0.1 is not present in existing list of ip addresses. Please use a different private ip.
Postgresql Ip 193.0.0.1 is not present in existing list of ip addresses. Please use a different private ip.`
	multipleIpAddressError = `IP address validation failed: 
Only one Automate is allowed to delete for AWS deployment type
Only one Chef-Server is allowed to delete for AWS deployment type
Only one OpenSearch is allowed to delete for AWS deployment type
Only one Postgresql is allowed to delete for AWS deployment type`
	isManagedServicesError = `Cannot remove OpenSearch or Postgresql nodes if external.database.type is either aws or self-managed.
Please set external.database.type to empty if you want to add OpenSearch or Postgresql nodes`
)

func TestDeletenodeAWSValidateIsManagedServicesOnError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{opensearchIp: "192.0.0.1"}
	isManagedServicesOn := false
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				isManagedServicesOn = true
				return true
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.(*DeleteNodeAWSImpl).getAwsHAIp()
	assert.NoError(t, err)
	err = nodeDelete.validate()
	assert.True(t, isManagedServicesOn)
	assert.Contains(t, err.Error(), isManagedServicesError)
}

func TestDeletenodeAWSValidateErrorIpAddressNotMatched(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp:   "193.0.0.1",
		chefServerIp: "193.0.0.1",
		opensearchIp: "193.0.0.1",
		postgresqlIp: "193.0.0.1",
	}
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.(*DeleteNodeAWSImpl).getAwsHAIp()
	assert.NoError(t, err)
	err = nodeDelete.validate()
	assert.Equal(t, err.Error(), ipAddressNotPresent)
}

func TestDeletenodeAWSValidateErrorMoreThenOneIpAddress(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp:   "193.0.0.1,193.0.0.2",
		chefServerIp: "193.0.0.1,193.0.0.2",
		opensearchIp: "193.0.0.1,193.0.0.2",
		postgresqlIp: "193.0.0.1,193.0.0.2",
	}
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.(*DeleteNodeAWSImpl).getAwsHAIp()
	assert.NoError(t, err)
	err = nodeDelete.validate()
	assert.Contains(t, err.Error(), multipleIpAddressError)
}

func TestDeletenodeAWSModify(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.(*DeleteNodeAWSImpl).getAwsHAIp()
	assert.NoError(t, err)
	err = nodeDelete.validate()
	assert.NoError(t, err)
	err = nodeDelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "4", nodeDelete.(*DeleteNodeAWSImpl).config.Automate.Config.InstanceCount)
}

func TestDeleteAwsnodePrompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.validate()
	assert.NoError(t, err)
	err = nodeDelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "4", nodeDelete.(*DeleteNodeAWSImpl).config.Automate.Config.InstanceCount)
	res, err := nodeDelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.0.1, 192.0.0.2, 192.0.0.3, 192.0.0.4
Chef-Server => 192.0.1.1, 192.0.1.2, 192.0.1.3, 192.0.1.4
OpenSearch => 192.0.2.1, 192.0.2.2, 192.0.2.3, 192.0.2.4
Postgresql => 192.0.3.1, 192.0.3.2, 192.0.3.3, 192.0.3.4

Nodes to be deleted:
================================================
Automate => 192.0.0.1
Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestDeletenodeDeployWithNewOSNodeInAws(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	var ipAddres, filewritten, executeCommands, autoFileMoved, tfArchModified bool
	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				filewritten = true
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
			taintTerraformFunc: func(path string) error {
				return nil
			},
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				ipAddres = true
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			executeShellCommandFunc: func() error {
				executeCommands = true
				return nil
			},
			moveAWSAutoTfvarsFileFunc: func(path string) error {
				autoFileMoved = true
				return nil
			},
			modifyTfArchFileFunc: func(path string) error {
				tfArchModified = true
				return nil
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.validate()
	assert.NoError(t, err)
	err = nodeDelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "4", nodeDelete.(*DeleteNodeAWSImpl).config.Automate.Config.InstanceCount)
	res, err := nodeDelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.0.1, 192.0.0.2, 192.0.0.3, 192.0.0.4
Chef-Server => 192.0.1.1, 192.0.1.2, 192.0.1.3, 192.0.1.4
OpenSearch => 192.0.2.1, 192.0.2.2, 192.0.2.3, 192.0.2.4
Postgresql => 192.0.3.1, 192.0.3.2, 192.0.3.3, 192.0.3.4

Nodes to be deleted:
================================================
Automate => 192.0.0.1
Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeDelete.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.True(t, ipAddres)
	assert.True(t, executeCommands)
}

func TestDeletenodeWithExecuteA2haRbNotExist(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	nodeDelete := NewDeleteNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		isA2HARBFileExistFunc: func() bool {
			return false
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeDelete.Execute(nil, nil)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `Invalid bastion, to run this command use automate bastion`)
}

func TestDeletenodeAWSExecuteWithError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	var ipAddres, filewritten, executeCommands, autoFileMoved, tfArchModified bool

	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				filewritten = true
				return errors.New("random")
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
			taintTerraformFunc: func(path string) error {
				return nil
			},
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				ipAddres = true
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			executeShellCommandFunc: func() error {
				executeCommands = true
				return nil
			},
			moveAWSAutoTfvarsFileFunc: func(path string) error {
				autoFileMoved = true
				return nil
			},
			modifyTfArchFileFunc: func(path string) error {
				tfArchModified = true
				return nil
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.Execute(nil, nil)
	assert.True(t, tfArchModified)
	assert.True(t, ipAddres)
	assert.True(t, filewritten)
	assert.True(t, executeCommands)
	assert.True(t, autoFileMoved)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestDeletenodeAWSExecuteNoError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	var ipAddres, filewritten, executeCommands, autoFileMoved, tfArchModified bool

	nodeDelete := NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		&MockNodeUtilsImpl{
			getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
				return nil, &SSHConfig{}, nil
			},
			executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
				return nil
			},
			writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
				filewritten = true
				return nil
			},
			getModeFromConfigFunc: func(path string) (string, error) {
				return EXISTING_INFRA_MODE, nil
			},
			isManagedServicesOnFunc: func() bool {
				return false
			},
			pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
			isA2HARBFileExistFunc: func() bool {
				return true
			},
			taintTerraformFunc: func(path string) error {
				return nil
			},
			getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
				ipAddres = true
				return &AWSConfigIp{
					configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
					configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
					configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
					configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
				}, nil
			},
			executeShellCommandFunc: func() error {
				executeCommands = true
				return nil
			},
			moveAWSAutoTfvarsFileFunc: func(path string) error {
				autoFileMoved = true
				return nil
			},
			modifyTfArchFileFunc: func(path string) error {
				tfArchModified = true
				return nil
			},
		},
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
	err := nodeDelete.Execute(nil, nil)
	assert.NoError(t, err)
	assert.True(t, tfArchModified)
	assert.True(t, ipAddres)
	assert.True(t, filewritten)
	assert.True(t, executeCommands)
	assert.True(t, autoFileMoved)
}
