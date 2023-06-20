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
	automateIpNotPresent = `IP address validation failed: 
Automate Ip 193.0.0.1 is not present in existing list of ip addresses. Please use a different private ip.`
	multipleIpAddressError = `IP address validation failed: 
Only one node can be deleted at a time`
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
		automateIp: "193.0.0.1",
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
	assert.Equal(t, automateIpNotPresent, err.Error())
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
	assert.Contains(t, err.Error(), multipleNodeError)
}

func TestDeletenodeAWSModifyA2(t *testing.T) {
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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

func TestDeletenodeAWSModifyCS(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		chefServerIp: "192.0.1.2",
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
	assert.Equal(t, "1", nodeDelete.(*DeleteNodeAWSImpl).config.ChefServer.Config.InstanceCount)
}

func TestDeletenodeAWSModifyPG(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: "192.0.3.3",
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
	assert.Equal(t, "3", nodeDelete.(*DeleteNodeAWSImpl).config.Postgresql.Config.InstanceCount)
}

func TestDeletenodeAWSModifyOS(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.4",
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
	assert.Equal(t, "5", nodeDelete.(*DeleteNodeAWSImpl).config.Opensearch.Config.InstanceCount)
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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

Node to be deleted:
================================================
Automate => 192.0.0.1
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
			executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
				return nil
			},
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return nil
			},
			syncConfigToAllNodesFunc: func() error {
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

Node to be deleted:
================================================
Automate => 192.0.0.1
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
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
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
			stopServicesOnNodeFunc: func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
				return nil
			},
			calculateTotalInstanceCountFunc: func() (int, error) {
				return 0, nil
			},
			executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
				return nil
			},
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return nil
			},
			saveConfigToBastionFunc: func() error {
				return nil
			},
			syncConfigToAllNodesFunc: func() error {
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
	count := 10
	var ipAddres, filewritten, executeCommands, autoFileMoved, tfArchModified bool
	mnu := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
		stopServicesOnNodeFunc: func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		calculateTotalInstanceCountFunc: func() (int, error) {
			count = count - 1
			return count, nil
		},
		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
	}
	flagsArr := []AddDeleteNodeHACmdFlags{
		{
			automateIp: "192.0.0.1",
		},
		{
			chefServerIp: "192.0.1.1",
		},
		{
			postgresqlIp: "192.0.3.1",
		},
		{
			opensearchIp: "192.0.2.1",
		},
	}

	for _, flags := range flagsArr {
		w := majorupgrade_utils.NewCustomWriterWithInputs("y")
		nodeDelete := NewDeleteNodeAWS(
			w.CliWriter,
			flags,
			mnu,
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

}

func TestDeletenodeDeploy(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mockNodeUtil := newMockNodeUtilsImplForDeleteAWS()

	t.Run("With save config error", func(t *testing.T) {

		mockNodeUtil.saveConfigToBastionFunc = func() error {
			return errors.New("error removing header from output file")
		}
		nodeDelete := createNewDeleteNodeAWS(mockNodeUtil, nil, w)

		err := nodeDelete.Execute(nil, nil)
		assert.Error(t, err, "error removing header from output file")
	})

	t.Run("With sync config error", func(t *testing.T) {

		mockNodeUtil.syncConfigToAllNodesFunc = func() error {
			return errors.New("sync error")
		}
		nodeDelete := createNewDeleteNodeAWS(mockNodeUtil, nil, w)

		err := nodeDelete.runDeploy()
		assert.Error(t, err, "sync error")
	})
	t.Run("With sync config error and deploy error", func(t *testing.T) {

		mockNodeUtil.syncConfigToAllNodesFunc = func() error {
			return errors.New("sync error")
		}
		mockNodeUtil.executeAutomateClusterCtlCommandAsyncfunc = func(command string, args []string, helpDocs string) error {
			if command == "deploy" {
				return errors.New("deploy error")
			}
			return nil
		}
		nodeDelete := createNewDeleteNodeAWS(mockNodeUtil, nil, w)

		err := nodeDelete.runDeploy()
		assert.Error(t, err, "sync error")
		assert.Error(t, err, "deploy error")
	})
}

func newMockNodeUtilsImplForDeleteAWS() *MockNodeUtilsImpl {
	return &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
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
		taintTerraformFunc: func(path string) error {
			return nil
		},
		getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
			return &AWSConfigIp{
				configAutomateIpList:   []string{"192.0.0.1", "192.0.0.2", "192.0.0.3", "192.0.0.4"},
				configChefServerIpList: []string{"192.0.1.1", "192.0.1.2", "192.0.1.3", "192.0.1.4"},
				configOpensearchIpList: []string{"192.0.2.1", "192.0.2.2", "192.0.2.3", "192.0.2.4"},
				configPostgresqlIpList: []string{"192.0.3.1", "192.0.3.2", "192.0.3.3", "192.0.3.4"},
			}, nil
		},
		executeShellCommandFunc: func() error {
			return nil
		},
		moveAWSAutoTfvarsFileFunc: func(path string) error {
			return nil
		},
		modifyTfArchFileFunc: func(path string) error {
			return nil
		},
		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
		calculateTotalInstanceCountFunc: func() (int, error) {
			return 0, nil
		},
	}
}

func createNewDeleteNodeAWS(mockNodeUtilsImpl *MockNodeUtilsImpl, mockSSHUtilsImpl *MockSSHUtilsImpl, w *majorupgrade_utils.CustomWriter) HAModifyAndDeploy {

	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.0.1",
	}
	return NewDeleteNodeAWS(
		w.CliWriter,
		flags,
		mockNodeUtilsImpl,
		CONFIG_TOML_PATH_AWS,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
}
