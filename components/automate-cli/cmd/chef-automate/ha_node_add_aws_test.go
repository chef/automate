package main

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const CONFIG_TOML_PATH_AWS = "../../pkg/testfiles/aws"

func TestAddnodeAWSValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{automateCount: 0}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Either one of automate-count or chef-server-count or opensearch-count or postgresql-count must be more than 0.")
}

func TestAddnodeAWSValidate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	assert.Equal(t, "6", nodeAdd.(*AddNodeAWSImpl).config.Opensearch.Config.InstanceCount)
}

func TestAddnodeAWSValidateErrorIsManagedServicesOn(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return true
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "add"))
}

func TestAddnodeModifyAws(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		chefServerCount: 1,
	}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
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
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
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
Chef-Server => 2
OpenSearch => 6
Postgresql => 4

New node count:
================================================
Automate => 6
Chef-Server => 2
OpenSearch => 6
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestAddnodeDeployWithNewOSNodeInAws(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, deployed, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = true
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
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
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
		getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
			return &AWSConfigIp{
				configAutomateIpList: []string{"127.0.0.1","127.0.1.1","127.0.2.1"},
				configChefServerIpList: []string{"127.0.0.2","127.0.1.2","127.0.2.2"},
				configOpensearchIpList: []string{"127.0.0.3","127.0.1.3","127.0.2.3"},
				configPostgresqlIpList: []string{"127.0.0.4","127.0.1.4","127.0.2.4"},
			}, nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, "7", nodeAdd.(*AddNodeAWSImpl).config.Opensearch.Config.InstanceCount)
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing node count:
================================================
Automate => 5
Chef-Server => 2
OpenSearch => 6
Postgresql => 4

New node count:
================================================
Automate => 5
Chef-Server => 2
OpenSearch => 7
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeWithExecuteA2haRbNotExist(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		isA2HARBFileExistFunc: func() bool {
			return false
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `Invalid bastion, to run this command use automate bastion`)
}

func TestAddnodeWithExecuteFuncGenConfigErr(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = true
			return errors.New("random")
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
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.Contains(t, w.Output(), `Existing node count:
================================================
Automate => 5
Chef-Server => 2
OpenSearch => 6
Postgresql => 4

New node count:
================================================
Automate => 5
Chef-Server => 2
OpenSearch => 7
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestAddnodeWithSaveConfigToBasionErr(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, deployed, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = false
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = false
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
		moveAWSAutoTfvarsFileFunc: func(path string) error {
			autoFileMoved = false
			return nil
		},
		modifyTfArchFileFunc: func(path string) error {
			tfArchModified = false
			return nil
		},
		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		saveConfigToBastionFunc: func() error {
			return errors.New("error removing header")
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.Error(t, err)
	assert.ErrorContains(t, err, "error removing header")
	assert.Equal(t, false, autoFileMoved)
	assert.Equal(t, false, tfArchModified)
	assert.Equal(t, false, filewritten)
	assert.Equal(t, false, deployed)
}

func TestAddnodeWithSyncConfigToAllNodesErr(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{automateCount: 1}
	var filewritten, deployed, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = true
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
			return true
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
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
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return errors.New("random")
		},
		getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
			return &AWSConfigIp{
				configAutomateIpList: []string{"127.0.0.1","127.0.1.1","127.0.2.1"},
				configChefServerIpList: []string{"127.0.0.2","127.0.1.2","127.0.2.2"},
				configOpensearchIpList: []string{"127.0.0.3","127.0.1.3","127.0.2.3"},
				configPostgresqlIpList: []string{"127.0.0.4","127.0.1.4","127.0.2.4"},
			}, nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})

	err := nodeAdd.Execute(nil, nil)
	assert.Error(t, err)
	assert.ErrorContains(t, err, "random")
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeWithSyncConfigToAllNodesErrAndDeployError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{automateCount: 1}
	var filewritten, deployed, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			if command == "deploy" {
				deployed = false
				return errors.New("Deployment failed")
			}
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = true
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
			return true
		},
		pullAndUpdateConfigAwsFunc: PullAwsConfFunc,
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
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return errors.New("random")
		},
		getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
			return &AWSConfigIp{
				configAutomateIpList: []string{"127.0.0.1","127.0.1.1","127.0.2.1"},
				configChefServerIpList: []string{"127.0.0.2","127.0.1.2","127.0.2.2"},
				configOpensearchIpList: []string{"127.0.0.3","127.0.1.3","127.0.2.3"},
				configPostgresqlIpList: []string{"127.0.0.4","127.0.1.4","127.0.2.4"},
			}, nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})

	err := nodeAdd.Execute(nil, nil)
	assert.Error(t, err)
	assert.ErrorContains(t, err, "random")
	assert.ErrorContains(t, err, "Deployment failed")
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, false, deployed)
}

func TestAddnodeWithExecuteFunc(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{opensearchCount: 1}
	var filewritten, deployed, autoFileMoved, tfArchModified bool
	nodeAdd := NewAddNodeAWS(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}, state string) error {
			filewritten = true
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
		executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
		saveConfigToBastionFunc: func() error {
			return nil
		},
		getAWSConfigIpFunc: func() (*AWSConfigIp, error) {
			return &AWSConfigIp{
				configAutomateIpList: []string{"127.0.0.1","127.0.1.1","127.0.2.1"},
				configChefServerIpList: []string{"127.0.0.2","127.0.1.2","127.0.2.2"},
				configOpensearchIpList: []string{"127.0.0.3","127.0.1.3","127.0.2.3"},
				configPostgresqlIpList: []string{"127.0.0.4","127.0.1.4","127.0.2.4"},
			}, nil
		},
	}, CONFIG_TOML_PATH_AWS, &fileutils.MockFileSystemUtils{
		MoveFunc: func(sourceFile, destinationFile string) error {
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
Chef-Server => 2
OpenSearch => 6
Postgresql => 4

New node count:
================================================
Automate => 5
Chef-Server => 2
OpenSearch => 7
Postgresql => 4
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, autoFileMoved)
	assert.Equal(t, true, tfArchModified)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}
