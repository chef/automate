package main

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"golang.org/x/crypto/ssh"
)

const CONFIG_TOML_PATH = "../../pkg/testfiles/onprem"
const TEST_IP_1 = "192.0.2.11"

type MockSSHUtilsImpl struct {
	getSSHConfigFunc                                func() *SSHConfig
	setSSHConfigFunc                                func(sshConfig *SSHConfig)
	getClientConfigFunc                             func() (*ssh.ClientConfig, error)
	getConnectionFunc                               func() (*ssh.Client, error)
	connectAndExecuteCommandOnRemoteFunc            func(remoteCommands string, spinner bool) (string, error)
	connectAndExecuteCommandOnRemoteSteamOutputFunc func(remoteCommands string) (string, error)
	copyFileToRemoteFunc                            func(srcFilePath string, destFileName string, removeFile bool) error
	copyFileFromRemoteFunc                          func(remoteFilePath string, outputFileName string) (string, error)
}

func (msu *MockSSHUtilsImpl) getSSHConfig() *SSHConfig {
	return msu.getSSHConfigFunc()
}
func (msu *MockSSHUtilsImpl) setSSHConfig(sshConfig *SSHConfig) {
	return
}
func (msu *MockSSHUtilsImpl) getClientConfig() (*ssh.ClientConfig, error) {
	return msu.getClientConfigFunc()
}
func (msu *MockSSHUtilsImpl) getConnection() (*ssh.Client, error) {
	return msu.getConnectionFunc()
}
func (msu *MockSSHUtilsImpl) connectAndExecuteCommandOnRemote(remoteCommands string, spinner bool) (string, error) {
	return msu.connectAndExecuteCommandOnRemoteFunc(remoteCommands, spinner)
}
func (msu *MockSSHUtilsImpl) connectAndExecuteCommandOnRemoteSteamOutput(remoteCommands string) (string, error) {
	return msu.connectAndExecuteCommandOnRemoteSteamOutputFunc(remoteCommands)
}
func (msu *MockSSHUtilsImpl) copyFileToRemote(srcFilePath string, destFileName string, removeFile bool) error {
	return msu.copyFileToRemoteFunc(srcFilePath, destFileName, removeFile)
}
func (msu *MockSSHUtilsImpl) copyFileFromRemote(remoteFilePath string, outputFileName string) (string, error) {
	return msu.copyFileFromRemoteFunc(remoteFilePath, outputFileName)
}

func TestAddnodeValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,ewewedw",
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "IP address validation failed: \nIncorrect Automate IP address format for ip ewewedw")
}

func TestAddnodeValidateErrorMultiple(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp:   "10.2.1.67,ewewedw",
		chefServerIp: "10.2.1.637,ewewedw",
		postgresqlIp: "10.2.1.657,ewewedw",
		opensearchIp: "10.2.1.61,ewewedw",
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Incorrect Automate IP address format for ip ewewedw
Incorrect Chef-Server IP address format for ip 10.2.1.637
Incorrect Chef-Server IP address format for ip ewewedw
Incorrect OpenSearch IP address format for ip ewewedw
Incorrect Postgresql IP address format for ip 10.2.1.657
Incorrect Postgresql IP address format for ip ewewedw`)
}

func TestAddnodeReadfileError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			return nil, errors.New("random")
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestAddnodeValidateTypeAwsOrSelfManaged(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return true
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			return &cfg, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "add"))
}

func TestAddnodeValidateTypeAwsOrSelfManaged2(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
		automateIp:   TEST_IP_2,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return true
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			return &cfg, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "add"))
}

func TestAddnodeModifyAutomate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeOnPremImpl).automateIpList[0])
	assert.Equal(t, "3", nodeAdd.(*AddNodeOnPremImpl).config.Automate.Config.InstanceCount)
	assert.Equal(t, 3, len(nodeAdd.(*AddNodeOnPremImpl).config.Automate.Config.CertsByIP))
	assert.Equal(t, 3, len(nodeAdd.(*AddNodeOnPremImpl).config.ExistingInfra.Config.AutomatePrivateIps))
}

func TestAddnodeModifyInfra(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		chefServerIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.chefServerIp, nodeAdd.(*AddNodeOnPremImpl).chefServerIpList[0])
	assert.Equal(t, "2", nodeAdd.(*AddNodeOnPremImpl).config.ChefServer.Config.InstanceCount)
	assert.Equal(t, 2, len(nodeAdd.(*AddNodeOnPremImpl).config.ChefServer.Config.CertsByIP))
	assert.Equal(t, 2, len(nodeAdd.(*AddNodeOnPremImpl).config.ExistingInfra.Config.ChefServerPrivateIps))
}

func TestAddnodeModifyPostgresql(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.postgresqlIp, nodeAdd.(*AddNodeOnPremImpl).postgresqlIp[0])
	assert.Equal(t, "4", nodeAdd.(*AddNodeOnPremImpl).config.Postgresql.Config.InstanceCount)
	assert.Equal(t, 4, len(nodeAdd.(*AddNodeOnPremImpl).config.Postgresql.Config.CertsByIP))
	assert.Equal(t, 4, len(nodeAdd.(*AddNodeOnPremImpl).config.ExistingInfra.Config.PostgresqlPrivateIps))
}

func TestAddnodeModifyOpensearch(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeOnPremImpl).opensearchIpList[0])
	assert.Equal(t, "5", nodeAdd.(*AddNodeOnPremImpl).config.Opensearch.Config.InstanceCount)
	assert.Equal(t, 5, len(nodeAdd.(*AddNodeOnPremImpl).config.Opensearch.Config.CertsByIP))
	assert.Equal(t, 5, len(nodeAdd.(*AddNodeOnPremImpl).config.ExistingInfra.Config.OpensearchPrivateIps))
}

func TestAddnodePrompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeOnPremImpl).automateIpList[0])
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

New nodes to be added:
================================================
Automate => 192.0.2.11
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestAddnodeDeployWithNewOSNode(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	var filewritten, deployed bool
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
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
		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeOnPremImpl).opensearchIpList[0])
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

New nodes to be added:
================================================
OpenSearch => 192.0.2.11
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeDeployWithNewOSNodeGenconfigError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
			return errors.New("random")
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeOnPremImpl).opensearchIpList[0])
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

New nodes to be added:
================================================
OpenSearch => 192.0.2.11
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestAddnodeExecuteWithNewOSNodeNoCertByIP(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	var filewritten, deployed bool
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
			filewritten = true
			return nil
		},
		isA2HARBFileExistFunc: func() bool {
			return true
		},
		checkIfFileExistFunc: func(path string) bool {
			return checkIfFileExist(path)
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		taintTerraformFunc: func(path string) error {
			return nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			cfg.Automate.Config.CertsByIP = []CertByIP{}
			cfg.ChefServer.Config.CertsByIP = []CertByIP{}
			cfg.Postgresql.Config.CertsByIP = []CertByIP{}
			cfg.Opensearch.Config.CertsByIP = []CertByIP{}
			return &cfg, nil
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
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

New nodes to be added:
================================================
OpenSearch => 192.0.2.11
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeExecuteWithNewOSNode(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	var filewritten, deployed bool
	nodeAdd := NewAddNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
			filewritten = true
			return nil
		},
		isA2HARBFileExistFunc: func() bool {
			return true
		},
		checkIfFileExistFunc: func(path string) bool {
			return checkIfFileExist(path)
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		taintTerraformFunc: func(path string) error {
			return nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
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
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.Execute(nil, nil)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

New nodes to be added:
================================================
OpenSearch => 192.0.2.11
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestAddnodeExecuteSyncConfigToAllNodes(t *testing.T) {

	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mockNodeUtil := newMockNodeUtilsImplForAddOnprem()

	t.Run("With sync config error", func(t *testing.T) {

		mockNodeUtil.syncConfigToAllNodesFunc = func() error {
			return errors.New("sync error")
		}
		nodeAdd := createNewAddNodeOnprem(mockNodeUtil, nil, w)

		err := nodeAdd.runDeploy()
		assert.Error(t, err, "sync error")
	})
	t.Run("With sync config error and deploy error", func(t *testing.T) {

		mockNodeUtil.syncConfigToAllNodesFunc = func() error {
			return errors.New("sync error")
		}
		mockNodeUtil.executeAutomateClusterCtlCommandAsyncfunc = func(command string, args []string, helpDocs string) error {
			return errors.New("deploy error")
		}
		nodeAdd := createNewAddNodeOnprem(mockNodeUtil, nil, w)

		err := nodeAdd.runDeploy()
		assert.Error(t, err, "sync error")
	})
}

func TestAddnodeExecuteSaveConfigFromAllNodeType(t *testing.T) {

	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mockNodeUtil := newMockNodeUtilsImplForAddOnprem()

	t.Run("With save config error", func(t *testing.T) {

		mockNodeUtil.saveConfigToBastionFunc = func() error {
			return errors.New("error fetching config")
		}
		mockNodeUtil.pullAndUpdateConfigFunc = PullConfFunc
		nodeAdd := createNewAddNodeOnprem(mockNodeUtil, nil, w)

		err := nodeAdd.Execute(nil, nil)
		assert.Error(t, err, "error fetching config")
	})

	t.Run("no error", func(t *testing.T) {

		mockNodeUtil.saveConfigToBastionFunc = func() error {
			return nil
		}
		mockNodeUtil.pullAndUpdateConfigFunc = PullConfFunc
		nodeAdd := createNewAddNodeOnprem(mockNodeUtil, nil, w)

		err := nodeAdd.Execute(nil, nil)
		assert.NoError(t, err)
	})
}

func newMockNodeUtilsImplForAddOnprem() *MockNodeUtilsImpl {
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

func createNewAddNodeOnprem(mockNodeUtilsImpl *MockNodeUtilsImpl, mockSSHUtilsImpl *MockSSHUtilsImpl, w *majorupgrade_utils.CustomWriter) HAModifyAndDeploy {

	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	return NewAddNodeOnPrem(
		w.CliWriter,
		flags,
		mockNodeUtilsImpl,
		CONFIG_TOML_PATH,
		&fileutils.MockFileSystemUtils{},
		&MockSSHUtilsImpl{
			connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
				return "", nil
			},
		})
}
