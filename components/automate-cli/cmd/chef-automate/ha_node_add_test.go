package main

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"golang.org/x/crypto/ssh"
)

const CONFIG_TOML_PATH = "../../pkg/testfiles/config.toml"
const TEST_IP_1 = "192.0.2.11"

type MockSSHUtilsImpl struct {
	getSSHConfigFunc                                func() *SSHConfig
	setSSHConfigFunc                                func(sshConfig *SSHConfig)
	getClientConfigFunc                             func() (*ssh.ClientConfig, error)
	getConnectionFunc                               func() (*ssh.Client, error)
	connectAndExecuteCommandOnRemoteFunc            func(remoteCommands string, spinner bool) (string, error)
	connectAndExecuteCommandOnRemoteSteamOutputFunc func(remoteCommands string) (string, error)
	copyFileToRemoteFunc                            func(srcFilePath string, destFileName string, removeFile bool) error
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

func TestAddnodeValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,ewewedw",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
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
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
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

func TestMddnodeReadfileError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return ExistingInfraConfigToml{}, errors.New("random")
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
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

func TestAddnodeModifyAutomate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeImpl).automateIpList[0])
	assert.Equal(t, "3", nodeAdd.(*AddNodeImpl).config.Automate.Config.InstanceCount)
	assert.Equal(t, 3, len(nodeAdd.(*AddNodeImpl).config.Automate.Config.CertsByIP))
	assert.Equal(t, 3, len(nodeAdd.(*AddNodeImpl).config.ExistingInfra.Config.AutomatePrivateIps))
}

func TestAddnodeModifyInfra(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		chefServerIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.chefServerIp, nodeAdd.(*AddNodeImpl).chefServerIpList[0])
	assert.Equal(t, "2", nodeAdd.(*AddNodeImpl).config.ChefServer.Config.InstanceCount)
	assert.Equal(t, 2, len(nodeAdd.(*AddNodeImpl).config.ChefServer.Config.CertsByIP))
	assert.Equal(t, 2, len(nodeAdd.(*AddNodeImpl).config.ExistingInfra.Config.ChefServerPrivateIps))
}

func TestAddnodeModifyPostgresql(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.postgresqlIp, nodeAdd.(*AddNodeImpl).postgresqlIp[0])
	assert.Equal(t, "4", nodeAdd.(*AddNodeImpl).config.Postgresql.Config.InstanceCount)
	assert.Equal(t, 4, len(nodeAdd.(*AddNodeImpl).config.Postgresql.Config.CertsByIP))
	assert.Equal(t, 4, len(nodeAdd.(*AddNodeImpl).config.ExistingInfra.Config.PostgresqlPrivateIps))
}

func TestAddnodeModifyOpensearch(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeImpl).opensearchIpList[0])
	assert.Equal(t, "5", nodeAdd.(*AddNodeImpl).config.Opensearch.Config.InstanceCount)
	assert.Equal(t, 5, len(nodeAdd.(*AddNodeImpl).config.Opensearch.Config.CertsByIP))
	assert.Equal(t, 5, len(nodeAdd.(*AddNodeImpl).config.ExistingInfra.Config.OpensearchPrivateIps))
}

func TestAddnodePrompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: TEST_IP_1,
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeImpl).automateIpList[0])
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
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigfunc: func(path string) error {
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
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeImpl).opensearchIpList[0])
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
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		genConfigfunc: func(path string) error {
			return errors.New("random")
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
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
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeImpl).opensearchIpList[0])
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
