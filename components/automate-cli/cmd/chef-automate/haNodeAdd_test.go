package main

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"golang.org/x/crypto/ssh"
)

const configtomlpath = "./testfiles/config.toml"

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

func Test_addnode_validate_error(t *testing.T) {
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
	}, configtomlpath, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "IP address validation failed: \nIncorrect Automate IP address format for ip ewewedw")
}

func Test_addnode_validate_error_multiple(t *testing.T) {
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
	}, configtomlpath, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
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

func Test_addnode_readfile_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return ExistingInfraConfigToml{}, errors.New("random")
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func Test_addnode_Modify(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeImpl).automateIpList[0])
}

func Test_addnode_Prompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
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
Automate => 10.1.0.247
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

New nodes to be added:
================================================
Automate => 10.2.1.67
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func Test_addnode_Deploy_with_newOS_node(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.2.1.67",
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
	}, configtomlpath, &fileutils.MockFileSystemUtils{
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
Automate => 10.1.0.247
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

New nodes to be added:
================================================
OpenSearch => 10.2.1.67
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func Test_addnode_Deploy_with_newOS_node_genconfig_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.2.1.67",
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
	}, configtomlpath, &fileutils.MockFileSystemUtils{
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
Automate => 10.1.0.247
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

New nodes to be added:
================================================
OpenSearch => 10.2.1.67
This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodeAdd.runDeploy()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}
