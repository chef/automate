package main

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const configtomlpath = "./testfiles/config.toml"

func Test_validate_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,ewewedw",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (string, string, string, error) {
			return "", "", "", nil
		},
		connectAndExecuteCommandOnRemotefunc: func(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
			return "", nil
		},
	}, configtomlpath)
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "IP address validation failed: \nIncorrect Automate IP address format for ip ewewedw")
}

func Test_Modify(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (string, string, string, error) {
			return "", "", "", nil
		},
		connectAndExecuteCommandOnRemotefunc: func(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
			return "", nil
		},
	}, configtomlpath)
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeImpl).automateIpList[0])
	t.Log(nodeAdd.(*AddNodeImpl).automateIpList[0])
}

func Test_Prompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67",
	}
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (string, string, string, error) {
			return "", "", "", nil
		},
		connectAndExecuteCommandOnRemotefunc: func(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
			return "", nil
		},
	}, configtomlpath)
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodeAdd.(*AddNodeImpl).automateIpList[0])
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), "Existing nodes:\n================================================\nAutomate => 10.1.0.247\nChef-Server => 10.1.0.80\nOpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114\nPostgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163\n\nNew nodes to be added:\n================================================\nAutomate => 10.2.1.67\nThis will add the new nodes to your existing setup. The process may thae a while. Are you sure you want to continue? (y/n)\n")
}

func Test_Deploy_with_newOS_node(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.2.1.67",
	}
	var filewritten, deployed bool
	nodeAdd := NewAddNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (string, string, string, error) {
			return "", "", "", nil
		},
		connectAndExecuteCommandOnRemotefunc: func(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
			return "", nil
		},
		writeFilefunc: func(tomlbytes []byte, filepath string) error {
			filewritten = true
			return nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigfunc: func(path string) error {
			return nil
		},
	}, configtomlpath)
	err := nodeAdd.validate()
	assert.NoError(t, err)
	err = nodeAdd.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodeAdd.(*AddNodeImpl).opensearchIpList[0])
	res, err := nodeAdd.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), "Existing nodes:\n================================================\nAutomate => 10.1.0.247\nChef-Server => 10.1.0.80\nOpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114\nPostgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163\n\nNew nodes to be added:\n================================================\nOpenSearch => 10.2.1.67\nThis will add the new nodes to your existing setup. The process may thae a while. Are you sure you want to continue? (y/n)\n")
	err = nodeAdd.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}
