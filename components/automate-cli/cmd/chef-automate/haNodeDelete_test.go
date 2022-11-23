package main

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func Test_deletenode_validate_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,ewewedw",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "IP address validation failed: \nAutomate Ip 10.2.1.67 is not present in existing list of ip addresses. Please use a different private ip.\nAutomate Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.")
}

func Test_deletenode_Modify(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.1.0.247",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeImpl).automateIpList[0])
}

func Test_deletenode_Prompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.1.0.247",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeImpl).automateIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), "Existing nodes:\n================================================\nAutomate => 10.1.0.247\nChef-Server => 10.1.0.80\nOpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114\nPostgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163\n\nNodes to be deleted:\n================================================\nAutomate => 10.1.0.247\nThis will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)\n")
}

func Test_deletenode_Deploy_with_newOS_node(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.1.0.6",
	}
	var filewritten, deployed bool
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeImpl).opensearchIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), "Existing nodes:\n================================================\nAutomate => 10.1.0.247\nChef-Server => 10.1.0.80\nOpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114\nPostgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163\n\nNodes to be deleted:\n================================================\nOpenSearch => 10.1.0.6\nThis will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)\n")
	err = nodedelete.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}
