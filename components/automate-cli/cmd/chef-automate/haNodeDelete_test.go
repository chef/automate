package main

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
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
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Unable to remove node. Automate instance count cannot be less than 1. Final count 0 not allowed.
Automate Ip 10.2.1.67 is not present in existing list of ip addresses. Please use a different private ip.
Automate Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.`)
}

func Test_deletenode_validate_error_multiple(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp:   "10.2.1.67,ewewedw",
		chefServerIp: "10.2.1.637,ewewedw",
		postgresqlIp: "10.2.1.657,ewewedw",
		opensearchIp: "10.2.1.61,ewewedw",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
		readConfigfunc: func(path string) (ExistingInfraConfigToml, error) {
			return readConfig(path)
		},
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Unable to remove node. Automate instance count cannot be less than 1. Final count 0 not allowed.
Automate Ip 10.2.1.67 is not present in existing list of ip addresses. Please use a different private ip.
Automate Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.
Unable to remove node. Chef Server instance count cannot be less than 1. Final count -1 not allowed.
Chef-Server Ip 10.2.1.637 is not present in existing list of ip addresses. Please use a different private ip.
Chef-Server Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.
Unable to remove node. OpenSearch instance count cannot be less than 3. Final count 2 not allowed.
OpenSearch Ip 10.2.1.61 is not present in existing list of ip addresses. Please use a different private ip.
OpenSearch Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.
Unable to remove node. Postgresql instance count cannot be less than 3. Final count 1 not allowed.
Postgresql Ip 10.2.1.657 is not present in existing list of ip addresses. Please use a different private ip.
Postgresql Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.`)
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
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{})
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
		getHaInfraDetailsfunc: func() (*SSHConfig, error) {
			return &SSHConfig{}, nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeImpl).automateIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 10.1.0.247, 10.1.0.248
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114, 10.1.2.115
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

Nodes to be deleted:
================================================
Automate => 10.1.0.247
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
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
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeImpl).opensearchIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 10.1.0.247, 10.1.0.248
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114, 10.1.2.115
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

Nodes to be deleted:
================================================
OpenSearch => 10.1.0.6
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func Test_deletenode_Deploy_with_newOS_mincount_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.1.0.6,10.1.2.114",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
			return nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			return errors.New("random")
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Unable to remove node. OpenSearch instance count cannot be less than 3. Final count 2 not allowed.`)
}

func Test_deletenode_Deploy_with_newOS_node_error(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "10.1.0.6",
	}
	nodedelete := NewDeleteNode(w.CliWriter, flags, &MockNodeUtilsImpl{
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
			return nil
		},
	}, configtomlpath, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			return errors.New("random")
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeImpl).opensearchIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 10.1.0.247, 10.1.0.248
Chef-Server => 10.1.0.80
OpenSearch => 10.1.0.6, 10.1.1.253, 10.1.2.114, 10.1.2.115
Postgresql => 10.1.0.134, 10.1.1.196, 10.1.2.163

Nodes to be deleted:
================================================
OpenSearch => 10.1.0.6
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}
