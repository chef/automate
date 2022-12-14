package main

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func PullConfFunc(sshUtil *SSHUtil, ex []string) (*ExistingInfraConfigToml, error) {
	cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
	if err != nil {
		return nil, err
	}
	return &cfg, nil
}

func TestDeleteNodeValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,ewewedw",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Unable to remove node. Automate instance count cannot be less than 1. Final count 0 not allowed.
Automate Ip 10.2.1.67 is not present in existing list of ip addresses. Please use a different private ip.
Automate Ip ewewedw is not present in existing list of ip addresses. Please use a different private ip.`)
}

func TestDeleteNodeValidateErrorMultiple(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp:   "10.2.1.67,ewewedw",
		chefServerIp: "10.2.1.637,ewewedw",
		postgresqlIp: "10.2.1.657,ewewedw",
		opensearchIp: "10.2.1.61,ewewedw",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
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

func TestDeleteNodeModifyAutomate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.2.0",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeOnPremImpl).automateIpList[0])
	assert.Equal(t, "1", nodedelete.(*DeleteNodeOnPremImpl).config.Automate.Config.InstanceCount)
	assert.Equal(t, 1, len(nodedelete.(*DeleteNodeOnPremImpl).config.Automate.Config.CertsByIP))
	assert.Equal(t, 1, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.AutomatePrivateIps))
}

func TestRemovenodeValidateTypeAwsOrSelfManaged(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: TEST_IP_1,
	}
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			cfg.ExternalDB.Database.Type = TYPE_AWS
			return &cfg, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "remove"))
}

func TestRemovenodeValidateTypeAwsOrSelfManaged2(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
		automateIp:   TEST_IP_2,
	}
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			cfg.ExternalDB.Database.Type = TYPE_AWS
			return &cfg, nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodeAdd.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "remove"))
}

func TestDeleteNodeModifyInfra(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		chefServerIp: "192.0.2.2",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unable to remove node. Chef Server instance count cannot be less than 1. Final count 0 not allowed.")
	// even though validation will fail still we check if modify config is working as expected or not
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.chefServerIp, nodedelete.(*DeleteNodeOnPremImpl).chefServerIpList[0])
	assert.Equal(t, "0", nodedelete.(*DeleteNodeOnPremImpl).config.ChefServer.Config.InstanceCount)
	assert.Equal(t, 0, len(nodedelete.(*DeleteNodeOnPremImpl).config.ChefServer.Config.CertsByIP))
	assert.Equal(t, 0, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.ChefServerPrivateIps))
}

func TestDeletenodeModifyOpensearch(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).opensearchIpList[0])
	assert.Equal(t, "3", nodedelete.(*DeleteNodeOnPremImpl).config.Opensearch.Config.InstanceCount)
	assert.Equal(t, 3, len(nodedelete.(*DeleteNodeOnPremImpl).config.Opensearch.Config.CertsByIP))
	assert.Equal(t, 3, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.OpensearchPrivateIps))
}

func TestDeletenodeModifyPostgresql(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: "192.0.2.9",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unable to remove node. Postgresql instance count cannot be less than 3. Final count 2 not allowed.")
	// even though validation will fail still we check if modify config is working as expected or not
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.postgresqlIp, nodedelete.(*DeleteNodeOnPremImpl).postgresqlIp[0])
	assert.Equal(t, "2", nodedelete.(*DeleteNodeOnPremImpl).config.Postgresql.Config.InstanceCount)
	assert.Equal(t, 2, len(nodedelete.(*DeleteNodeOnPremImpl).config.Postgresql.Config.CertsByIP))
	assert.Equal(t, 2, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.PostgresqlPrivateIps))
}

func TestDeleteNodePrompt(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.2.0",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeOnPremImpl).automateIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Nodes to be deleted:
================================================
Automate => 192.0.2.0
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestDeleteNodeDeployWithNewOSNode(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	var filewritten, deployed bool
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigfunc: func(path string) error {
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).opensearchIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Nodes to be deleted:
================================================
OpenSearch => 192.0.2.3
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestDeleteNodeDeployWithNewOSMinCountError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3,192.0.2.5",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		genConfigfunc: func(path string) error {
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			return errors.New("random")
		},
	}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `IP address validation failed: 
Unable to remove node. OpenSearch instance count cannot be less than 3. Final count 2 not allowed.`)
}

func TestDeleteNodeDeployWithNewOSNodeError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		genConfigfunc: func(path string) error {
			return nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{
		WriteToFileFunc: func(filepath string, data []byte) error {
			return errors.New("random")
		},
	}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).opensearchIpList[0])
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Nodes to be deleted:
================================================
OpenSearch => 192.0.2.3
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestRemovenodeExecuteWithNewOSNodeNoCertsByIP(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	var filewritten, deployed bool
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigfunc: func(path string) error {
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
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Nodes to be deleted:
================================================
OpenSearch => 192.0.2.6
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestRemovenodeExecuteWithNewOSNode(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	var filewritten, deployed bool
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = true
			return nil
		},
		genConfigfunc: func(path string) error {
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
		pullAndUpdateConfigFunc: PullConfFunc,
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
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Nodes to be deleted:
================================================
OpenSearch => 192.0.2.6
This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}
