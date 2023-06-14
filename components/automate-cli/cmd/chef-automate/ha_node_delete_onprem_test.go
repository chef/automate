package main

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	multipleNodeError = `Only one node can be deleted at a time`
)

func PullConfFunc(sshUtil *SSHUtil, ex []string) (*ExistingInfraConfigToml, error) {
	cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
	if err != nil {
		return nil, err
	}
	return &cfg, nil
}

func PullAwsConfFunc(sshUtil *SSHUtil, ex []string) (*AwsConfigToml, error) {
	cfg, err := readConfigAWS(CONFIG_TOML_PATH_AWS + "/config.toml")
	if err != nil {
		return nil, err
	}
	return &cfg, nil
}

func TestDeleteNodeValidateError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.2.2",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), `
Automate Ip 192.0.2.2 is not present in existing list of ip addresses. Please use a different private ip.`)
}

func TestDeleteNodeValidateErrorMultiple(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "10.2.1.67,10.2.1.637",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(),
		multipleNodeError)
}

func TestDeleteNodeModifyAutomate(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		automateIp: "192.0.2.0",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	assert.Equal(t, "1", nodedelete.(*DeleteNodeOnPremImpl).config.Automate.Config.InstanceCount)
	assert.Equal(t, 1, len(nodedelete.(*DeleteNodeOnPremImpl).config.Automate.Config.CertsByIP))
	assert.Equal(t, 1, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.AutomatePrivateIps))
}

func TestRemovenodeValidateTypeAwsOrSelfManaged(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: TEST_IP_1,
	}
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	assert.Contains(t, err.Error(), fmt.Sprintf(TYPE_ERROR, "remove"))
}

func TestRemovenodeValidateTypeAwsOrSelfManaged2(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: TEST_IP_1,
		automateIp:   TEST_IP_2,
	}
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	assert.Contains(t, err.Error(), multipleNodeError)
}

func TestDeleteNodeModifyInfra(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		chefServerIp: "192.0.2.2",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unable to remove node. Chef-Server instance count cannot be less than 1. Final count 0 not allowed.")
	// even though validation will fail still we check if modify config is working as expected or not
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.chefServerIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	assert.Equal(t, "0", nodedelete.(*DeleteNodeOnPremImpl).config.ChefServer.Config.InstanceCount)
	assert.Equal(t, 0, len(nodedelete.(*DeleteNodeOnPremImpl).config.ChefServer.Config.CertsByIP))
	assert.Equal(t, 0, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.ChefServerPrivateIps))
}

func TestDeletenodeModifyOpensearch(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	assert.Equal(t, "3", nodedelete.(*DeleteNodeOnPremImpl).config.Opensearch.Config.InstanceCount)
	assert.Equal(t, 3, len(nodedelete.(*DeleteNodeOnPremImpl).config.Opensearch.Config.CertsByIP))
	assert.Equal(t, 3, len(nodedelete.(*DeleteNodeOnPremImpl).config.ExistingInfra.Config.OpensearchPrivateIps))
}

func TestDeletenodeModifyPostgresql(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		postgresqlIp: "192.0.2.9",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unable to remove node. Postgresql instance count cannot be less than 3. Final count 2 not allowed.")
	// even though validation will fail still we check if modify config is working as expected or not
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.postgresqlIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
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
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.automateIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Node to be deleted:
================================================
Automate => 192.0.2.0
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
}

func TestDeleteNodeDeployWithNewOSNode(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	var filewritten, deployed bool
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		pullAndUpdateConfigFunc: PullConfFunc,
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Node to be deleted:
================================================
Opensearch => 192.0.2.3
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.NoError(t, err)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestDeleteNodeDeployWithSaveConfigToBastionError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return errors.New("error on removing output header in fetched config")
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Node to be deleted:
================================================
Opensearch => 192.0.2.3
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.Error(t, err, "error on removing output header in fetched config")
}

func TestDeleteNodeDeployWithError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	var filewritten, deployed bool
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		}, executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			deployed = false
			return errors.New("Invalid or empty command")
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
			filewritten = true
			return nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		pullAndUpdateConfigFunc: PullConfFunc,
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Node to be deleted:
================================================
Opensearch => 192.0.2.3
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.Error(t, err, "Invalid or empty command")
	assert.Equal(t, true, filewritten)
	assert.Equal(t, false, deployed)
}

func TestDeleteNodeDeployWithNewOSMinCountError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriter()
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3,192.0.2.5",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		pullAndUpdateConfigFunc: PullConfFunc,
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.Error(t, err)
	assert.Contains(t, err.Error(),
		multipleNodeError)
}

func TestDeleteNodeDeployWithNewOSNodeError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.3",
	}
	nodedelete := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		pullAndUpdateConfigFunc: PullConfFunc,
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
	}, CONFIG_TOML_PATH, &fileutils.MockFileSystemUtils{}, &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	})
	err := nodedelete.validate()
	assert.NoError(t, err)
	err = nodedelete.modifyConfig()
	assert.NoError(t, err)
	assert.Equal(t, flags.opensearchIp, nodedelete.(*DeleteNodeOnPremImpl).ipToDelete)
	res, err := nodedelete.promptUserConfirmation()
	assert.Equal(t, true, res)
	assert.NoError(t, err)
	assert.Contains(t, w.Output(), `Existing nodes:
================================================
Automate => 192.0.2.0, 192.0.2.1
Chef-Server => 192.0.2.2
OpenSearch => 192.0.2.3, 192.0.2.4, 192.0.2.5, 192.0.2.6
Postgresql => 192.0.2.7, 192.0.2.8, 192.0.2.9

Node to be deleted:
================================================
Opensearch => 192.0.2.3
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	err = nodedelete.runDeploy()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "random")
}

func TestRemovenodeExecuteWithNewOSNodeNoCertsByIP(t *testing.T) {
	count := 10
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	var filewritten, deployed bool
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		stopServicesOnNodeFunc: func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		calculateTotalInstanceCountFunc: func() (int, error) {
			count = count - 1
			return count, nil
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
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
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

Node to be deleted:
================================================
Opensearch => 192.0.2.6
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}

func TestRemovenodeExecuteWithNewOSNode(t *testing.T) {
	count := 10
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	flags := AddDeleteNodeHACmdFlags{
		opensearchIp: "192.0.2.6",
	}
	var filewritten, deployed bool
	nodeAdd := NewDeleteNodeOnPrem(w.CliWriter, flags, &MockNodeUtilsImpl{
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
		stopServicesOnNodeFunc: func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		calculateTotalInstanceCountFunc: func() (int, error) {
			count = count - 1
			return count, nil
		},
		pullAndUpdateConfigFunc: PullConfFunc,
		executeCmdInAllNodeAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
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

Node to be deleted:
================================================
Opensearch => 192.0.2.6
Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.
This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue? (y/n)`)
	assert.Equal(t, true, filewritten)
	assert.Equal(t, true, deployed)
}
