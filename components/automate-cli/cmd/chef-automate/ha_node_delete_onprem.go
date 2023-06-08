package main

import (
	"container/list"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	AUTOMATE_MIN_INSTANCE_COUNT    = 1
	CHEF_SERVER_MIN_INSTANCE_COUNT = 1
	POSTGRESQL_MIN_INSTANCE_COUNT  = 3
	OPENSEARCH_MIN_INSTANCE_COUNT  = 3
)

type DeleteNodeOnPremImpl struct {
	config                  ExistingInfraConfigToml
	copyConfigForUserPrompt ExistingInfraConfigToml
	ipToDelete              string
	nodeType                string
	nodeUtils               NodeOpUtils
	flags                   AddDeleteNodeHACmdFlags
	configpath              string
	terraformPath           string
	writer                  *cli.Writer
	fileUtils               fileutils.FileUtils
	sshUtil                 SSHUtil
}

func NewDeleteNodeOnPrem(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileutils fileutils.FileUtils, sshUtil SSHUtil) HAModifyAndDeploy {
	return &DeleteNodeOnPremImpl{
		flags:         flags,
		writer:        writer,
		nodeUtils:     nodeUtils,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		fileUtils:     fileutils,
		sshUtil:       sshUtil,
	}
}

func (dni *DeleteNodeOnPremImpl) Execute(c *cobra.Command, args []string) error {
	if !dni.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	if dni.flags.automateIp == "" &&
		dni.flags.chefServerIp == "" &&
		dni.flags.opensearchIp == "" &&
		dni.flags.postgresqlIp == "" {
		c.Help()
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
	}
	err := dni.validate()
	if err != nil {
		return err
	}
	err = dni.modifyConfig()
	if err != nil {
		return err
	}
	if !dni.flags.autoAccept {
		res, err := dni.promptUserConfirmation()
		if err != nil {
			return err
		}
		if !res {
			return nil
		}
	}

	previousCount, err := dni.nodeUtils.calculateTotalInstanceCount()
	if err != nil {
		return err
	}

	err = dni.prepare()
	if err != nil {
		return err
	}

	err = dni.runDeploy()
	currentCount, newErr := dni.nodeUtils.calculateTotalInstanceCount()
	if newErr != nil {
		if err != nil {
			return errors.Wrap(err, newErr.Error())
		}
		return newErr
	}

	if currentCount == previousCount-1 {
		stopErr := dni.stopNodes()
		if stopErr != nil {
			if err != nil {
				return errors.Wrap(err, stopErr.Error())
			}
			return stopErr
		}
	} else {
		if err != nil {
			return errors.Wrap(err, "Error in deleting node")
		}
		return errors.New("Error in deleting node")
	}

	if err == nil {
		dni.writer.Println("Node is successfully removed from the cluster. Please delete the machine manually.")
	}

	return err
}

func (dni *DeleteNodeOnPremImpl) prepare() error {
	return dni.nodeUtils.taintTerraform(dni.terraformPath)
}

func (dni *DeleteNodeOnPremImpl) validate() error {
	automateIpList, chefServerIpList, opensearchIpList, postgresqlIpList := splitIPCSV(
		dni.flags.automateIp,
		dni.flags.chefServerIp,
		dni.flags.opensearchIp,
		dni.flags.postgresqlIp,
	)

	// Check if only one node is being deleted
	if (len(automateIpList) + len(chefServerIpList) + len(opensearchIpList) + len(postgresqlIpList)) != 1 {
		return status.New(status.InvalidCommandArgsError, "Only one node can be deleted at a time")
	}

	// Get the node type and ip address of the node to be deleted
	if len(automateIpList) > 0 {
		dni.ipToDelete = automateIpList[0]
		dni.nodeType = AUTOMATE
	} else if len(chefServerIpList) > 0 {
		dni.ipToDelete = chefServerIpList[0]
		dni.nodeType = CHEF_SERVER
	} else if len(postgresqlIpList) > 0 {
		dni.ipToDelete = postgresqlIpList[0]
		dni.nodeType = POSTGRESQL
	} else if len(opensearchIpList) > 0 {
		dni.ipToDelete = opensearchIpList[0]
		dni.nodeType = OPENSEARCH
	} else {
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
	}

	if !isValidIPFormat(dni.ipToDelete) {
		return status.New(status.InvalidCommandArgsError, "Invalid IP address "+dni.ipToDelete)
	}

	updatedConfig, err := dni.nodeUtils.pullAndUpdateConfig(&dni.sshUtil, []string{dni.ipToDelete})
	if err != nil {
		return err
	}
	dni.config = *updatedConfig
	dni.copyConfigForUserPrompt = dni.config
	if dni.nodeUtils.isManagedServicesOn() {
		if dni.nodeType == POSTGRESQL || dni.nodeType == OPENSEARCH {
			return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "remove"))
		}
	}
	errorList := dni.validateCmdArgs()
	if errorList != nil && errorList.Len() > 0 {
		return getSingleErrorFromList(errorList)
	}
	return nil
}

func (dni *DeleteNodeOnPremImpl) modifyConfig() error {
	var err error

	switch dni.nodeType {
	case AUTOMATE:
		err = modifyConfigForDeleteNode(
			&dni.config.Automate.Config.InstanceCount,
			&dni.config.ExistingInfra.Config.AutomatePrivateIps,
			[]string{dni.ipToDelete},
			&dni.config.Automate.Config.CertsByIP,
		)
	case CHEF_SERVER:
		err = modifyConfigForDeleteNode(
			&dni.config.ChefServer.Config.InstanceCount,
			&dni.config.ExistingInfra.Config.ChefServerPrivateIps,
			[]string{dni.ipToDelete},
			&dni.config.ChefServer.Config.CertsByIP,
		)
	case POSTGRESQL:
		err = modifyConfigForDeleteNode(
			&dni.config.Postgresql.Config.InstanceCount,
			&dni.config.ExistingInfra.Config.PostgresqlPrivateIps,
			[]string{dni.ipToDelete},
			&dni.config.Postgresql.Config.CertsByIP,
		)
	case OPENSEARCH:
		err = modifyConfigForDeleteNode(
			&dni.config.Opensearch.Config.InstanceCount,
			&dni.config.ExistingInfra.Config.OpensearchPrivateIps,
			[]string{dni.ipToDelete},
			&dni.config.Opensearch.Config.CertsByIP,
		)
	default:
		return errors.New("Invalid node type")
	}

	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying "+stringutils.TitleReplace(dni.nodeType, "_", "-")+" instance count")
	}

	return nil
}

func (dni *DeleteNodeOnPremImpl) promptUserConfirmation() (bool, error) {
	dni.writer.Println("Existing nodes:")
	dni.writer.Println("================================================")
	dni.writer.Println("Automate => " + strings.Join(dni.copyConfigForUserPrompt.ExistingInfra.Config.AutomatePrivateIps, ", "))
	dni.writer.Println("Chef-Server => " + strings.Join(dni.copyConfigForUserPrompt.ExistingInfra.Config.ChefServerPrivateIps, ", "))
	dni.writer.Println("OpenSearch => " + strings.Join(dni.copyConfigForUserPrompt.ExistingInfra.Config.OpensearchPrivateIps, ", "))
	dni.writer.Println("Postgresql => " + strings.Join(dni.copyConfigForUserPrompt.ExistingInfra.Config.PostgresqlPrivateIps, ", "))
	dni.writer.Println("")
	dni.writer.Println("Node to be deleted:")
	dni.writer.Println("================================================")

	dni.writer.Println(fmt.Sprintf("%s => %s", stringutils.TitleReplace(dni.nodeType, "_", "-"), dni.ipToDelete))

	dni.writer.Println("Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.")
	return dni.writer.Confirm("This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue?")
}

func (dni *DeleteNodeOnPremImpl) saveConfigToBastion() error {
	nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
	return dni.nodeUtils.ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR)
}

func (dni *DeleteNodeOnPremImpl) syncConfigToAllNodes() error {
	nodeObjects := getNodeObjectsToPatchWorkspaceConfigToAllNodes()
	return dni.nodeUtils.ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR)
}

func (dni *DeleteNodeOnPremImpl) runDeploy() error {
	err := dni.saveConfigToBastion()
	if err != nil {
		return errors.Wrap(err, "error saving node configuration to bastion")
	}

	err = dni.nodeUtils.writeHAConfigFiles(existingNodesA2harbTemplate, dni.config)
	if err != nil {
		return errors.Wrap(err, "error writing HA config.toml in workspace directory")
	}
	argsdeploy := []string{"-y"}
	err = dni.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	if err != nil {
		err = errors.Wrap(err, "error while deploying architecture")
	}

	syncErr := dni.syncConfigToAllNodes()
	if syncErr != nil {
		err = errors.Wrapf(syncErr, "error syncing config to all nodes. \n%v", err)
	}

	if err != nil {
		return err
	}
	return nil
}

func (dni *DeleteNodeOnPremImpl) validateCmdArgs() *list.List {
	errorList := list.New()

	var minCount int
	var instanceCount string
	var existingIplist []string

	switch dni.nodeType {
	case AUTOMATE:
		instanceCount = dni.config.Automate.Config.InstanceCount
		minCount = AUTOMATE_MIN_INSTANCE_COUNT
		existingIplist = dni.config.ExistingInfra.Config.AutomatePrivateIps
	case CHEF_SERVER:
		instanceCount = dni.config.ChefServer.Config.InstanceCount
		minCount = CHEF_SERVER_MIN_INSTANCE_COUNT
		existingIplist = dni.config.ExistingInfra.Config.ChefServerPrivateIps
	case POSTGRESQL:
		instanceCount = dni.config.Postgresql.Config.InstanceCount
		minCount = POSTGRESQL_MIN_INSTANCE_COUNT
		existingIplist = dni.config.ExistingInfra.Config.PostgresqlPrivateIps
	case OPENSEARCH:
		instanceCount = dni.config.Opensearch.Config.InstanceCount
		minCount = OPENSEARCH_MIN_INSTANCE_COUNT
		existingIplist = dni.config.ExistingInfra.Config.OpensearchPrivateIps
	default:
		errorList.PushBack("Invalid node type")
	}

	allowed, finalCount, err := isFinalInstanceCountAllowed(instanceCount, -1, minCount)
	if err != nil {
		errorList.PushBack("Error occurred in calculating " + stringutils.TitleReplace(dni.nodeType, "_", "-") + " final instance count")
	}
	if !allowed {
		errorList.PushBack(fmt.Sprintf("Unable to remove node. %s instance count cannot be less than %d. Final count %d not allowed.", stringutils.TitleReplace(dni.nodeType, "_", "-"), minCount, finalCount))
		return errorList
	}
	errorList.PushBackList(checkIfPresentInPrivateIPList(existingIplist, []string{dni.ipToDelete}, stringutils.TitleReplace(dni.nodeType, "_", "-")))

	return errorList
}

// Stop all services on the node to be deleted
func (dni *DeleteNodeOnPremImpl) stopNodes() error {

	infra, _, err := dni.nodeUtils.getHaInfraDetails()
	if err != nil {
		return err
	}

	err = dni.nodeUtils.stopServicesOnNode(dni.ipToDelete, dni.nodeType, EXISTING_INFRA_MODE, infra)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "Error stopping services on node")
	}
	return nil
}
