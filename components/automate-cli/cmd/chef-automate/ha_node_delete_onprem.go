package main

import (
	"container/list"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	AUTOMATE_MIN_INSTANCE_COUNT    = 1
	CHEF_SERVER_MIN_INSTANCE_COUNT = 1
	POSTGRESQL_MIN_INSTANCE_COUNT  = 3
	OPENSEARCH_MIN_INSTANCE_COUNT  = 3
)

type SvcDetailOnPrem struct {
	ipList         []string
	configKey      string
	minCount       int
	instanceCount  string
	existingIplist []string
}

type DeleteNodeOnPremImpl struct {
	config                  ExistingInfraConfigToml
	copyConfigForUserPrompt ExistingInfraConfigToml
	automateIpList          []string
	chefServerIpList        []string
	opensearchIpList        []string
	postgresqlIpList        []string
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
	dni.prepare()
	return dni.runDeploy()
}

func (dni *DeleteNodeOnPremImpl) prepare() error {
	return dni.nodeUtils.taintTerraform(dni.terraformPath)
}

func (dni *DeleteNodeOnPremImpl) validate() error {
	dni.automateIpList, dni.chefServerIpList, dni.opensearchIpList, dni.postgresqlIpList = splitIPCSV(
		dni.flags.automateIp,
		dni.flags.chefServerIp,
		dni.flags.opensearchIp,
		dni.flags.postgresqlIp,
	)
	var exceptionIps []string
	exceptionIps = append(exceptionIps, dni.automateIpList...)
	exceptionIps = append(exceptionIps, dni.chefServerIpList...)
	exceptionIps = append(exceptionIps, dni.opensearchIpList...)
	exceptionIps = append(exceptionIps, dni.postgresqlIpList...)
	updatedConfig, err := dni.nodeUtils.pullAndUpdateConfig(&dni.sshUtil, exceptionIps)
	if err != nil {
		return err
	}
	dni.config = *updatedConfig
	dni.copyConfigForUserPrompt = dni.config
	if dni.nodeUtils.isManagedServicesOn() {
		if len(dni.opensearchIpList) > 0 || len(dni.postgresqlIpList) > 0 {
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
	err := modifyConfigForDeleteNode(
		&dni.config.Automate.Config.InstanceCount,
		&dni.config.ExistingInfra.Config.AutomatePrivateIps,
		dni.automateIpList,
		&dni.config.Automate.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying automate instance count")
	}
	err = modifyConfigForDeleteNode(
		&dni.config.ChefServer.Config.InstanceCount,
		&dni.config.ExistingInfra.Config.ChefServerPrivateIps,
		dni.chefServerIpList,
		&dni.config.ChefServer.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying chef-server instance count")
	}
	err = modifyConfigForDeleteNode(
		&dni.config.Opensearch.Config.InstanceCount,
		&dni.config.ExistingInfra.Config.OpensearchPrivateIps,
		dni.opensearchIpList,
		&dni.config.Opensearch.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying opensearch instance count")
	}
	err = modifyConfigForDeleteNode(
		&dni.config.Postgresql.Config.InstanceCount,
		&dni.config.ExistingInfra.Config.PostgresqlPrivateIps,
		dni.postgresqlIpList,
		&dni.config.Postgresql.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying postgresql instance count")
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
	dni.writer.Println("Nodes to be deleted:")
	dni.writer.Println("================================================")
	if len(dni.automateIpList) > 0 {
		dni.writer.Println("Automate => " + strings.Join(dni.automateIpList, ", "))
	}
	if len(dni.chefServerIpList) > 0 {
		dni.writer.Println("Chef-Server => " + strings.Join(dni.chefServerIpList, ", "))
	}
	if len(dni.opensearchIpList) > 0 {
		dni.writer.Println("OpenSearch => " + strings.Join(dni.opensearchIpList, ", "))
	}
	if len(dni.postgresqlIpList) > 0 {
		dni.writer.Println("Postgresql => " + strings.Join(dni.postgresqlIpList, ", "))
	}
	dni.writer.Println("Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.")
	return dni.writer.Confirm("This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue?")
}

func (dni *DeleteNodeOnPremImpl) runDeploy() error {
	err := dni.nodeUtils.writeHAConfigFiles(existingNodesA2harbTemplate, dni.config)
	if err != nil {
		return err
	}
	argsdeploy := []string{"-y"}
	return dni.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
}

func (dni *DeleteNodeOnPremImpl) validateCmdArgs() *list.List {
	errorList := list.New()
	var validations = []SvcDetailOnPrem{
		{dni.automateIpList, "Automate", AUTOMATE_MIN_INSTANCE_COUNT, dni.config.Automate.Config.InstanceCount, dni.config.ExistingInfra.Config.AutomatePrivateIps},
		{dni.chefServerIpList, "Chef-Server", CHEF_SERVER_MIN_INSTANCE_COUNT, dni.config.ChefServer.Config.InstanceCount, dni.config.ExistingInfra.Config.ChefServerPrivateIps},
	}
	if !dni.nodeUtils.isManagedServicesOn() {
		validations = append(validations, []SvcDetailOnPrem{
			{dni.opensearchIpList, "OpenSearch", OPENSEARCH_MIN_INSTANCE_COUNT, dni.config.Opensearch.Config.InstanceCount, dni.config.ExistingInfra.Config.OpensearchPrivateIps},
			{dni.postgresqlIpList, "Postgresql", POSTGRESQL_MIN_INSTANCE_COUNT, dni.config.Postgresql.Config.InstanceCount, dni.config.ExistingInfra.Config.PostgresqlPrivateIps},
		}...)
	}
	for _, v := range validations {
		if len(v.ipList) == 0 {
			continue
		}
		allowed, finalCount, err := isFinalInstanceCountAllowed(v.instanceCount, -len(v.ipList), v.minCount)
		if err != nil {
			errorList.PushBack("Error occurred in calculating " + v.configKey + " final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. %s instance count cannot be less than %d. Final count %d not allowed.", v.configKey, v.minCount, finalCount))
			continue
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(v.existingIplist, v.ipList, v.configKey))
	}
	return errorList
}
