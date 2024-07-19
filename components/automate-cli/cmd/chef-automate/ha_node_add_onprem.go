package main

import (
	"container/list"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	cli "github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/spf13/cobra"
)

const (
	TYPE_ERROR = `Cannot %s OpenSearch or Postgresql nodes if external.database.type is either aws or self-managed.
Please set external.database.type to empty if you want to add OpenSearch or Postgresql nodes`
	TYPE_AWS          = "aws"
	TYPE_SELF_MANAGED = "self-managed"
)

type AddDeleteNodeHACmdFlags struct {
	automateIp            string
	chefServerIp          string
	opensearchIp          string
	postgresqlIp          string
	automateCount         int
	chefServerCount       int
	opensearchCount       int
	postgresqlCount       int
	onPremMode            bool
	awsMode               bool
	autoAccept            bool
	removeUnreachableNode bool
}

type AddNodeOnPremImpl struct {
	config                  ExistingInfraConfigToml
	copyConfigForUserPrompt ExistingInfraConfigToml
	automateIpList          []string
	chefServerIpList        []string
	opensearchIpList        []string
	postgresqlIp            []string
	nodeUtils               NodeOpUtils
	flags                   AddDeleteNodeHACmdFlags
	configpath              string
	terraformPath           string
	writer                  *cli.Writer
	fileutils               fileutils.FileUtils
	sshUtil                 SSHUtil
	unreachableIpMap        map[string][]string
	statusSummary           StatusSummary
}

func NewAddNodeOnPrem(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileUtils fileutils.FileUtils, sshUtil SSHUtil, statusSummary StatusSummary) HAModifyAndDeploy {
	return &AddNodeOnPremImpl{
		flags:         flags,
		writer:        writer,
		nodeUtils:     nodeUtils,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		fileutils:     fileUtils,
		sshUtil:       sshUtil,
		statusSummary: statusSummary,
	}
}

func (ani *AddNodeOnPremImpl) Execute(c *cobra.Command, args []string) error {
	if !ani.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	if ani.flags.automateIp == "" &&
		ani.flags.chefServerIp == "" &&
		ani.flags.opensearchIp == "" &&
		ani.flags.postgresqlIp == "" {
		c.Help()
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to add")
	}
	err := ani.validate()
	if err != nil {
		return err
	}
	err = ani.modifyConfig()
	if err != nil {
		return err
	}

	err = ani.nodeUtils.saveConfigToBastion()
	if err != nil {
		return err
	}

	if !ani.flags.autoAccept {
		res, err := ani.promptUserConfirmation()
		if err != nil {
			return err
		}
		if !res {
			return nil
		}
	}
	ani.prepare()
	return ani.runDeploy()
}

func (ani *AddNodeOnPremImpl) prepare() error {
	return ani.nodeUtils.taintTerraform(ani.terraformPath)
}

func (ani *AddNodeOnPremImpl) validate() error {
	updatedConfig, unreachableNodes, err := ani.nodeUtils.pullAndUpdateConfig(&ani.sshUtil, []string{}, ani.flags.removeUnreachableNode)
	if err != nil {
		return err
	}
	ani.config = *updatedConfig
	ani.copyConfigForUserPrompt = ani.config
	ani.automateIpList, ani.chefServerIpList, ani.opensearchIpList, ani.postgresqlIp = splitIPCSV(
		ani.flags.automateIp,
		ani.flags.chefServerIp,
		ani.flags.opensearchIp,
		ani.flags.postgresqlIp,
	)
	if ani.nodeUtils.isManagedServicesOn() {
		if len(ani.opensearchIpList) > 0 || len(ani.postgresqlIp) > 0 {
			return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "add"))
		}
	}
	errorList := ani.validateCmdArgs()
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errorList), status.ConfigError, "IP address validation failed")
	}
	ani.unreachableIpMap = unreachableNodes
	return nil
}

func (ani *AddNodeOnPremImpl) modifyConfig() error {
	err := modifyConfigForAddNewNode(
		&ani.config.Automate.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.AutomatePrivateIps,
		ani.automateIpList,
		&ani.config.Automate.Config.CertsByIP,
		ani.unreachableIpMap[AUTOMATE],
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying automate instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.ChefServer.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.ChefServerPrivateIps,
		ani.chefServerIpList,
		&ani.config.ChefServer.Config.CertsByIP,
		ani.unreachableIpMap[CHEF_SERVER],
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying chef-server instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.Opensearch.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.OpensearchPrivateIps,
		ani.opensearchIpList,
		&ani.config.Opensearch.Config.CertsByIP,
		ani.unreachableIpMap[OPENSEARCH],
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying opensearch instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.Postgresql.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.PostgresqlPrivateIps,
		ani.postgresqlIp,
		&ani.config.Postgresql.Config.CertsByIP,
		ani.unreachableIpMap[POSTGRESQL],
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying postgresql instance count")
	}
	return nil
}

func (ani *AddNodeOnPremImpl) promptUserConfirmation() (bool, error) {
	ani.writer.Println("Existing nodes:")
	ani.writer.Println("================================================")
	ani.writer.Println("Automate => " + strings.Join(ani.copyConfigForUserPrompt.ExistingInfra.Config.AutomatePrivateIps, ", "))
	ani.writer.Println("Chef-Server => " + strings.Join(ani.copyConfigForUserPrompt.ExistingInfra.Config.ChefServerPrivateIps, ", "))
	ani.writer.Println("OpenSearch => " + strings.Join(ani.copyConfigForUserPrompt.ExistingInfra.Config.OpensearchPrivateIps, ", "))
	ani.writer.Println("Postgresql => " + strings.Join(ani.copyConfigForUserPrompt.ExistingInfra.Config.PostgresqlPrivateIps, ", "))
	ani.writer.Println("")
	ani.writer.Println("New nodes to be added:")
	ani.writer.Println("================================================")
	if len(ani.automateIpList) > 0 {
		ani.writer.Println("Automate => " + strings.Join(ani.automateIpList, ", "))
		if ani.unreachableIpMap != nil && len(ani.unreachableIpMap[AUTOMATE]) > 0 {
			ani.writer.Println("Unreachable Automate nodes will be removed => " + strings.Join(ani.unreachableIpMap[AUTOMATE], ", "))
		}
	}
	if len(ani.chefServerIpList) > 0 {
		ani.writer.Println("Chef-Server => " + strings.Join(ani.chefServerIpList, ", "))
		if ani.unreachableIpMap != nil && len(ani.unreachableIpMap[CHEF_SERVER]) > 0 {
			ani.writer.Println("Unreachable Chef-Server nodes will be removed => " + strings.Join(ani.unreachableIpMap[CHEF_SERVER], ", "))
		}
	}
	if len(ani.opensearchIpList) > 0 {
		ani.writer.Println("OpenSearch => " + strings.Join(ani.opensearchIpList, ", "))
		if ani.unreachableIpMap != nil && len(ani.unreachableIpMap[OPENSEARCH]) > 0 {
			ani.writer.Println("Unreachable Opensearch nodes will be removed => " + strings.Join(ani.unreachableIpMap[OPENSEARCH], ", "))
		}
	}
	if len(ani.postgresqlIp) > 0 {
		ani.writer.Println("Postgresql => " + strings.Join(ani.postgresqlIp, ", "))
		if ani.unreachableIpMap != nil && len(ani.unreachableIpMap[POSTGRESQL]) > 0 {
			ani.writer.Println("Unreachable Postgresql nodes will be removed => " + strings.Join(ani.unreachableIpMap[POSTGRESQL], ", "))
		}
	}
	return ani.writer.Confirm("This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue?")
}

func (ani *AddNodeOnPremImpl) runDeploy() error {
	err := ani.nodeUtils.writeHAConfigFiles(existingNodesA2harbTemplate, ani.config, DEPLOY)
	if err != nil {
		return err
	}
	argsdeploy := []string{"-y"}
	err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	syncErr := ani.nodeUtils.syncConfigToAllNodes(ani.unreachableIpMap)
	if syncErr != nil {
		if err != nil {
			return errors.Wrap(err, syncErr.Error())
		}
		return syncErr
	}

	// Restart all PostgreSQL nodes in order to apply the new configuration
	if len(ani.postgresqlIp) > 0 {
		leader := getPGLeader(ani.statusSummary)
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		err = ani.nodeUtils.restartPgNodes(*leader, infra.Outputs.PostgresqlPrivateIps.Value, infra, ani.statusSummary)
		if err != nil {
			return err
		}
	}

	return err
}

func (ani *AddNodeOnPremImpl) validateCmdArgs() *list.List {
	errorList := list.New()
	if len(ani.automateIpList) > 0 {
		errorList.PushBackList(ani.validateIPAddresses(ani.automateIpList, "Automate"))
	}
	if len(ani.chefServerIpList) > 0 {
		errorList.PushBackList(ani.validateIPAddresses(ani.chefServerIpList, "Chef-Server"))
	}
	if len(ani.opensearchIpList) > 0 {
		errorList.PushBackList(ani.validateIPAddresses(ani.opensearchIpList, "OpenSearch"))
	}
	if len(ani.postgresqlIp) > 0 {
		errorList.PushBackList(ani.validateIPAddresses(ani.postgresqlIp, "Postgresql"))
	}
	return errorList
}

func (ani *AddNodeOnPremImpl) validateIPAddresses(ips []string, errorPrefix string) *list.List {
	errorList := list.New()
	prefixAdder := ""
	if errorPrefix != "" {
		prefixAdder = " "
	}
	for _, ip := range ips {
		if stringutils.SliceContains(ani.config.ExistingInfra.Config.AutomatePrivateIps, ip) ||
			stringutils.SliceContains(ani.config.ExistingInfra.Config.ChefServerPrivateIps, ip) ||
			stringutils.SliceContains(ani.config.ExistingInfra.Config.OpensearchPrivateIps, ip) ||
			stringutils.SliceContains(ani.config.ExistingInfra.Config.PostgresqlPrivateIps, ip) {
			errorList.PushBack(fmt.Sprintf("%s%sIp %s is already configured for a node. Please use a different private ip.", errorPrefix, prefixAdder, ip))
		}
		err := checkIPAddress(ip)
		if err != nil {
			errorList.PushBack(fmt.Sprintf("Incorrect %s IP address format for ip %s", errorPrefix, ip))
		}
		err = ani.validateConnection(ip)
		if err != nil {
			errorList.PushBack(fmt.Sprintf("%s IP address %s is unreachable", errorPrefix, ip))
		}
	}
	return errorList
}

func (ani *AddNodeOnPremImpl) validateConnection(ip string) error {
	_, sshConfig, err := ani.nodeUtils.getHaInfraDetails()
	if err != nil {
		return err
	}
	sshConfig.hostIP = ip
	ani.sshUtil.setSSHConfig(sshConfig)
	_, err = ani.sshUtil.connectAndExecuteCommandOnRemote("sudo echo 1", true)
	if err != nil {
		return err
	}
	return nil
}
