package main

import (
	"container/list"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"errors"

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
	automateIp      string
	chefServerIp    string
	opensearchIp    string
	postgresqlIp    string
	automateCount   int
	chefServerCount int
	opensearchCount int
	postgresqlCount int
	onPremMode      bool
	awsMode         bool
	autoAccept      bool
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
}

func NewAddNodeOnPrem(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileUtils fileutils.FileUtils, sshUtil SSHUtil) HAModifyAndDeploy {
	return &AddNodeOnPremImpl{
		flags:         flags,
		writer:        writer,
		nodeUtils:     nodeUtils,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		fileutils:     fileUtils,
		sshUtil:       sshUtil,
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
	if !ani.flags.autoAccept {
		res, err := ani.promptUserConfirmation()
		if err != nil {
			return err
		}
		if !res {
			return nil
		}
	}
	// ani.prepare()
	return ani.runDeploy()
}

func (ani *AddNodeOnPremImpl) prepare() error {
	return ani.nodeUtils.taintTerraform(ani.terraformPath)
}

func (ani *AddNodeOnPremImpl) validate() error {
	updatedConfig, err := ani.nodeUtils.pullAndUpdateConfig(&ani.sshUtil, []string{})
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
	return nil
}

func (ani *AddNodeOnPremImpl) modifyConfig() error {
	err := modifyConfigForAddNewNode(
		&ani.config.Automate.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.AutomatePrivateIps,
		ani.automateIpList,
		&ani.config.Automate.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying automate instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.ChefServer.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.ChefServerPrivateIps,
		ani.chefServerIpList,
		&ani.config.ChefServer.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying chef-server instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.Opensearch.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.OpensearchPrivateIps,
		ani.opensearchIpList,
		&ani.config.Opensearch.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying opensearch instance count")
	}
	err = modifyConfigForAddNewNode(
		&ani.config.Postgresql.Config.InstanceCount,
		&ani.config.ExistingInfra.Config.PostgresqlPrivateIps,
		ani.postgresqlIp,
		&ani.config.Postgresql.Config.CertsByIP,
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
	}
	if len(ani.chefServerIpList) > 0 {
		ani.writer.Println("Chef-Server => " + strings.Join(ani.chefServerIpList, ", "))
	}
	if len(ani.opensearchIpList) > 0 {
		ani.writer.Println("OpenSearch => " + strings.Join(ani.opensearchIpList, ", "))
	}
	if len(ani.postgresqlIp) > 0 {
		ani.writer.Println("Postgresql => " + strings.Join(ani.postgresqlIp, ", "))
	}
	return ani.writer.Confirm("This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue?")
}

func (ani *AddNodeOnPremImpl) runDeploy() error {
	// err := SaveConfigInBastion()
	// if err != nil {
	// 	return err
	// }
	// // return nil
	// err = ani.nodeUtils.writeHAConfigFiles(existingNodesA2harbTemplate, ani.config)
	// if err != nil {
	// 	return err
	// }
	// argsdeploy := []string{"-y"}
	// err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	// if err != nil {
	// 	return err
	// }
	err := syncConfigToAllNodes()
	if err != nil {
		return err
	}
	return nil
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

type NodeObject struct {
	CmdString  string
	OutputFile []string
	InputFile  []string
	NodeType   string
}

func NewNodeObjectWithOutputFile(cmdString string, outFile []string, inputFile []string, nodeType string) *NodeObject {
	return &NodeObject{cmdString, outFile, inputFile, nodeType}
}

// Save all config from each services to Bastion server annd move it to WORKSPACE dir
func SaveConfigInBastion() error {

	nodeObjects := []*NodeObject{
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_FRONTEND_CONFIG, AUTOMATE_TOML), []string{AUTOMATE_TOML}, nil, CONST_AUTOMATE),
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_FRONTEND_CONFIG, CHEF_SERVER_TOML), []string{CHEF_SERVER_TOML}, nil, CONST_CHEF_SERVER),
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_BACKEND_CONFIG, CONST_POSTGRESQL, " > "+POSTGRESQL_TOML), []string{POSTGRESQL_TOML}, nil, CONST_POSTGRESQL),
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_BACKEND_CONFIG, CONST_OPENSEARCH, " > "+OPENSEARCH_TOML), []string{OPENSEARCH_TOML}, nil, CONST_OPENSEARCH),
	}
	return ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR)
}

func syncConfigToAllNodes() error {
	timestamp := time.Now().Format("20060102150405")
	fmt.Println("====================================================================")
	fmt.Println("syncConfigToAllNodes")
	frontend := fmt.Sprintf(FRONTEND_COMMAND, PATCH, "frontend"+"_"+timestamp+"_"+AUTOMATE_TOML, DATE_FORMAT)
	chefserver := fmt.Sprintf(FRONTEND_COMMAND, PATCH, "frontend"+"_"+timestamp+"_"+CHEF_SERVER_TOML, DATE_FORMAT)
	postgresql := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, "postgresql", "%s", "postgresql"+"_"+timestamp+"_"+POSTGRESQL_TOML)
	opensearch := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, "opensearch", "%s", "opensearch"+"_"+timestamp+"_"+OPENSEARCH_TOML)
	fmt.Println("frontend: ", frontend)
	fmt.Println("chefserver: ", chefserver)
	fmt.Println("postgresql: ", postgresql)
	fmt.Println("opensearch: ", opensearch)
	nodeObjects := []*NodeObject{
		NewNodeObjectWithOutputFile(frontend, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + AUTOMATE_TOML}, CONST_AUTOMATE),
		NewNodeObjectWithOutputFile(chefserver, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + CHEF_SERVER_TOML}, CONST_CHEF_SERVER),
		NewNodeObjectWithOutputFile(postgresql, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + POSTGRESQL_TOML}, CONST_POSTGRESQL),
		NewNodeObjectWithOutputFile(opensearch, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + OPENSEARCH_TOML}, CONST_OPENSEARCH),
	}
	return ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, false, "")
}
