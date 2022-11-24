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
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	AUTOMATE_MIN_INSTANCE_COUNT    = 1
	CHEF_SERVER_MIN_INSTANCE_COUNT = 1
	POSTGRESQL_MIN_INSTANCE_COUNT  = 3
	OPENSEARCH_MIN_INSTANCE_COUNT  = 3
)

func runDeleteNodeHACmd(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) func(c *cobra.Command, args []string) error {
	return func(c *cobra.Command, args []string) error {
		if !isA2HARBFileExist() {
			return errors.New(AUTOMATE_HA_INVALID_BASTION)
		}
		if addDeleteNodeHACmdFlags.automateIp == "" &&
			addDeleteNodeHACmdFlags.chefServerIp == "" &&
			addDeleteNodeHACmdFlags.opensearchIp == "" &&
			addDeleteNodeHACmdFlags.postgresqlIp == "" {
			c.Help()
			return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
		}
		configFilePath := filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml")
		if !checkIfFileExist(configFilePath) {
			return status.New(status.FileAccessError, fmt.Sprintf("%s file not found.", configFilePath))
		}
		// check deployment type AWS or ExistingInfra
		deployerType, err := getModeFromConfig(configFilePath)
		if err != nil {
			return err
		}
		if deployerType == EXISTING_INFRA_MODE {
			nodedeleter := NewDeleteNode(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), configFilePath, &fileutils.FileSystemUtils{})
			err := nodedeleter.validate()
			if err != nil {
				return err
			}
			err = nodedeleter.modifyConfig()
			if err != nil {
				return err
			}
			if !addDeleteNodeHACmdFlags.autoAccept {
				res, err := nodedeleter.promptUserConfirmation()
				if err != nil {
					return err
				}
				if !res {
					return nil
				}
			}
			nodedeleter.prepare()
			err = nodedeleter.runDeploy()
			if err != nil {
				return err
			}
		} else {
			return errors.New(fmt.Sprintf("Failed to get deployment type. Please check %s", configFilePath))
		}
		return nil
	}
}

func deleteNodeHACmd() *cobra.Command {
	var addDeleteNodeHACmdFlags = AddDeleteNodeHACmdFlags{}
	var deleteNodeHACmd = &cobra.Command{
		Use:   "remove",
		Short: "remove existing node in HA",
		Long:  `remove existing node in HA`,
		RunE:  runDeleteNodeHACmd(&addDeleteNodeHACmdFlags),
	}
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.automateIp, "automate", "", "new automate ip addresses")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server", "", "new chef-server ip addresses")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.opensearchIp, "opensearch", "", "new opensearch ip addresses")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql", "", "new postgresql ip addresses")
	deleteNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "auto-accept", "y", false, "auto-accept")

	return deleteNodeHACmd
}

type DeleteNodeImpl struct {
	config                  ExistingInfraConfigToml
	copyConfigForUserPrompt ExistingInfraConfigToml
	automateIpList          []string
	chefServerIpList        []string
	opensearchIpList        []string
	postgresqlIp            []string
	nodeUtils               NodeOpUtils
	flags                   AddDeleteNodeHACmdFlags
	configpath              string
	writer                  *cli.Writer
	fileUtils               fileutils.FileUtils
}

func NewDeleteNode(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, filepath string, fileutils fileutils.FileUtils) HAModifyAndDeploy {
	return &DeleteNodeImpl{
		flags:      flags,
		writer:     writer,
		nodeUtils:  nodeUtils,
		configpath: filepath,
		fileUtils:  fileutils,
	}
}

func (dni *DeleteNodeImpl) prepare() error {
	return dni.nodeUtils.taintTerraform()
}

func (dni *DeleteNodeImpl) validate() error {
	var err error
	dni.config, err = dni.nodeUtils.readConfig(dni.configpath)
	if err != nil {
		return err
	}
	dni.copyConfigForUserPrompt = dni.config
	dni.automateIpList, dni.chefServerIpList, dni.opensearchIpList, dni.postgresqlIp = splitIPCSV(
		dni.flags.automateIp,
		dni.flags.chefServerIp,
		dni.flags.opensearchIp,
		dni.flags.postgresqlIp,
	)
	errorList := dni.validateCmdArgs(dni.automateIpList, dni.chefServerIpList, dni.postgresqlIp, dni.opensearchIpList, dni.config)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errorList), status.ConfigError, "IP address validation failed")
	}
	return nil
}
func (dni *DeleteNodeImpl) modifyConfig() error {
	err := modifyConfigForDeleteNode(&dni.config.Automate.Config.InstanceCount, &dni.config.ExistingInfra.Config.AutomatePrivateIps, dni.automateIpList, &dni.config.Automate.Config.CertsByIP)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying automate instance count")
	}
	err = modifyConfigForDeleteNode(&dni.config.ChefServer.Config.InstanceCount, &dni.config.ExistingInfra.Config.ChefServerPrivateIps, dni.chefServerIpList, &dni.config.ChefServer.Config.CertsByIP)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying chef-server instance count")
	}
	err = modifyConfigForDeleteNode(&dni.config.Opensearch.Config.InstanceCount, &dni.config.ExistingInfra.Config.OpensearchPrivateIps, dni.opensearchIpList, &dni.config.Opensearch.Config.CertsByIP)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying opensearch instance count")
	}
	err = modifyConfigForDeleteNode(&dni.config.Postgresql.Config.InstanceCount, &dni.config.ExistingInfra.Config.PostgresqlPrivateIps, dni.postgresqlIp, &dni.config.Postgresql.Config.CertsByIP)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying postgresql instance count")
	}
	return nil
}
func (dni *DeleteNodeImpl) promptUserConfirmation() (bool, error) {
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
	if len(dni.postgresqlIp) > 0 {
		dni.writer.Println("Postgresql => " + strings.Join(dni.postgresqlIp, ", "))
	}
	return dni.writer.Confirm("This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue?")
}

func (dni *DeleteNodeImpl) runDeploy() error {
	tomlbytes, err := ptoml.Marshal(dni.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error converting config to bytes")
	}
	err = dni.fileUtils.WriteToFile(dni.configpath, tomlbytes)
	if err != nil {
		return err
	}
	err = dni.nodeUtils.genConfig(dni.configpath)
	if err != nil {
		return err
	}
	argsdeploy := []string{"-y"}
	return dni.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
}

func (dni *DeleteNodeImpl) validateCmdArgs(automateIpList, chefServerIpList, postgresqlIp, opensearchIpList []string, config ExistingInfraConfigToml) *list.List {
	errorList := list.New()
	if len(automateIpList) > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Automate.Config.InstanceCount, -len(automateIpList), AUTOMATE_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating automate final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Automate instance count cannot be less than %d. Final count %d not allowed.", AUTOMATE_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(config.ExistingInfra.Config.AutomatePrivateIps, automateIpList, "Automate"))
	}
	if len(chefServerIpList) > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.ChefServer.Config.InstanceCount, -len(chefServerIpList), CHEF_SERVER_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating chef server final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Chef Server instance count cannot be less than %d. Final count %d not allowed.", CHEF_SERVER_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(config.ExistingInfra.Config.ChefServerPrivateIps, chefServerIpList, "Chef-Server"))
	}
	if len(opensearchIpList) > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Opensearch.Config.InstanceCount, -len(opensearchIpList), OPENSEARCH_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating opensearch final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. OpenSearch instance count cannot be less than %d. Final count %d not allowed.", OPENSEARCH_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(config.ExistingInfra.Config.OpensearchPrivateIps, opensearchIpList, "OpenSearch"))
	}
	if len(postgresqlIp) > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Postgresql.Config.InstanceCount, -len(postgresqlIp), POSTGRESQL_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating postgresql final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Postgresql instance count cannot be less than %d. Final count %d not allowed.", POSTGRESQL_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(config.ExistingInfra.Config.PostgresqlPrivateIps, postgresqlIp, "Postgresql"))
	}
	return errorList
}

func checkIfPresentInPrivateIPList(existingIPArray []string, ips []string, errorPrefix string) *list.List {
	errorList := list.New()
	prefixAdder := ""
	if errorPrefix != "" {
		prefixAdder = " "
	}
	for _, ip := range ips {
		if !stringutils.SliceContains(existingIPArray, ip) {
			errorList.PushBack(fmt.Sprintf("%s%sIp %s is not present in existing list of ip addresses. Please use a different private ip.", errorPrefix, prefixAdder, ip))
		}
	}
	return errorList
}
