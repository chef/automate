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

type SvcDetails struct {
	ipList         []string
	configIpList   []string
	instanceCount  string
	minInstanceCnt int
	name           string
}
type DeleteNodeAWSImpl struct {
	config           AwsConfigToml
	automateIpList   []string
	chefServerIpList []string
	opensearchIpList []string
	postgresqlIpList []string
	nodeUtils        NodeOpUtils
	flags            AddDeleteNodeHACmdFlags
	configpath       string
	terraformPath    string
	writer           *cli.Writer
	fileUtils        fileutils.FileUtils
	sshUtil          SSHUtil
	AWSConfigIp
}

type AWSConfigIp struct {
	configAutomateIpList   []string
	configChefServerIpList []string
	configOpensearchIpList []string
	configPostgresqlIpList []string
}

const (
	terrformStateFile    = "-state=/hab/a2_deploy_workspace/terraform/terraform.tfstate"
	runTerraformInit     = "terraform init"
	terraformPath        = "/hab/a2_deploy_workspace/terraform"
	destoryTerraformPath = terraformPath + "/destroy/aws/"
)

var (
	destoryCommand   = `terraform destroy -target="module.aws.aws_instance.%[1]s[%[2]d]" -refresh=true -auto-approve`
	moveStateCommand = `terraform state mv "module.aws.aws_instance.%[1]s[%[2]d]" "module.aws.aws_instance.%[1]s[%[3]d]"`
)

func NewDeleteNodeAWS(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileutils fileutils.FileUtils, sshUtil SSHUtil) HAModifyAndDeploy {
	return &DeleteNodeAWSImpl{
		config:           AwsConfigToml{},
		automateIpList:   []string{},
		chefServerIpList: []string{},
		opensearchIpList: []string{},
		postgresqlIpList: []string{},
		nodeUtils:        nodeUtils,
		flags:            flags,
		configpath:       filepath.Join(haDirPath, "config.toml"),
		terraformPath:    filepath.Join(haDirPath, "terraform"),
		writer:           writer,
		fileUtils:        fileutils,
		sshUtil:          sshUtil,
	}
}

func (dna *DeleteNodeAWSImpl) Execute(c *cobra.Command, args []string) error {
	if !dna.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}

	if dna.flags.automateIp == "" &&
		dna.flags.chefServerIp == "" &&
		dna.flags.opensearchIp == "" &&
		dna.flags.postgresqlIp == "" {
		c.Help()
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
	}
	err := dna.validate()
	if err != nil {
		return err
	}
	err = dna.modifyConfig()
	if err != nil {
		return err
	}
	if !dna.flags.autoAccept {
		res, err := dna.promptUserConfirmation()
		if err != nil {
			return err
		}
		if !res {
			return nil
		}
	}

	err = dna.prepare()
	if err != nil {
		return err
	}

	return dna.runDeploy()
}
func (dna *DeleteNodeAWSImpl) modifyConfig() error {
	dna.config.Architecture.ConfigInitials.Architecture = "aws"
	err := modifyConfigForDeleteNodeForAWS(
		&dna.config.Automate.Config.InstanceCount,
		dna.automateIpList,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying automate instance count")
	}
	err = modifyConfigForDeleteNodeForAWS(
		&dna.config.ChefServer.Config.InstanceCount,
		dna.chefServerIpList,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying chef-server instance count")
	}
	err = modifyConfigForDeleteNodeForAWS(
		&dna.config.Opensearch.Config.InstanceCount,
		dna.opensearchIpList,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying opensearch instance count")
	}
	err = modifyConfigForDeleteNodeForAWS(
		&dna.config.Postgresql.Config.InstanceCount,
		dna.postgresqlIpList,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying postgresql instance count")
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) prepare() error {
	// Stop all services on the node to be deleted
	err := dna.nodeUtils.stopServicesOnNode(dna.automateIpList, dna.chefServerIpList, dna.postgresqlIpList, dna.opensearchIpList)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "Error stoping services on node")
	}

	return dna.nodeUtils.taintTerraform(dna.terraformPath)
}

func (dna *DeleteNodeAWSImpl) promptUserConfirmation() (bool, error) {
	dna.writer.Println("Existing nodes:")
	dna.writer.Println("================================================")
	dna.writer.Println("Automate => " + strings.Join(dna.AWSConfigIp.configAutomateIpList, ", "))
	dna.writer.Println("Chef-Server => " + strings.Join(dna.AWSConfigIp.configChefServerIpList, ", "))
	dna.writer.Println("OpenSearch => " + strings.Join(dna.AWSConfigIp.configOpensearchIpList, ", "))
	dna.writer.Println("Postgresql => " + strings.Join(dna.AWSConfigIp.configPostgresqlIpList, ", "))
	dna.writer.Println("")
	dna.writer.Println("Nodes to be deleted:")
	dna.writer.Println("================================================")
	if len(dna.automateIpList) > 0 {
		dna.writer.Println("Automate => " + strings.Join(dna.automateIpList, ", "))
	}
	if len(dna.chefServerIpList) > 0 {
		dna.writer.Println("Chef-Server => " + strings.Join(dna.chefServerIpList, ", "))
	}
	if len(dna.opensearchIpList) > 0 {
		dna.writer.Println("OpenSearch => " + strings.Join(dna.opensearchIpList, ", "))
	}
	if len(dna.postgresqlIpList) > 0 {
		dna.writer.Println("Postgresql => " + strings.Join(dna.postgresqlIpList, ", "))
	}
	dna.writer.Println("Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.")
	return dna.writer.Confirm("This will delete the above nodes from your existing setup. It might take a while. Are you sure you want to continue?")
}

func (dna *DeleteNodeAWSImpl) runDeploy() error {
	err := dna.runRemoveNodeFromAws()
	if err != nil {
		return err
	}

	err = dna.nodeUtils.moveAWSAutoTfvarsFile(dna.terraformPath)
	if err != nil {
		return err
	}

	err = dna.nodeUtils.modifyTfArchFile(dna.terraformPath)
	if err != nil {
		return err
	}
	err = dna.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, dna.config)
	if err != nil {
		return err
	}

	argsdeploy := []string{"-y"}
	err = dna.nodeUtils.executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return err
	}

	return dna.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
}

func (dna *DeleteNodeAWSImpl) runRemoveNodeFromAws() error {
	err := dna.removeNodeIfExists("chef_automate", dna.automateIpList, dna.configAutomateIpList)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error removing automate node")
	}

	err = dna.removeNodeIfExists("chef_server", dna.chefServerIpList, dna.configChefServerIpList)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error removing chef server node")
	}

	err = dna.removeNodeIfExists("chef_automate_opensearch", dna.opensearchIpList, dna.configOpensearchIpList)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error removing opensearch node")
	}

	err = dna.removeNodeIfExists("chef_automate_postgresql", dna.postgresqlIpList, dna.configPostgresqlIpList)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error removing postgresql node")
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) removeNodeIfExists(nodeType string, nodeIpList, configNodeIpList []string) error {
	if len(nodeIpList) == 1 {
		for i := 0; i < len(configNodeIpList); i++ {
			if configNodeIpList[i] == nodeIpList[0] {
				return dna.removeNodeFromAws(nodeType, i, len(configNodeIpList)-1)
			}
		}
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) validate() error {
	err := dna.getAwsHAIp()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error getting AWS instance Ip")
	}
	dna.automateIpList, dna.chefServerIpList, dna.opensearchIpList, dna.postgresqlIpList = splitIPCSV(
		dna.flags.automateIp,
		dna.flags.chefServerIp,
		dna.flags.opensearchIp,
		dna.flags.postgresqlIp,
	)
	var exceptionIps []string
	exceptionIps = append(exceptionIps, dna.automateIpList...)
	exceptionIps = append(exceptionIps, dna.chefServerIpList...)
	exceptionIps = append(exceptionIps, dna.opensearchIpList...)
	exceptionIps = append(exceptionIps, dna.postgresqlIpList...)

	updatedConfig, err := dna.nodeUtils.pullAndUpdateConfigAws(&dna.sshUtil, exceptionIps)
	if err != nil {
		return err
	}
	dna.config = *updatedConfig
	if dna.nodeUtils.isManagedServicesOn() {
		if len(dna.opensearchIpList) > 0 || len(dna.postgresqlIpList) > 0 {
			return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "remove"))
		}
	}
	errorList := dna.validateCmdArgs()
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errorList), status.ConfigError, "IP address validation failed")
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) validateCmdArgs() *list.List {
	errorList := list.New()
	services := []SvcDetails{
		{dna.automateIpList, dna.configAutomateIpList, dna.config.Automate.Config.InstanceCount, AUTOMATE_MIN_INSTANCE_COUNT, "Automate"},
		{dna.chefServerIpList, dna.configChefServerIpList, dna.config.ChefServer.Config.InstanceCount, CHEF_SERVER_MIN_INSTANCE_COUNT, "Chef-Server"},
	}
	if !dna.nodeUtils.isManagedServicesOn() {
		services = append(services, []SvcDetails{
			{dna.opensearchIpList, dna.configOpensearchIpList, dna.config.Opensearch.Config.InstanceCount, OPENSEARCH_MIN_INSTANCE_COUNT, "OpenSearch"},
			{dna.postgresqlIpList, dna.configPostgresqlIpList, dna.config.Postgresql.Config.InstanceCount, POSTGRESQL_MIN_INSTANCE_COUNT, "Postgresql"},
		}...)
	}

	// Check if only one node is being deleted
	if (len(dna.automateIpList) + len(dna.chefServerIpList) + len(dna.opensearchIpList) + len(dna.postgresqlIpList)) != 1 {
		errorList.PushBack("Only one node can be deleted at a time")
		return errorList
	}

	for _, service := range services {
		if len(service.ipList) > 1 {
			errorList.PushBack(fmt.Sprintf("Only one %s is allowed to delete for AWS deployment type", service.name))
		} else if len(service.ipList) == 1 {
			allowed, finalCount, err := isFinalInstanceCountAllowed(service.instanceCount, -len(service.ipList), service.minInstanceCnt)
			if err != nil {
				errorList.PushBack(fmt.Sprintf("Error occurred in calculating %s final instance count", service.name))
				continue
			}
			if !allowed {
				errorList.PushBack(fmt.Sprintf("Unable to remove node. %s instance count cannot be less than %d. Final count %d not allowed.", service.name, service.minInstanceCnt, finalCount))
			}
			errorList.PushBackList(checkIfPresentInPrivateIPList(service.configIpList, service.ipList, service.name))
		}
	}
	return errorList
}

func (dna *DeleteNodeAWSImpl) removeNodeFromAws(instanceType string, index int, lastIndex int) error {
	err := dna.nodeUtils.executeShellCommand(runTerraformInit, destoryTerraformPath)
	if err != nil {
		return err
	}

	runDestoryCommand := fmt.Sprintf(destoryCommand, instanceType, index)
	err = dna.nodeUtils.executeShellCommand(runDestoryCommand, destoryTerraformPath)
	if err != nil {
		return err
	}

	if index != lastIndex {
		ruMoveStateCommand := fmt.Sprintf(moveStateCommand, instanceType, lastIndex, index)
		err = dna.nodeUtils.executeShellCommand(ruMoveStateCommand, destoryTerraformPath)
		if err != nil {
			return err
		}
	}
	err = dna.nodeUtils.executeShellCommand(runTerraformInit, terraformPath)
	if err != nil {
		return err
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) getAwsHAIp() error {
	ConfigIp, err := dna.nodeUtils.getAWSConfigIp()
	if err != nil {
		return err
	}
	dna.AWSConfigIp = *ConfigIp
	return nil
}
