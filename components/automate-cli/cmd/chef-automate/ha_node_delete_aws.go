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

type DeleteNodeAWSImpl struct {
	config        AwsConfigToml
	ipToDelete    string
	nodeType      string
	nodeUtils     NodeOpUtils
	flags         AddDeleteNodeHACmdFlags
	configpath    string
	terraformPath string
	writer        *cli.Writer
	fileUtils     fileutils.FileUtils
	sshUtil       SSHUtil
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
		config:        AwsConfigToml{},
		nodeUtils:     nodeUtils,
		flags:         flags,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		writer:        writer,
		fileUtils:     fileutils,
		sshUtil:       sshUtil,
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

	// TODO : Remove this after fixing the following ticket
	// https://chefio.atlassian.net/browse/CHEF-3630
	err = dna.nodeUtils.saveConfigToBastion()
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

	previousCount, err := dna.nodeUtils.calculateTotalInstanceCount()
	if err != nil {
		return err
	}

	err = dna.prepare()
	if err != nil {
		return err
	}

	err = dna.runDeploy()
	currentCount, newErr := dna.nodeUtils.calculateTotalInstanceCount()
	if newErr != nil {
		if err != nil {
			return errors.Wrap(err, newErr.Error())
		}
		return newErr
	}

	if currentCount == previousCount-1 {
		stopErr := dna.stopNodes()
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
		dna.writer.Println("Node is successfully deleted from the cluster.")
	}

	return err
}

func (dna *DeleteNodeAWSImpl) modifyConfig() error {
	dna.config.Architecture.ConfigInitials.Architecture = "aws"

	var err error

	switch dna.nodeType {
	case AUTOMATE:
		err = modifyConfigForDeleteNodeForAWS(
			&dna.config.Automate.Config.InstanceCount,
			[]string{dna.ipToDelete},
			&dna.config.Automate.Config.CertsByIP,
		)
	case CHEF_SERVER:
		err = modifyConfigForDeleteNodeForAWS(
			&dna.config.ChefServer.Config.InstanceCount,
			[]string{dna.ipToDelete},
			&dna.config.ChefServer.Config.CertsByIP,
		)
	case POSTGRESQL:
		err = modifyConfigForDeleteNodeForAWS(
			&dna.config.Postgresql.Config.InstanceCount,
			[]string{dna.ipToDelete},
			&dna.config.Postgresql.Config.CertsByIP,
		)
	case OPENSEARCH:
		err = modifyConfigForDeleteNodeForAWS(
			&dna.config.Opensearch.Config.InstanceCount,
			[]string{dna.ipToDelete},
			&dna.config.Opensearch.Config.CertsByIP,
		)
	default:
		return errors.New("Invalid node type")
	}

	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error modifying "+stringutils.TitleReplace(dna.nodeType, "_", "-")+" instance count")
	}

	return nil
}

func (dna *DeleteNodeAWSImpl) prepare() error {
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
	dna.writer.Println("Node to be deleted:")
	dna.writer.Println("================================================")

	dna.writer.Println(fmt.Sprintf("%s => %s", stringutils.TitleReplace(dna.nodeType, "_", "-"), dna.ipToDelete))

	dna.writer.Println("Removal of node for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch node.")
	return dna.writer.Confirm("This will delete the above node from your existing setup. It might take a while. Are you sure you want to continue?")
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
	err = dna.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, dna.config, PROVISION)
	if err != nil {
		return err
	}

	argsdeploy := []string{"-y"}
	err = dna.nodeUtils.executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return err
	}
	dna.config.Architecture.ConfigInitials.Architecture = "deployment"
	err = dna.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, dna.config, DEPLOY)
	if err != nil {
		return err
	}

	err = dna.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	// TODO : Remove this after fixing the following ticket
	// https://chefio.atlassian.net/browse/CHEF-3630
	syncErr := dna.nodeUtils.syncConfigToAllNodes()
	if syncErr != nil {
		if err != nil {
			return errors.Wrap(err, syncErr.Error())
		}
		return syncErr
	}
	return err
}

func (dna *DeleteNodeAWSImpl) runRemoveNodeFromAws() error {

	var instanceType string
	var configNodeIpList []string
	switch dna.nodeType {
	case AUTOMATE:
		instanceType = "chef_automate"
		configNodeIpList = dna.configAutomateIpList
	case CHEF_SERVER:
		instanceType = "chef_server"
		configNodeIpList = dna.configChefServerIpList
	case POSTGRESQL:
		instanceType = "chef_automate_postgresql"
		configNodeIpList = dna.configPostgresqlIpList
	case OPENSEARCH:
		instanceType = "chef_automate_opensearch"
		configNodeIpList = dna.configOpensearchIpList
	default:
		status.New(status.InvalidCommandArgsError, "Invalid node type")
	}

	err := dna.removeNodeIfExists(instanceType, dna.ipToDelete, configNodeIpList)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error removing "+stringutils.TitleReplace(dna.nodeType, "_", "-")+" node")
	}

	return nil
}

func (dna *DeleteNodeAWSImpl) removeNodeIfExists(nodeType, ipToDelete string, configNodeIpList []string) error {
	for i := 0; i < len(configNodeIpList); i++ {
		if configNodeIpList[i] == ipToDelete {
			return dna.removeNodeFromAws(nodeType, i, len(configNodeIpList)-1)
		}
	}
	return nil
}

func (dna *DeleteNodeAWSImpl) validate() error {
	err := dna.getAwsHAIp()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error getting AWS instance Ip")
	}

	automateIpList, chefServerIpList, opensearchIpList, postgresqlIpList := splitIPCSV(
		dna.flags.automateIp,
		dna.flags.chefServerIp,
		dna.flags.opensearchIp,
		dna.flags.postgresqlIp,
	)

	// Check if only one node is being deleted
	if (len(automateIpList) + len(chefServerIpList) + len(opensearchIpList) + len(postgresqlIpList)) != 1 {
		return status.New(status.InvalidCommandArgsError, "Only one node can be deleted at a time")
	}

	// Get the node type and ip address of the node to be deleted
	if len(automateIpList) > 0 {
		dna.ipToDelete = automateIpList[0]
		dna.nodeType = AUTOMATE
	} else if len(chefServerIpList) > 0 {
		dna.ipToDelete = chefServerIpList[0]
		dna.nodeType = CHEF_SERVER
	} else if len(postgresqlIpList) > 0 {
		dna.ipToDelete = postgresqlIpList[0]
		dna.nodeType = POSTGRESQL
	} else if len(opensearchIpList) > 0 {
		dna.ipToDelete = opensearchIpList[0]
		dna.nodeType = OPENSEARCH
	} else {
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
	}

	if !isValidIPFormat(dna.ipToDelete) {
		return status.New(status.InvalidCommandArgsError, "Invalid IP address "+dna.ipToDelete)
	}

	updatedConfig, err := dna.nodeUtils.pullAndUpdateConfigAws(&dna.sshUtil, []string{dna.ipToDelete})
	if err != nil {
		return err
	}
	dna.config = *updatedConfig
	if dna.nodeUtils.isManagedServicesOn() {
		if dna.nodeType == POSTGRESQL || dna.nodeType == OPENSEARCH {
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

	var minCount int
	var instanceCount string
	var configIpList []string

	switch dna.nodeType {
	case AUTOMATE:
		instanceCount = dna.config.Automate.Config.InstanceCount
		minCount = AUTOMATE_MIN_INSTANCE_COUNT
		configIpList = dna.configAutomateIpList
	case CHEF_SERVER:
		instanceCount = dna.config.ChefServer.Config.InstanceCount
		minCount = CHEF_SERVER_MIN_INSTANCE_COUNT
		configIpList = dna.configChefServerIpList
	case POSTGRESQL:
		instanceCount = dna.config.Postgresql.Config.InstanceCount
		minCount = POSTGRESQL_MIN_INSTANCE_COUNT
		configIpList = dna.configPostgresqlIpList
	case OPENSEARCH:
		instanceCount = dna.config.Opensearch.Config.InstanceCount
		minCount = OPENSEARCH_MIN_INSTANCE_COUNT
		configIpList = dna.configOpensearchIpList
	default:
		errorList.PushBack("Invalid node type")
	}

	allowed, finalCount, err := isFinalInstanceCountAllowed(instanceCount, -1, minCount)
	if err != nil {
		errorList.PushBack(fmt.Sprintf("Error occurred in calculating %s final instance count", stringutils.TitleReplace(dna.nodeType, "_", "-")))
	}
	if !allowed {
		errorList.PushBack(fmt.Sprintf("Unable to remove node. %s instance count cannot be less than %d. Final count %d not allowed.", stringutils.TitleReplace(dna.nodeType, "_", "-"), minCount, finalCount))
	}
	errorList.PushBackList(checkIfPresentInPrivateIPList(configIpList, []string{dna.ipToDelete}, stringutils.TitleReplace(dna.nodeType, "_", "-")))

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

// Stop all services on the node to be deleted
func (dna *DeleteNodeAWSImpl) stopNodes() error {

	infra, _, err := dna.nodeUtils.getHaInfraDetails()
	if err != nil {
		return err
	}

	err = dna.nodeUtils.stopServicesOnNode(dna.ipToDelete, dna.nodeType, AWS_MODE, infra)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "Error stopping services on node")
	}
	return nil
}
