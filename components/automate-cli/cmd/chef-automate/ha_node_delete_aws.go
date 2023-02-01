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

func NewDeleteNodeAWS(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileutils fileutils.FileUtils, sshUtil SSHUtil) (HAModifyAndDeploy, error) {
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
	}, nil
}

func (dna *DeleteNodeAWSImpl) Execute(c *cobra.Command, args []string) error {
	if !dna.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}

	err := dna.getAwsHAIp()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error getting AWS instance Ip")
	}

	if dna.flags.automateIp == "" &&
		dna.flags.chefServerIp == "" &&
		dna.flags.opensearchIp == "" &&
		dna.flags.postgresqlIp == "" {
		c.Help()
		return status.New(status.InvalidCommandArgsError, "Please provide service name and ip address of the node which you want to delete")
	}
	err = dna.validate()
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
	dna.prepare()

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
	if len(dna.automateIpList) == 1 {
		for i := 0; i < len(dna.configAutomateIpList); i++ {
			if dna.configAutomateIpList[i] == dna.automateIpList[0] {
				err := dna.removeNodeFromAws("chef_automate", i, len(dna.configAutomateIpList)-1)
				if err != nil {
					return status.Wrap(err, status.ConfigError, "Error removing automate node")
				}
				break
			}
		}
	}

	if len(dna.chefServerIpList) == 1 {
		for i := 0; i < len(dna.configChefServerIpList); i++ {
			if dna.configChefServerIpList[i] == dna.chefServerIpList[0] {
				err := dna.removeNodeFromAws("chef_server", i, len(dna.configChefServerIpList)-1)
				if err != nil {
					return status.Wrap(err, status.ConfigError, "Error removing chef server node")
				}
				break
			}
		}
	}

	if len(dna.opensearchIpList) == 1 {
		for i := 0; i < len(dna.configOpensearchIpList); i++ {
			if dna.configOpensearchIpList[i] == dna.opensearchIpList[0] {
				err := dna.removeNodeFromAws("chef_automate_opensearch", i, len(dna.configOpensearchIpList)-1)
				if err != nil {
					return status.Wrap(err, status.ConfigError, "Error removing opensearch node")
				}
				break
			}
		}
	}

	if len(dna.postgresqlIpList) == 1 {
		for i := 0; i < len(dna.configPostgresqlIpList); i++ {
			if dna.configPostgresqlIpList[i] == dna.postgresqlIpList[0] {
				fmt.Println(dna.configPostgresqlIpList[i])
				err := dna.removeNodeFromAws("chef_automate_postgresql", i, len(dna.configPostgresqlIpList)-1)
				if err != nil {
					return status.Wrap(err, status.ConfigError, "Error removing postgresql node")
				}
				break
			}
		}
	}
	return nil
}
func (dna *DeleteNodeAWSImpl) validate() error {
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
	if len(dna.automateIpList) == 1 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dna.config.Automate.Config.InstanceCount, -len(dna.automateIpList), AUTOMATE_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating automate final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Automate instance count cannot be less than %d. Final count %d not allowed.", AUTOMATE_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(dna.configAutomateIpList, dna.automateIpList, "Automate"))
	} else if len(dna.automateIpList) != 0 {
		errorList.PushBack("only one automate is allowed to delete for aws deployment type")
	}
	if len(dna.chefServerIpList) == 1 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dna.config.ChefServer.Config.InstanceCount, -len(dna.chefServerIpList), CHEF_SERVER_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating chef server final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Chef Server instance count cannot be less than %d. Final count %d not allowed.", CHEF_SERVER_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(dna.configChefServerIpList, dna.chefServerIpList, "Chef-Server"))
	} else if len(dna.chefServerIpList) != 0 {
		errorList.PushBack("only one Chef server is allowed to delete for aws deployment type")
	}
	if len(dna.opensearchIpList) == 1 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dna.config.Opensearch.Config.InstanceCount, -len(dna.opensearchIpList), OPENSEARCH_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating opensearch final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. OpenSearch instance count cannot be less than %d. Final count %d not allowed.", OPENSEARCH_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(dna.configOpensearchIpList, dna.opensearchIpList, "OpenSearch"))
	} else if len(dna.opensearchIpList) != 0 {
		errorList.PushBack("only one opensearch is allowed to delete for aws deployment type")
	}
	if len(dna.postgresqlIpList) == 1 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dna.config.Postgresql.Config.InstanceCount, -len(dna.postgresqlIpList), POSTGRESQL_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating postgresql final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Postgresql instance count cannot be less than %d. Final count %d not allowed.", POSTGRESQL_MIN_INSTANCE_COUNT, finalCount))
		}
		errorList.PushBackList(checkIfPresentInPrivateIPList(dna.configPostgresqlIpList, dna.postgresqlIpList, "Postgresql"))
	} else if len(dna.postgresqlIpList) != 0 {
		errorList.PushBack("only one postgresql is allowed to delete for aws deployment type")
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
