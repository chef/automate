package main

import (
	"fmt"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type AddNodeAWSImpl struct {
	config                  AwsConfigToml
	copyConfigForUserPrompt AwsConfigToml
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

func NewAddNodeAWS(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileUtils fileutils.FileUtils, sshUtil SSHUtil) HAModifyAndDeploy {
	return &AddNodeAWSImpl{
		flags:         flags,
		writer:        writer,
		nodeUtils:     nodeUtils,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		fileutils:     fileUtils,
		sshUtil:       sshUtil,
	}
}

func (ani *AddNodeAWSImpl) Execute(c *cobra.Command, args []string) error {
	if !ani.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
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
	ani.prepare()
	return ani.runDeploy()
}

func (ani *AddNodeAWSImpl) prepare() error {
	return ani.nodeUtils.taintTerraform(ani.terraformPath)
}

func (ani *AddNodeAWSImpl) validate() error {
	updatedConfig, err := ani.nodeUtils.pullAndUpdateConfigAws(&ani.sshUtil, []string{})
	if err != nil {
		return err
	}
	ani.config = *updatedConfig
	ani.copyConfigForUserPrompt = ani.config
	if ani.flags.automateCount == 0 &&
		ani.flags.chefServerCount == 0 &&
		ani.flags.opensearchCount == 0 &&
		ani.flags.postgresqlCount == 0 {
		return errors.New("Either one of automate-count or chef-server-count or opensearch-count or postgresql-count must be more than 0.")
	}
	if ani.nodeUtils.isManagedServicesOn() {
		if ani.flags.opensearchCount > 0 || ani.flags.postgresqlCount > 0 {
			return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "add"))
		}
	}
	return nil
}

func (ani *AddNodeAWSImpl) modifyConfig() error {
	ani.config.Architecture.ConfigInitials.Architecture = "aws"
	inc, err := modifyInstanceCount(ani.config.Automate.Config.InstanceCount, ani.flags.automateCount)
	if err != nil {
		return err
	}
	ani.config.Automate.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(ani.config.ChefServer.Config.InstanceCount, ani.flags.chefServerCount)
	if err != nil {
		return err
	}
	ani.config.ChefServer.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(ani.config.Opensearch.Config.InstanceCount, ani.flags.opensearchCount)
	if err != nil {
		return err
	}
	ani.config.Opensearch.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(ani.config.Postgresql.Config.InstanceCount, ani.flags.postgresqlCount)
	if err != nil {
		return err
	}
	ani.config.Postgresql.Config.InstanceCount = inc
	return nil
}

func (ani *AddNodeAWSImpl) promptUserConfirmation() (bool, error) {
	ani.writer.Println("Existing node count:")
	ani.writer.Println("================================================")
	ani.writer.Println("Automate => " + ani.copyConfigForUserPrompt.Automate.Config.InstanceCount)
	ani.writer.Println("Chef-Server => " + ani.copyConfigForUserPrompt.ChefServer.Config.InstanceCount)
	ani.writer.Println("OpenSearch => " + ani.copyConfigForUserPrompt.Opensearch.Config.InstanceCount)
	ani.writer.Println("Postgresql => " + ani.copyConfigForUserPrompt.Postgresql.Config.InstanceCount)
	ani.writer.Println("")
	ani.writer.Println("New node count:")
	ani.writer.Println("================================================")
	ani.writer.Println("Automate => " + ani.config.Automate.Config.InstanceCount)
	ani.writer.Println("Chef-Server => " + ani.config.ChefServer.Config.InstanceCount)
	ani.writer.Println("OpenSearch => " + ani.config.Opensearch.Config.InstanceCount)
	ani.writer.Println("Postgresql => " + ani.config.Postgresql.Config.InstanceCount)
	return ani.writer.Confirm("This will add the new nodes to your existing setup. It might take a while. Are you sure you want to continue?")
}

func (ani *AddNodeAWSImpl) saveConfigToBastion() error {
	nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
	return ani.nodeUtils.ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR)
}

func (ani *AddNodeAWSImpl) syncConfigToAllNodes() error {
	nodeObjects := getNodeObjectsToPatchWorkspaceConfigToAllNodes()
	return ani.nodeUtils.ExecuteCmdInAllNodeAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR)
}

func (ani *AddNodeAWSImpl) runDeploy() error {
	err := ani.saveConfigToBastion()
	if err != nil {
		return errors.Wrap(err, "error saving node configuration to bastion")
	}

	// return nil
	err = ani.nodeUtils.moveAWSAutoTfvarsFile(ani.terraformPath)
	if err != nil {
		return errors.Wrap(err, "error moving aws.auto.tfvars file")
	}
	err = ani.nodeUtils.modifyTfArchFile(ani.terraformPath)
	if err != nil {
		return errors.Wrap(err, "error modifying tf_arch file")
	}
	err = ani.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, ani.config)
	if err != nil {
		return errors.Wrap(err, "error writing HA config.toml in workspace directory")
	}
	argsdeploy := []string{"-y"}
	err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return errors.Wrap(err, "error while provisioning resources")
	}
	err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	if err != nil {
		err = errors.Wrap(err, "error while deploying architecture")
	}
	syncErr := ani.syncConfigToAllNodes()
	if syncErr != nil {
		err = errors.Wrapf(syncErr, "error syncing config to all nodes. \n%v", err)
	}

	if err != nil {
		return err
	}
	return nil

}
