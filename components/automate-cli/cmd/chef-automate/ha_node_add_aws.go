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
	AWSConfigIp
	nodeUtils     NodeOpUtils
	flags         AddDeleteNodeHACmdFlags
	configpath    string
	terraformPath string
	writer        *cli.Writer
	fileutils     fileutils.FileUtils
	sshUtil       SSHUtil
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

func (ani *AddNodeAWSImpl) runDeploy() error {
	err := ani.nodeUtils.moveAWSAutoTfvarsFile(ani.terraformPath)
	if err != nil {
		return err
	}
	err = ani.nodeUtils.modifyTfArchFile(ani.terraformPath)
	if err != nil {
		return err
	}
	err = ani.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, ani.config, PROVISION)
	if err != nil {
		return err
	}
	argsdeploy := []string{"-y"}
	err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return err
	}
	err = ani.modifyConfigForCertByIp()
	if err != nil {
		return err
	}
	ani.config.Architecture.ConfigInitials.Architecture = "deployment"
	err = ani.nodeUtils.writeHAConfigFiles(awsA2harbTemplate, ani.config, DEPLOY)
	if err != nil {
		return err
	}
	err = ani.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
	syncErr := ani.nodeUtils.syncConfigToAllNodes()
	if syncErr != nil {
		if err != nil {
			return errors.Wrap(err, syncErr.Error())
		}
		return syncErr
	}
	return err
}

func (ani *AddNodeAWSImpl) getAwsHAIp() error {
	ConfigIp, err := ani.nodeUtils.getAWSConfigIp()
	if err != nil {
		return err
	}
	ani.AWSConfigIp = *ConfigIp
	return nil
}

func (ani *AddNodeAWSImpl) modifyConfigForCertByIp() error {
	err := ani.getAwsHAIp()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error fetching ips")
	}
	err = modifyConfigForNewNodeCertByIp(
		ani.flags.automateCount,
		ani.configAutomateIpList,
		&ani.config.Automate.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error adding certs in automate")
	}
	err = modifyConfigForNewNodeCertByIp(
		ani.flags.chefServerCount,
		ani.configChefServerIpList,
		&ani.config.ChefServer.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error adding certs in  chef-server ")
	}
	err = modifyConfigForNewNodeCertByIp(
		ani.flags.opensearchCount,
		ani.configOpensearchIpList,
		&ani.config.Opensearch.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error adding certs in opensearch")
	}
	err = modifyConfigForNewNodeCertByIp(
		ani.flags.postgresqlCount,
		ani.configPostgresqlIpList,
		&ani.config.Postgresql.Config.CertsByIP,
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error adding certs in postgresql")
	}
	return nil
}
