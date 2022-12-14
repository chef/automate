package main

import (
	"fmt"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type AddNodeAWSImpl struct {
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
	err = ani.runDeploy()
	if err != nil {
		return err
	}
	return nil
}

func (ani *AddNodeAWSImpl) prepare() error {
	return ani.nodeUtils.taintTerraform(ani.terraformPath)
}

func (ani *AddNodeAWSImpl) validate() error {
	updatedConfig, err := ani.nodeUtils.pullAndUpdateConfig(&ani.sshUtil, []string{})
	if err != nil {
		return err
	}
	ani.config = *updatedConfig
	ani.copyConfigForUserPrompt = ani.config
	deployerType, err := ani.nodeUtils.getModeFromConfig(ani.configpath)
	if err != nil {
		return err
	}
	if deployerType == AWS_MODE {
		if ani.config.ExternalDB.Database.Type == TYPE_AWS || ani.config.ExternalDB.Database.Type == TYPE_SELF_MANAGED {
			if ani.flags.opensearchCount > 0 || ani.flags.postgresqlCount > 0 {
				return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "add"))
			}
		}
		if ani.flags.automateCount == 0 &&
			ani.flags.chefServerCount == 0 &&
			ani.flags.opensearchCount == 0 &&
			ani.flags.postgresqlCount == 0 {
			return errors.New("Either one of automate-count or chef-server-count or opensearch-count or postgresql-count must be more than 0.")
		}
	} else {
		return errors.New(fmt.Sprintf("Unsupported deployment type. Please check %s", ani.configpath))
	}
	return nil
}

func (ani *AddNodeAWSImpl) modifyConfig() error {
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
	tomlbytes, err := ptoml.Marshal(ani.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Error converting config to bytes")
	}
	err = ani.fileutils.WriteToFile(ani.configpath, tomlbytes)
	if err != nil {
		return err
	}
	err = ani.nodeUtils.genConfig(ani.configpath)
	if err != nil {
		return err
	}
	argsdeploy := []string{"-y"}
	err = executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return err
	}
	return ani.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
}
