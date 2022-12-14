package main

import (
	"container/list"
	"fmt"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type DeleteNodeAWSImpl struct {
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
	fileUtils               fileutils.FileUtils
	sshUtil                 SSHUtil
}

func NewDeleteNodeAWS(writer *cli.Writer, flags AddDeleteNodeHACmdFlags, nodeUtils NodeOpUtils, haDirPath string, fileutils fileutils.FileUtils, sshUtil SSHUtil) HAModifyAndDeploy {
	return &DeleteNodeAWSImpl{
		flags:         flags,
		writer:        writer,
		nodeUtils:     nodeUtils,
		configpath:    filepath.Join(haDirPath, "config.toml"),
		terraformPath: filepath.Join(haDirPath, "terraform"),
		fileUtils:     fileutils,
		sshUtil:       sshUtil,
	}
}

func (dni *DeleteNodeAWSImpl) Execute(c *cobra.Command, args []string) error {
	if !dni.nodeUtils.isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
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
	err = dni.runDeploy()
	if err != nil {
		return err
	}
	return nil
}

func (dni *DeleteNodeAWSImpl) prepare() error {
	return dni.nodeUtils.taintTerraform(dni.terraformPath)
}

func (dni *DeleteNodeAWSImpl) validate() error {
	updatedConfig, err := dni.nodeUtils.pullAndUpdateConfig(&dni.sshUtil, []string{})
	if err != nil {
		return err
	}
	dni.config = *updatedConfig
	dni.copyConfigForUserPrompt = dni.config
	deployerType, err := dni.nodeUtils.getModeFromConfig(dni.configpath)
	if err != nil {
		return err
	}
	if deployerType == AWS_MODE {
		if dni.config.ExternalDB.Database.Type == TYPE_AWS || dni.config.ExternalDB.Database.Type == TYPE_SELF_MANAGED {
			if dni.flags.opensearchCount > 0 || dni.flags.postgresqlCount > 0 {
				return status.New(status.ConfigError, fmt.Sprintf(TYPE_ERROR, "remove"))
			}
		}
		if dni.flags.automateCount == 0 &&
			dni.flags.chefServerCount == 0 &&
			dni.flags.opensearchCount == 0 &&
			dni.flags.postgresqlCount == 0 {
			return errors.New("Either one of automate-count or chef-server-count or opensearch-count or postgresql-count must be more than 0.")
		}
		errorList := dni.validateCmdArgs()
		if errorList != nil && errorList.Len() > 0 {
			return status.Wrap(getSingleErrorFromList(errorList), status.ConfigError, "Instance count validation failed")
		}
	} else {
		return errors.New(fmt.Sprintf("Unsupported deployment type. Please check %s", dni.configpath))
	}
	return nil
}

func (dni *DeleteNodeAWSImpl) validateCmdArgs() *list.List {
	errorList := list.New()
	if dni.flags.automateCount > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Automate.Config.InstanceCount, -dni.flags.automateCount, AUTOMATE_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating automate final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Automate instance count cannot be less than %d. Final count %d not allowed.", AUTOMATE_MIN_INSTANCE_COUNT, finalCount))
		}
	}
	if dni.flags.chefServerCount > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.ChefServer.Config.InstanceCount, -dni.flags.chefServerCount, CHEF_SERVER_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating chef server final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Chef Server instance count cannot be less than %d. Final count %d not allowed.", CHEF_SERVER_MIN_INSTANCE_COUNT, finalCount))
		}
	}
	if dni.flags.opensearchCount > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Opensearch.Config.InstanceCount, -dni.flags.opensearchCount, OPENSEARCH_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating opensearch final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. OpenSearch instance count cannot be less than %d. Final count %d not allowed.", OPENSEARCH_MIN_INSTANCE_COUNT, finalCount))
		}
	}
	if dni.flags.postgresqlCount > 0 {
		allowed, finalCount, err := isFinalInstanceCountAllowed(dni.config.Postgresql.Config.InstanceCount, -dni.flags.postgresqlCount, POSTGRESQL_MIN_INSTANCE_COUNT)
		if err != nil {
			errorList.PushBack("Error occurred in calculating postgresql final instance count")
		}
		if !allowed {
			errorList.PushBack(fmt.Sprintf("Unable to remove node. Postgresql instance count cannot be less than %d. Final count %d not allowed.", POSTGRESQL_MIN_INSTANCE_COUNT, finalCount))
		}
	}
	return errorList
}

func (dni *DeleteNodeAWSImpl) modifyConfig() error {
	inc, err := modifyInstanceCount(dni.config.Automate.Config.InstanceCount, -dni.flags.automateCount)
	if err != nil {
		return err
	}
	dni.config.Automate.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(dni.config.ChefServer.Config.InstanceCount, -dni.flags.chefServerCount)
	if err != nil {
		return err
	}
	dni.config.ChefServer.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(dni.config.Opensearch.Config.InstanceCount, -dni.flags.opensearchCount)
	if err != nil {
		return err
	}
	dni.config.Opensearch.Config.InstanceCount = inc
	inc, err = modifyInstanceCount(dni.config.Postgresql.Config.InstanceCount, -dni.flags.postgresqlCount)
	if err != nil {
		return err
	}
	dni.config.Postgresql.Config.InstanceCount = inc
	return nil
}

func (dni *DeleteNodeAWSImpl) promptUserConfirmation() (bool, error) {
	dni.writer.Println("Existing node count:")
	dni.writer.Println("================================================")
	dni.writer.Println("Automate => " + dni.copyConfigForUserPrompt.Automate.Config.InstanceCount)
	dni.writer.Println("Chef-Server => " + dni.copyConfigForUserPrompt.ChefServer.Config.InstanceCount)
	dni.writer.Println("OpenSearch => " + dni.copyConfigForUserPrompt.Opensearch.Config.InstanceCount)
	dni.writer.Println("Postgresql => " + dni.copyConfigForUserPrompt.Postgresql.Config.InstanceCount)
	dni.writer.Println("")
	dni.writer.Println("New node count:")
	dni.writer.Println("================================================")
	dni.writer.Println("Automate => " + dni.config.Automate.Config.InstanceCount)
	dni.writer.Println("Chef-Server => " + dni.config.ChefServer.Config.InstanceCount)
	dni.writer.Println("OpenSearch => " + dni.config.Opensearch.Config.InstanceCount)
	dni.writer.Println("Postgresql => " + dni.config.Postgresql.Config.InstanceCount)
	return dni.writer.Confirm("This will remove the nodes from your existing setup. It might take a while. Are you sure you want to continue?")
}

func (dni *DeleteNodeAWSImpl) runDeploy() error {
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
	err = executeAutomateClusterCtlCommandAsync("provision", argsdeploy, provisionInfraHelpDocs)
	if err != nil {
		return err
	}
	return dni.nodeUtils.executeAutomateClusterCtlCommandAsync("deploy", argsdeploy, upgradeHaHelpDoc)
}
