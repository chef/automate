package main

import (
	"container/list"
	"io/ioutil"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
)

type existingInfra struct {
	config     ExistingInfraConfigToml
	configPath string
}

func newExistingInfa(configPath string) *existingInfra {
	return &existingInfra{
		configPath: configPath,
	}
}

func (e *existingInfra) doDeployWork(args []string) error {
	var err = bootstrapEnv(e)
	if err != nil {
		return err
	}
	err = executeSecretsInitCommand(e.config.Architecture.ConfigInitials.SecretsKeyFile)
	if err != nil {
		return err
	}
	return executeDeployment(args)
}

func (e *existingInfra) doProvisionJob(args []string) error {
	return nil
}

func (e *existingInfra) generateConfig() error {
	templateBytes, err := ioutil.ReadFile(e.getConfigPath())
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	e.config = ExistingInfraConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &e.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	errList := e.validateConfigFields()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid.")
	}
	finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, e.config)
	writeToA2HARBFile(finalTemplate, initConfigHabA2HAPathFlag.a2haDirPath+"a2ha.rb")
	return nil
}

func (e *existingInfra) getConfigPath() string {
	return e.configPath
}

func (e *existingInfra) validateConfigFields() *list.List {
	errorList := list.New()
	if len(e.config.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(e.config.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(e.config.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(e.config.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(e.config.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(e.config.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if len(e.config.Architecture.ConfigInitials.BackupMount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	}
	if len(e.config.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(e.config.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}
	if len(e.config.Elasticsearch.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty elastic-search instance_count")
	}
	if len(e.config.Postgresql.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty postgres-sql instance_count")
	}

	if len(e.config.ExistingInfra.Config.AutomatePrivateIps) < 1 {
		errorList.PushBack("Invalid or empty automate_private_ips")
	}

	if len(e.config.ExistingInfra.Config.ChefServerPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty chef_server_private_ips")
	}
	if len(e.config.ExistingInfra.Config.ElasticsearchIps) < 1 {
		errorList.PushBack("Invalid or empty elasticsearch_ips")
	}

	if len(e.config.ExistingInfra.Config.ElasticsearchPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty elasticsearch_private_ips")
	}

	if len(e.config.ExistingInfra.Config.PostgresqlPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty postgresql_private_ips")
	}
	errorList.PushBackList(e.validateIPs())
	return errorList
}

func (e *existingInfra) validateIPs() *list.List {
	const notValidErrorString = "is not valid"
	errorList := list.New()

	for _, element := range e.config.ExistingInfra.Config.AutomatePrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Automate private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range e.config.ExistingInfra.Config.ChefServerPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("chef server private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range e.config.ExistingInfra.Config.ElasticsearchIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("elastic search Ip " + element + notValidErrorString)
		}
	}

	for _, element := range e.config.ExistingInfra.Config.ElasticsearchPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("elastic search private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range e.config.ExistingInfra.Config.PostgresqlPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Postgresql private Ip " + element + notValidErrorString)
		}
	}
	return errorList
}
