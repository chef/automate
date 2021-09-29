// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"container/list"
	"html/template"
	"io/ioutil"
	"net"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

func getSingleErrorFromList(l *list.List) error {
	errorString := ""
	for e := l.Front(); e != nil; e = e.Next() {
		errorString = errorString + "\n" + e.Value.(string)
	}
	return errors.New(errorString)
}

func readConfigAndWriteToFile(configPath string) error {
	writer.Printf("reading configs from toml file\n")
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath
	}
	templateBytes, err := ioutil.ReadFile(initConfigHAPath)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	architectureAws := config.Get("architecture.aws")
	if architectureAws != nil {
		AwsConfig := AwsConfigToml{}
		err := ptoml.Unmarshal(templateBytes, &AwsConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
		}
		errList := validateAwsFields(AwsConfig)
		if errList != nil && errList.Len() > 0 {
			return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid.")
		}
		finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, AwsConfig)
		writeToA2HARBFile(finalTemplate, filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb"))
	} else if config.Get("architecture.existing_infra") != nil {
		ExitiingInfraConfig := ExistingInfraConfigToml{}
		err := ptoml.Unmarshal(templateBytes, &ExitiingInfraConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
		}
		errList := validateExistingInfraFields(ExitiingInfraConfig)
		if errList != nil && errList.Len() > 0 {
			return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid.")
		}
		finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, ExitiingInfraConfig)
		writeToA2HARBFile(finalTemplate, initConfigHabA2HAPathFlag.a2haDirPath+"a2ha.rb")
	} else {
		msg := "Invalid toml configuration"
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	}
	return nil
}

func writeToA2HARBFile(template string, path string) {
	err := ioutil.WriteFile(path, []byte(template), 0600)
	if err != nil {
		writer.Printf("Writing into A2HA.rb file failed\n")
	}
	writer.Printf("Config written to %s\n", path)
}

func renderSettingsToA2HARBFile(templateName string, data interface{}) string {
	temp := template.Must(template.New("init").
		Funcs(template.FuncMap{"StringsJoin": strings.Join}).
		Parse(templateName))

	var buf bytes.Buffer
	err := temp.Execute(&buf, data)
	if err != nil {
		writer.Printf("some error occurred while rendering template \n %s", err)
	}
	finalTemplate := buf.String()
	return finalTemplate
}

func validateAwsFields(awsConfig AwsConfigToml) *list.List {
	errorList := list.New()
	if len(awsConfig.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(awsConfig.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(awsConfig.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(awsConfig.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(awsConfig.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(awsConfig.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if len(awsConfig.Architecture.ConfigInitials.BackupMount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	}
	if len(awsConfig.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(awsConfig.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}
	if len(awsConfig.Elasticsearch.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty elastic-search instance_count")
	}
	if len(awsConfig.Postgresql.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty postgres-sql instance_count")
	}
	errorList.PushBackList(validateAwsConfigFields(awsConfig))
	return errorList
}

func validateExistingInfraFields(exitingInfra ExistingInfraConfigToml) *list.List {
	errorList := list.New()
	if len(exitingInfra.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(exitingInfra.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(exitingInfra.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(exitingInfra.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(exitingInfra.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(exitingInfra.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if len(exitingInfra.Architecture.ConfigInitials.BackupMount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	}
	if len(exitingInfra.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(exitingInfra.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}
	if len(exitingInfra.Elasticsearch.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty elastic-search instance_count")
	}
	if len(exitingInfra.Postgresql.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty postgres-sql instance_count")
	}
	if len(exitingInfra.ExistingInfra.Config.AutomateIps) < 1 {
		errorList.PushBack("Invalid or empty automate Ips")
	}
	if len(exitingInfra.ExistingInfra.Config.AutomatePrivateIps) < 1 {
		errorList.PushBack("Invalid or empty automate_private_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.ChefServerIps) < 1 {
		errorList.PushBack("Invalid or empty chef_server_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.ChefServerPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty chef_server_private_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.ElasticsearchIps) < 1 {
		errorList.PushBack("Invalid or empty elasticsearch_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.ElasticsearchPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty elasticsearch_private_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.PostgresqlIps) < 1 {
		errorList.PushBack("Invalid or empty postgresql_ips")
	}
	if len(exitingInfra.ExistingInfra.Config.PostgresqlPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty postgresql_private_ips")
	}
	errorList.PushBackList(validateIPs(exitingInfra))
	return errorList
}

func validateAwsConfigFields(awsConfig AwsConfigToml) *list.List {
	errorList := list.New()
	if len(awsConfig.Aws.Config.Profile) < 1 {
		errorList.PushBack("Invalid or empty aws profile name")
	}
	if len(awsConfig.Aws.Config.Region) < 1 {
		errorList.PushBack("Invalid or empty aws region")
	}
	if len(awsConfig.Aws.Config.SSHKeyPairName) < 1 {
		errorList.PushBack("Invalid or empty aws ssh_key_pair_name")
	}
	if len(awsConfig.Aws.Config.AutomateServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_server_instance_type")
	}
	if len(awsConfig.Aws.Config.ChefServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_instance_type")
	}
	if len(awsConfig.Aws.Config.ElasticsearchServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws elasticsearch_server_instance_type")
	}
	if len(awsConfig.Aws.Config.PostgresqlServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws postgresql_server_instance_type")
	}
	if len(awsConfig.Aws.Config.AutomateLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws automate_lb_certificate_arn")
	}
	if len(awsConfig.Aws.Config.ChefServerLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_lb_certificate_arn")
	}
	if len(awsConfig.Aws.Config.AutomateEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_iops")
	}
	if len(awsConfig.Aws.Config.AutomateEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_size")
	}
	if len(awsConfig.Aws.Config.AutomateEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_type")
	}
	if len(awsConfig.Aws.Config.ChefEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_iops")
	}
	if len(awsConfig.Aws.Config.ChefEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_size")
	}
	if len(awsConfig.Aws.Config.ChefEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_type")
	}
	if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws elasticsearch_ebs_volume_iops")
	}
	if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws elasticsearch_ebs_volume_size")
	}
	if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws elasticsearch_ebs_volume_type")
	}
	if len(awsConfig.Aws.Config.PostgresqlEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_iops")
	}
	if len(awsConfig.Aws.Config.PostgresqlEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_size")
	}
	if len(awsConfig.Aws.Config.PostgresqlEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_type")
	}
	return errorList
}

func validateIPs(exitingInfra ExistingInfraConfigToml) *list.List {
	const notValidErrorString = "is not valid"
	errorList := list.New()
	for _, element := range exitingInfra.ExistingInfra.Config.AutomateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Automate Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.AutomatePrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Automate private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.ChefServerIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("chef server Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.ChefServerPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("chef server private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.ElasticsearchIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("elastic search Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.ElasticsearchPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("elastic search private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.PostgresqlIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("postgresql Ip " + element + notValidErrorString)
		}
	}

	for _, element := range exitingInfra.ExistingInfra.Config.PostgresqlPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Postgresql private Ip " + element + notValidErrorString)
		}
	}
	return errorList
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("Ip Address is invalid.")
	} else {
		return nil
	}
}
