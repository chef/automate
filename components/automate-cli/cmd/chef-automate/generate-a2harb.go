// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"html/template"
	"io/ioutil"
	"net"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

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
		err = validateAwsFields(AwsConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "config is invalid.")
		}
		finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, AwsConfig)
		writeToA2HARBFile(finalTemplate, initConfigHabA2HAPathFlag.a2haDirPath+"a2ha.rb")
	} else if config.Get("architecture.existing_infra") != nil {
		ExitiingInfraConfig := ExistingInfraConfigToml{}
		err := ptoml.Unmarshal(templateBytes, &ExitiingInfraConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
		}
		err = validateExistingInfraFields(ExitiingInfraConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "config is invalid.")
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

func validateAwsFields(awsConfig AwsConfigToml) error {
	if len(awsConfig.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		return errors.New("Invalid or empty secrets_key_file")
	} else if len(awsConfig.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		return errors.New("Invalid or empty secrets_store_file")
	} else if len(awsConfig.Architecture.ConfigInitials.Architecture) < 1 {
		return errors.New("Invalid or empty Architecture")
	} else if len(awsConfig.Architecture.ConfigInitials.WorkspacePath) < 1 {
		return errors.New("Invalid or empty workspace_path")
	} else if len(awsConfig.Architecture.ConfigInitials.SSHUser) < 1 {
		return errors.New("Invalid or empty ssh_user")
	} else if len(awsConfig.Architecture.ConfigInitials.SSHUser) < 1 {
		return errors.New("Invalid or empty ssh_user")
	} else if len(awsConfig.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		return errors.New("Invalid or empty ssh_key_file")
	} else if len(awsConfig.Architecture.ConfigInitials.BackupMount) < 1 {
		return errors.New("Invalid or empty backup_mount")
	} else if len(awsConfig.Architecture.ConfigInitials.HabitatUIDGid) < 1 {
		return errors.New("Invalid or empty habitat_uid_gid")
	} else if len(awsConfig.Automate.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty automate instance_count")
	} else if len(awsConfig.ChefServer.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty chef-server instance_count")
	} else if len(awsConfig.Elasticsearch.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty elastic-search instance_count")
	} else if len(awsConfig.Postgresql.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty postgres-sql instance_count")
	} else if len(awsConfig.Aws.Config.Profile) < 1 {
		return errors.New("Invalid or empty aws profile name")
	} else if len(awsConfig.Aws.Config.Region) < 1 {
		return errors.New("Invalid or empty aws region")
	} else if len(awsConfig.Aws.Config.SSHKeyPairName) < 1 {
		return errors.New("Invalid or empty aws ssh_key_pair_name")
	} else if len(awsConfig.Aws.Config.AutomateServerInstanceType) < 1 {
		return errors.New("Invalid or empty aws automate_server_instance_type")
	} else if len(awsConfig.Aws.Config.ChefServerInstanceType) < 1 {
		return errors.New("Invalid or empty aws chef_server_instance_type")
	} else if len(awsConfig.Aws.Config.ElasticsearchServerInstanceType) < 1 {
		return errors.New("Invalid or empty aws elasticsearch_server_instance_type")
	} else if len(awsConfig.Aws.Config.PostgresqlServerInstanceType) < 1 {
		return errors.New("Invalid or empty aws postgresql_server_instance_type")
	} else if len(awsConfig.Aws.Config.AutomateLbCertificateArn) < 1 {
		return errors.New("Invalid or empty aws automate_lb_certificate_arn")
	} else if len(awsConfig.Aws.Config.ChefServerLbCertificateArn) < 1 {
		return errors.New("Invalid or empty aws chef_server_lb_certificate_arn")
	} else if len(awsConfig.Aws.Config.AutomateEbsVolumeIops) < 1 {
		return errors.New("Invalid or empty aws automate_ebs_volume_iops")
	} else if len(awsConfig.Aws.Config.AutomateEbsVolumeSize) < 1 {
		return errors.New("Invalid or empty aws automate_ebs_volume_size")
	} else if len(awsConfig.Aws.Config.AutomateEbsVolumeType) < 1 {
		return errors.New("Invalid or empty aws automate_ebs_volume_type")
	} else if len(awsConfig.Aws.Config.ChefEbsVolumeIops) < 1 {
		return errors.New("Invalid or empty aws chef_ebs_volume_iops")
	} else if len(awsConfig.Aws.Config.ChefEbsVolumeSize) < 1 {
		return errors.New("Invalid or empty aws chef_ebs_volume_size")
	} else if len(awsConfig.Aws.Config.ChefEbsVolumeType) < 1 {
		return errors.New("Invalid or empty aws chef_ebs_volume_type")
	} else if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeIops) < 1 {
		return errors.New("Invalid or empty aws elasticsearch_ebs_volume_iops")
	} else if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeSize) < 1 {
		return errors.New("Invalid or empty aws elasticsearch_ebs_volume_size")
	} else if len(awsConfig.Aws.Config.ElasticsearchEbsVolumeType) < 1 {
		return errors.New("Invalid or empty aws elasticsearch_ebs_volume_type")
	} else if len(awsConfig.Aws.Config.PostgresqlEbsVolumeIops) < 1 {
		return errors.New("Invalid or empty aws postgresql_ebs_volume_iops")
	} else if len(awsConfig.Aws.Config.PostgresqlEbsVolumeSize) < 1 {
		return errors.New("Invalid or empty aws postgresql_ebs_volume_size")
	} else if len(awsConfig.Aws.Config.PostgresqlEbsVolumeType) < 1 {
		return errors.New("Invalid or empty aws postgresql_ebs_volume_type")
	} else {
		return nil
	}
}

func validateExistingInfraFields(exitingInfra ExistingInfraConfigToml) error {
	if len(exitingInfra.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		return errors.New("Invalid or empty secrets_key_file")
	} else if len(exitingInfra.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		return errors.New("Invalid or empty secrets_store_file")
	} else if len(exitingInfra.Architecture.ConfigInitials.Architecture) < 1 {
		return errors.New("Invalid or empty Architecture")
	} else if len(exitingInfra.Architecture.ConfigInitials.WorkspacePath) < 1 {
		return errors.New("Invalid or empty workspace_path")
	} else if len(exitingInfra.Architecture.ConfigInitials.SSHUser) < 1 {
		return errors.New("Invalid or empty ssh_user")
	} else if len(exitingInfra.Architecture.ConfigInitials.SSHUser) < 1 {
		return errors.New("Invalid or empty ssh_user")
	} else if len(exitingInfra.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		return errors.New("Invalid or empty ssh_key_file")
	} else if len(exitingInfra.Architecture.ConfigInitials.BackupMount) < 1 {
		return errors.New("Invalid or empty backup_mount")
	} else if len(exitingInfra.Architecture.ConfigInitials.HabitatUIDGid) < 1 {
		return errors.New("Invalid or empty habitat_uid_gid")
	} else if len(exitingInfra.Automate.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty automate instance_count")
	} else if len(exitingInfra.ChefServer.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty chef-server instance_count")
	} else if len(exitingInfra.Elasticsearch.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty elastic-search instance_count")
	} else if len(exitingInfra.Postgresql.Config.InstanceCount) < 1 {
		return errors.New("Invalid or empty postgres-sql instance_count")
	} else if len(exitingInfra.ExistingInfra.Config.AutomateIps) < 1 {
		return errors.New("Invalid or empty automate Ips")
	} else if len(exitingInfra.ExistingInfra.Config.AutomatePrivateIps) < 1 {
		return errors.New("Invalid or empty automate_private_ips")
	} else if len(exitingInfra.ExistingInfra.Config.ChefServerIps) < 1 {
		return errors.New("Invalid or empty chef_server_ips")
	} else if len(exitingInfra.ExistingInfra.Config.ChefServerPrivateIps) < 1 {
		return errors.New("Invalid or empty chef_server_private_ips")
	} else if len(exitingInfra.ExistingInfra.Config.ElasticsearchIps) < 1 {
		return errors.New("Invalid or empty elasticsearch_ips")
	} else if len(exitingInfra.ExistingInfra.Config.ElasticsearchPrivateIps) < 1 {
		return errors.New("Invalid or empty elasticsearch_private_ips")
	} else if len(exitingInfra.ExistingInfra.Config.PostgresqlIps) < 1 {
		return errors.New("Invalid or empty postgresql_ips")
	} else if len(exitingInfra.ExistingInfra.Config.PostgresqlPrivateIps) < 1 {
		return errors.New("Invalid or empty postgresql_private_ips")
	} else {
		for index, element := range exitingInfra.ExistingInfra.Config.AutomateIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("Automate Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.AutomatePrivateIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("Automate private Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.ChefServerIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("chef server Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.ChefServerPrivateIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("chef server private Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.ElasticsearchIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("elastic search Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.ElasticsearchPrivateIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("elastic search private Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.PostgresqlIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("postgresql Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}

		for index, element := range exitingInfra.ExistingInfra.Config.PostgresqlPrivateIps {
			if checkIPAddress(element.(string)) != nil {
				return errors.New("Postgresql private Ip " + element.(string) + "is not valid at index : " + string(index))
			}
		}
		return nil
	}
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("Ip Address is invalid.")
	} else {
		return nil
	}
}
