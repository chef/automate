// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"fmt"
	"html/template"
	"io/ioutil"
	"strings"

	ptoml "github.com/pelletier/go-toml"
)

func readConfigAndWriteToFile() error {
	//fmt.Printf("reading configs from toml file")
	initConfigHAPath := initConfigHAPathFlags.path
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		fmt.Println(err.Error())
	}
	architectureAws := config.Get("architecture.aws")
	if architectureAws != nil {
		generateAWSA2HARBFile(config, architectureAws)
	} else {
		generateExistingInfraA2HARBFile(config)
	}
	return nil
}

func writeToA2HARBFile(template string, path string) {
	err := ioutil.WriteFile(path, []byte(template), 0600)
	if err != nil {
		writer.Printf("Writing into A2HA.rb file failed")
	}
	writer.Printf("Config written to %s", path)
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

func convertStructArrayToStringArray(data []interface{}) []string {
	dataArray := make([]string, len(data))
	for i := range dataArray {
		dataArray[i] = data[i].(string)
	}
	return dataArray
}

func setCommons(data *ptoml.Tree) *Commons {
	var initFlags *Commons = &Commons{}
	if data.Get("secrets_key_file") != nil {
		initFlags.SecretsKeyFile = data.Get("secrets_key_file").(string)
	}
	if data.Get("secrets_store_file") != nil {
		initFlags.SecretsStoreFile = data.Get("secrets_store_file").(string)
	}
	if data.Get("architecture") != nil {
		initFlags.Architecture = data.Get("architecture").(string)
	}
	if data.Get("workspace_path") != nil {
		initFlags.WorkspacePath = data.Get("workspace_path").(string)
	}
	if data.Get("ssh_user") != nil {
		initFlags.SshUser = data.Get("ssh_user").(string)
	}
	if data.Get("ssh_key_file") != nil {
		initFlags.SshKeyFile = data.Get("ssh_key_file").(string)
	}
	if data.Get("sudo_password") != nil {
		initFlags.SudoPassword = data.Get("sudo_password").(string)
	}
	if data.Get("backup_mount") != nil {
		initFlags.BackupMount = data.Get("backup_mount").(string)
	}
	fmt.Println(initFlags)
	return initFlags
}

func setAutomateConfig(data *ptoml.Tree) *Automate {
	var initFlags *Automate = &Automate{}
	if data.Get("admin_password") != nil {
		initFlags.AutomateAdminPassword = data.Get("admin_password").(string)
	}
	if data.Get("fqdn") != nil {
		initFlags.AutomateFQDN = data.Get("fqdn").(string)
	}
	if data.Get("AutomateTeamsPort") != nil {
		initFlags.AutomateTeamsPort = data.Get("AutomateTeamsPort").(string)
	}
	if data.Get("instance_count") != nil {
		initFlags.AutomateInstanceCount = data.Get("instance_count").(string)
	}
	if data.Get("config_file") != nil {
		initFlags.AutomateConfigFile = data.Get("config_file").(string)
	}
	return initFlags
}

func generateAWSA2HARBFile(config *ptoml.Tree, architectureAws interface{}) {
	var initConfigHAFlags *InitConfigHAFlags = &InitConfigHAFlags{}
	architectureAwsData := architectureAws.(*ptoml.Tree)
	initConfigHAFlags.ArchitectureCommons = setCommons(architectureAwsData)
	automateConfig := config.Get("automate.config").(*ptoml.Tree)
	initConfigHAFlags.AutomateConfig = setAutomateConfig(automateConfig)
	if config.Get("chef_server.config.instance_count") != nil {
		initConfigHAFlags.ChefServerInstanceCount = config.Get("chef_server.config.instance_count").(string)
	}
	if config.Get("elasticsearch.config.instance_count") != nil {
		initConfigHAFlags.ElasticSearchInstanceCount = config.Get("elasticsearch.config.instance_count").(string)
	}
	if config.Get("postgresql.config.instance_count") != nil {
		initConfigHAFlags.PostgresqlInstanceCount = config.Get("postgresql.config.instance_count").(string)
	}
	awsConfig := config.Get("aws.config").(*ptoml.Tree)
	if awsConfig.Get("profile") != nil {
		initConfigHAFlags.AwsProfile = awsConfig.Get("profile").(string)
	}
	if awsConfig.Get("region") != nil {
		initConfigHAFlags.AwsRegion = awsConfig.Get("region").(string)
	}
	if awsConfig.Get("ami_filter_name") != nil {
		initConfigHAFlags.AmiFilterName = awsConfig.Get("ami_filter_name").(string)
	}
	if awsConfig.Get("ami_filter_virt_type") != nil {
		initConfigHAFlags.AmiFilterVirtType = awsConfig.Get("ami_filter_virt_type").(string)
	}
	if awsConfig.Get("ami_filter_owner") != nil {
		initConfigHAFlags.AmiFilterOwner = awsConfig.Get("ami_filter_owner").(string)
	}
	if awsConfig.Get("ami_id") != nil {
		initConfigHAFlags.AmiId = awsConfig.Get("ami_id").(string)
	}
	if awsConfig.Get("ssh_key_pair_name") != nil {
		initConfigHAFlags.AwsSshKeyPairName = awsConfig.Get("ssh_key_pair_name").(string)
	}
	if awsConfig.Get("automate_server_instance_type") != nil {
		initConfigHAFlags.AwsAutomateServerInstaceType = awsConfig.Get("automate_server_instance_type").(string)
	}
	if awsConfig.Get("chef_server_instance_type") != nil {
		initConfigHAFlags.AwsChefServerInstanceType = awsConfig.Get("chef_server_instance_type").(string)
	}
	if awsConfig.Get("elasticsearch_server_instance_type") != nil {
		initConfigHAFlags.AwsElasticSearchServerInstaceType = awsConfig.Get("elasticsearch_server_instance_type").(string)
	}
	if awsConfig.Get("postgresql_server_instance_type") != nil {
		initConfigHAFlags.AwsPostgresqlServerInstanceType = awsConfig.Get("postgresql_server_instance_type").(string)
	}
	if awsConfig.Get("automate_lb_certificate_arn") != nil {
		initConfigHAFlags.AwsAutomateLBCertificateARN = awsConfig.Get("automate_lb_certificate_arn").(string)
	}
	if awsConfig.Get("chef_server_lb_certificate_arn") != nil {
		initConfigHAFlags.AwsChefServerLBCertificateARN = awsConfig.Get("chef_server_lb_certificate_arn").(string)
	}
	if awsConfig.Get("automate_ebs_volume_iops") != nil {
		initConfigHAFlags.AwsAutomateEbsVolumeIops = awsConfig.Get("automate_ebs_volume_iops").(string)
	}
	if awsConfig.Get("automate_ebs_volume_size") != nil {
		initConfigHAFlags.AwsAutomateEbsVolumeSize = awsConfig.Get("automate_ebs_volume_size").(string)
	}
	if awsConfig.Get("automate_ebs_volume_type") != nil {
		initConfigHAFlags.AwsAutomateEbsVolumeType = awsConfig.Get("automate_ebs_volume_type").(string)
	}
	if awsConfig.Get("chef_ebs_volume_iops") != nil {
		initConfigHAFlags.AwsChefEbsVolumeIops = awsConfig.Get("chef_ebs_volume_iops").(string)
	}
	if awsConfig.Get("chef_ebs_volume_size") != nil {
		initConfigHAFlags.AwsChefEbsVolumeSize = awsConfig.Get("chef_ebs_volume_size").(string)
	}
	if awsConfig.Get("chef_ebs_volume_type") != nil {
		initConfigHAFlags.AwsChefEbsVolumeType = awsConfig.Get("chef_ebs_volume_type").(string)
	}
	if awsConfig.Get("elasticsearch_ebs_volume_iops") != nil {
		initConfigHAFlags.AwsEsEbsVolumeIops = awsConfig.Get("elasticsearch_ebs_volume_iops").(string)
	}
	if awsConfig.Get("elasticsearch_ebs_volume_size") != nil {
		initConfigHAFlags.AwsEsEbsVolumeSize = awsConfig.Get("elasticsearch_ebs_volume_size").(string)
	}
	if awsConfig.Get("elasticsearch_ebs_volume_type") != nil {
		initConfigHAFlags.AwsEsEbsVolumeType = awsConfig.Get("elasticsearch_ebs_volume_type").(string)
	}
	if awsConfig.Get("postgresql_ebs_volume_iops") != nil {
		initConfigHAFlags.AwsPgsEbsVolumeIops = awsConfig.Get("postgresql_ebs_volume_iops").(string)
	}
	if awsConfig.Get("postgresql_ebs_volume_size") != nil {
		initConfigHAFlags.AwsPgsEbsVolumeSize = awsConfig.Get("postgresql_ebs_volume_size").(string)
	}
	if awsConfig.Get("postgresql_ebs_volume_type") != nil {
		initConfigHAFlags.AwsPgsEbsVolumeType = awsConfig.Get("postgresql_ebs_volume_type").(string)
	}
	if awsConfig.Get("X-Contact") != nil {
		initConfigHAFlags.AwsTagContact = awsConfig.Get("X-Contact").(string)
	}
	if awsConfig.Get("X-Dept") != nil {
		initConfigHAFlags.AwsTagDept = awsConfig.Get("X-Dept").(string)
	}
	if awsConfig.Get("X-Project") != nil {
		initConfigHAFlags.AwsTagProject = awsConfig.Get("X-Project").(string)
	}
	finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, initConfigHAFlags)
	writeToA2HARBFile(finalTemplate, "a2ha.rb")
}

func generateExistingInfraA2HARBFile(config *ptoml.Tree) {
	var initConfigHAExistingInfraFlags *InitConfigHAExistingInfraFlags = &InitConfigHAExistingInfraFlags{}
	architectureExistingInfra := config.Get("architecture.existing_infra").(*ptoml.Tree)
	initConfigHAExistingInfraFlags.ArchitectureCommons = setCommons(architectureExistingInfra)
	automateConfig := config.Get("automate.config").(*ptoml.Tree)
	initConfigHAExistingInfraFlags.AutomateConfig = setAutomateConfig(automateConfig)
	if config.Get("chef_server.config.instance_count") != nil {
		initConfigHAExistingInfraFlags.ChefServerInstanceCount = config.Get("chef_server.config.instance_count").(string)
	}
	if config.Get("elasticsearch.config.instance_count") != nil {
		initConfigHAExistingInfraFlags.ElasticSearchInstanceCount = config.Get("elasticsearch.config.instance_count").(string)
	}
	if config.Get("postgresql.config.instance_count") != nil {
		initConfigHAExistingInfraFlags.PostgresqlInstanceCount = config.Get("postgresql.config.instance_count").(string)
	}
	existingNodesConfig := config.Get("existing_infra.config").(*ptoml.Tree)
	if existingNodesConfig.Get("automate_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraAutomateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("automate_ips").([]interface{}))
	}
	if existingNodesConfig.Get("automate_private_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraAutomatePrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("automate_private_ips").([]interface{}))
	}
	if existingNodesConfig.Get("chef_server_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraChefServerIPs = convertStructArrayToStringArray(existingNodesConfig.Get("chef_server_ips").([]interface{}))
	}
	if existingNodesConfig.Get("chef_server_private_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraChefServerPrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("chef_server_private_ips").([]interface{}))
	}
	if existingNodesConfig.Get("elasticsearch_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraElasticsearchIPs = convertStructArrayToStringArray(existingNodesConfig.Get("elasticsearch_ips").([]interface{}))
	}
	if existingNodesConfig.Get("elasticsearch_private_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraElasticsearchPrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("elasticsearch_private_ips").([]interface{}))
	}
	if existingNodesConfig.Get("postgresql_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraPostgresqlIPs = convertStructArrayToStringArray(existingNodesConfig.Get("postgresql_ips").([]interface{}))
	}
	if existingNodesConfig.Get("postgresql_private_ips") != nil {
		initConfigHAExistingInfraFlags.ExistingInfraPostgresqlPrivateIps = convertStructArrayToStringArray(existingNodesConfig.Get("postgresql_private_ips").([]interface{}))
	}
	finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, initConfigHAExistingInfraFlags)
	writeToA2HARBFile(finalTemplate, "a2ha.rb")
}
