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
		writer.Printf("some error occured while rendering template \n %s", err)
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

func generateAWSA2HARBFile(config *ptoml.Tree, architectureAws interface{}) {
	architectureAwsData := architectureAws.(ptoml.Tree)
	initConfigHAFlags.SecretsKeyFile = architectureAwsData.Get("secrets_key_file").(string)
	initConfigHAFlags.SecretsStoreFile = architectureAwsData.Get("secrets_store_file").(string)
	initConfigHAFlags.Architecture = architectureAwsData.Get("architecture").(string)
	initConfigHAFlags.WorkspacePath = architectureAwsData.Get("workspace_path").(string)
	initConfigHAFlags.SshUser = architectureAwsData.Get("ssh_user").(string)
	initConfigHAFlags.SshKeyFile = architectureAwsData.Get("ssh_key_file").(string)
	initConfigHAFlags.SshKeyFile = architectureAwsData.Get("sudo_password").(string)
	initConfigHAFlags.BackupMount = architectureAwsData.Get("backup_mount").(string)
	automateConfig := config.Get("automate.config").(*ptoml.Tree)
	//initConfigHAFlags.AutomateAdminPassword = automateConfig.Get("admin_password").(string)
	//initConfigHAFlags.AutomateFQDN = automateConfig.Get("fqdn").(string)
	//initConfigHAFlags.AutomateTeamsPort = automateConfig.Get("AutomateTeamsPort").(string)
	initConfigHAFlags.AutomateInstanceCount = automateConfig.Get("instance_count").(string)
	initConfigHAFlags.AutomateConfigFile = automateConfig.Get("config_file").(string)
	initConfigHAFlags.AutomateInstanceCount = config.Get("chef_server.config.instance_count").(string)
	initConfigHAFlags.ElasticSearchInstanceCount = config.Get("elasticsearch.config.instance_count").(string)
	initConfigHAFlags.PostgresqlInstanceCount = config.Get("postgresql.config.instance_count").(string)
	awsConfig := config.Get("aws.config").(*ptoml.Tree)
	initConfigHAFlags.AwsProfile = awsConfig.Get("profile").(string)
	initConfigHAFlags.AwsRegion = awsConfig.Get("region").(string)
	initConfigHAFlags.AwsSshKeyPairName = awsConfig.Get("ssh_key_pair_name").(string)
	initConfigHAFlags.AwsAutomateServerInstaceType = awsConfig.Get("automate_server_instance_type").(string)
	initConfigHAFlags.AwsChefServerInstanceType = awsConfig.Get("chef_server_instance_type").(string)
	initConfigHAFlags.AwsElasticSearchServerInstaceType = awsConfig.Get("elasticsearch_server_instance_type").(string)
	initConfigHAFlags.AwsPostgresqlServerInstanceType = awsConfig.Get("postgresql_server_instance_type").(string)
	initConfigHAFlags.AwsAutomateLBCertificateARN = awsConfig.Get("automate_lb_certificate_arn").(string)
	initConfigHAFlags.AwsChefServerLBCertificateARN = awsConfig.Get("chef_server_lb_certificate_arn").(string)
	initConfigHAFlags.AwsAutomateEbsVolumeIops = awsConfig.Get("automate_ebs_volume_iops").(string)
	initConfigHAFlags.AwsAutomateEbsVolumeSize = awsConfig.Get("automate_ebs_volume_size").(string)
	initConfigHAFlags.AwsAutomateEbsVolumeType = awsConfig.Get("automate_ebs_volume_type").(string)
	initConfigHAFlags.AwsChefEbsVolumeIops = awsConfig.Get("chef_ebs_volume_iops").(string)
	initConfigHAFlags.AwsChefEbsVolumeSize = awsConfig.Get("chef_ebs_volume_size").(string)
	initConfigHAFlags.AwsChefEbsVolumeType = awsConfig.Get("chef_ebs_volume_type").(string)
	initConfigHAFlags.AwsEsEbsVolumeIops = awsConfig.Get("elasticsearch_ebs_volume_iops").(string)
	initConfigHAFlags.AwsEsEbsVolumeSize = awsConfig.Get("elasticsearch_ebs_volume_size").(string)
	initConfigHAFlags.AwsEsEbsVolumeType = awsConfig.Get("elasticsearch_ebs_volume_type").(string)
	initConfigHAFlags.AwsEsEbsVolumeType = awsConfig.Get("elasticsearch_ebs_volume_type").(string)
	initConfigHAFlags.AwsPgsEbsVolumeIops = awsConfig.Get("postgresql_ebs_volume_iops").(string)
	initConfigHAFlags.AwsPgsEbsVolumeSize = awsConfig.Get("postgresql_ebs_volume_size").(string)
	initConfigHAFlags.AwsPgsEbsVolumeType = awsConfig.Get("postgresql_ebs_volume_type").(string)
	initConfigHAFlags.AwsTagContact = awsConfig.Get("X-Contact").(string)
	initConfigHAFlags.AwsTagDept = awsConfig.Get("X-Dept").(string)
	initConfigHAFlags.AwsTagProject = awsConfig.Get("X-Project").(string)
	finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, initConfigHAFlags)
	fmt.Println(finalTemplate)
	writeToA2HARBFile(finalTemplate, "a2ha.rb")
}

func generateExistingInfraA2HARBFile(config *ptoml.Tree) {
	architectureExistingNodes := config.Get("architecture.existing_nodes").(*ptoml.Tree)
	initConfigHAExistingNodesFlags.SecretsKeyFile = architectureExistingNodes.Get("secrets_key_file").(string)
	initConfigHAExistingNodesFlags.SecretsStoreFile = architectureExistingNodes.Get("secrets_store_file").(string)
	initConfigHAExistingNodesFlags.Architecture = architectureExistingNodes.Get("architecture").(string)
	initConfigHAExistingNodesFlags.WorkspacePath = architectureExistingNodes.Get("workspace_path").(string)
	initConfigHAExistingNodesFlags.SshUser = architectureExistingNodes.Get("ssh_user").(string)
	initConfigHAExistingNodesFlags.SshKeyFile = architectureExistingNodes.Get("ssh_key_file").(string)
	initConfigHAExistingNodesFlags.SshKeyFile = architectureExistingNodes.Get("sudo_password").(string)
	initConfigHAExistingNodesFlags.BackupMount = architectureExistingNodes.Get("backup_mount").(string)
	automateConfig := config.Get("automate.config").(*ptoml.Tree)
	//initConfigHAExistingNodesFlags.AutomateAdminPassword = automateConfig.Get("admin_password").(string)
	//initConfigHAExistingNodesFlags.AutomateFQDN = automateConfig.Get("fqdn").(string)
	//initConfigHAExistingNodesFlags.AutomateTeamsPort = automateConfig.Get("AutomateTeamsPort").(string)
	initConfigHAExistingNodesFlags.AutomateInstanceCount = automateConfig.Get("instance_count").(string)
	initConfigHAExistingNodesFlags.AutomateConfigFile = automateConfig.Get("config_file").(string)
	initConfigHAExistingNodesFlags.AutomateInstanceCount = config.Get("chef_server.config.instance_count").(string)
	initConfigHAExistingNodesFlags.ElasticSearchInstanceCount = config.Get("elasticsearch.config.instance_count").(string)
	initConfigHAExistingNodesFlags.PostgresqlInstanceCount = config.Get("postgresql.config.instance_count").(string)
	existingNodesConfig := config.Get("existing_nodes.config").(*ptoml.Tree)
	initConfigHAExistingNodesFlags.ExistingNodesAutomateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("automate_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesAutomatePrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("automate_private_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesChefServerIPs = convertStructArrayToStringArray(existingNodesConfig.Get("chef_server_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesChefServerPrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("chef_server_private_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesElasticsearchIPs = convertStructArrayToStringArray(existingNodesConfig.Get("elasticsearch_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesElasticsearchPrivateIPs = convertStructArrayToStringArray(existingNodesConfig.Get("elasticsearch_private_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesPostgresqlIPs = convertStructArrayToStringArray(existingNodesConfig.Get("postgresql_ips").([]interface{}))
	initConfigHAExistingNodesFlags.ExistingNodesPostgresqlPrivateIps = convertStructArrayToStringArray(existingNodesConfig.Get("postgresql_private_ips").([]interface{}))
	finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, initConfigHAExistingNodesFlags)
	fmt.Println(finalTemplate)
	writeToA2HARBFile(finalTemplate, "a2ha.rb")
}
