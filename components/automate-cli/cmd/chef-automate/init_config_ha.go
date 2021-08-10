// Copyright © 2017 Chef Software

package main

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var initConfigHAPathFlags = struct {
	path string
}{}

var initConfigHAFlags = struct {
	SecretsKeyFile                    string `toml:"architecture.aws.secrets_key_file"`
	SecretsStoreFile                  string `toml:"architecture.aws.secrets_store_file"`
	Architecture                      string `toml:"architecture.aws.architecture"`
	WorkspacePath                     string `toml:"architecture.aws.workspace_path"`
	SshUser                           string `toml:"architecture.aws.ssh_user"`
	SshKeyFile                        string `toml:"architecture.aws.ssh_key_file"`
	SudoPassword                      string `toml:"architecture.aws.sudo_password"`
	BackupMount                       string `toml:"architecture.aws.backup_mount"`
	AutomateAdminPassword             string `toml:"automate.config.admin_password"`
	AutomateFQDN                      string `toml:"automate.config.fqdn"`
	AutomateTeamsPort                 string `toml:"automate.config.AutomateTeamsPort"`
	AutomateInstanceCount             string `toml:"automate.config.instance_count"`
	AutomateConfigFile                string `toml:"automate.config.config_file"`
	ChefServerInstanceCount           string `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount        string `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount           string `toml:"postgresql.config.instance_count"`
	AwsProfile                        string `toml:"aws.config.profile"`
	AwsRegion                         string `toml:"aws.config.region"`
	AwsSshKeyPairName                 string `toml:"aws.config.ssh_key_pair_name"`
	AwsAutomateServerInstaceType      string `toml:"aws.config.automate_server_instance_type"`
	AwsChefServerInstanceType         string `toml:"aws.config.chef_server_instance_type"`
	AwsElasticSearchServerInstaceType string `toml:"aws.config.elasticsearch_server_instance_type"`
	AwsPostgresqlServerInstanceType   string `toml:"aws.config.postgresql_server_instance_type"`
	AwsAutomateLBCertificateARN       string `toml:"aws.config.automate_lb_certificate_arn"`
	AwsChefServerLBCertificateARN     string `toml:"aws.config.chef_server_lb_certificate_arn"`
	AwsAutomateEbsVolumeIops          string `toml:"aws.config.automate_ebs_volume_iops"`
	AwsAutomateEbsVolumeSize          string `toml:"aws.config.automate_ebs_volume_size"`
	AwsAutomateEbsVolumeType          string `toml:"aws.config.automate_ebs_volume_type"`
	AwsChefEbsVolumeIops              string `toml:"aws.config.chef_ebs_volume_iops"`
	AwsChefEbsVolumeSize              string `toml:"aws.config.chef_ebs_volume_size"`
	AwsChefEbsVolumeType              string `toml:"aws.config.chef_ebs_volume_type"`
	AwsEsEbsVolumeIops                string `toml:"aws.config.elasticsearch_ebs_volume_iops"`
	AwsEsEbsVolumeSize                string `toml:"aws.config.elasticsearch_ebs_volume_size"`
	AwsEsEbsVolumeType                string `toml:"aws.config.elasticsearch_ebs_volume_type"`
	AwsPgsEbsVolumeIops               string `toml:"aws.config.postgresql_ebs_volume_iops"`
	AwsPgsEbsVolumeSize               string `toml:"aws.config.postgresql_ebs_volume_size"`
	AwsPgsEbsVolumeType               string `toml:"aws.config.postgresql_ebs_volume_type"`
	AwsTagContact                     string `toml:"aws.config.X-Contact"`
	AwsTagDept                        string `toml:"aws.config.X-Dept"`
	AwsTagProject                     string `toml:"aws.config.X-Project"`
}{}

var initConfigHAExistingNodesFlags = struct {
	SecretsKeyFile                       string `toml:"architecture.existing_nodes.secrets_key_file"`
	SecretsStoreFile                     string `toml:"architecture.existing_nodes.secrets_store_file"`
	Architecture                         string `toml:"architecture.existing_nodes.architecture"`
	WorkspacePath                        string `toml:"architecture.existing_nodes.workspace_path"`
	SshUser                              string `toml:"architecture.existing_nodes.ssh_user"`
	SshKeyFile                           string `toml:"architecture.existing_nodes.ssh_key_file"`
	SudoPassword                         string `toml:"architecture.existing_nodes.sudo_password"`
	BackupMount                          string `toml:"architecture.existing_nodes.backup_mount"`
	AutomateAdminPassword                string `toml:"automate.config.admin_password"`
	AutomateFQDN                         string `toml:"automate.config.fqdn"`
	AutomateTeamsPort                    string `toml:"automate.config.AutomateTeamsPort"`
	AutomateInstanceCount                string `toml:"automate.config.instance_count"`
	AutomateConfigFile                   string `toml:"automate.config.config_file"`
	ChefServerInstanceCount              string `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount           string `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount              string `toml:"postgresql.config.instance_count"`
	ExistingNodesAutomateIPs             string `toml:"existing_nodes.config.automate_ips"`
	ExistingNodesAutomatePrivateIPs      string `toml:"existing_nodes.config.chef_server_ips"`
	ExistingNodesChefServerPrivateIPs    string `toml:"existing_nodes.config.chef_server_private_ips"`
	ExistingNodesElasticsearchIPs        string `toml:"existing_nodes.config.elasticsearch_ips"`
	ExistingNodesElasticsearchPrivateIPs string `toml:"existing_nodes.config.elasticsearch_private_ips"`
	ExistingNodesPostgresqlIPs           string `toml:"existing_nodes.config.postgresql_ips"`
	ExistingNodesPostgresqlPrivateIps    string `toml:"existing_nodes.config.postgresql_private_ips"`
}{}

func init() {
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAPathFlags.path,
		"file",
		"config.toml",
		"File path to write the config")
}

var initConfigHACmd = &cobra.Command{
	Use:   "init-config-ha",
	Short: "Initialize default config for HA",
	Long:  "Initializd default configuration for HA and save it to a file.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runInitConfigHACmd,
}

func runInitConfigHACmd(cmd *cobra.Command, args []string) error {
	if len(args) == 0 {
		msg := "one argument expected as any of deplyment mode like aws or existing_node. "
		writer.Printf(msg)
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	} else if args[0] == "aws" {
		writer.Printf("Generating initial automate high availablity configuration for AWS deployment")
		return runInitConfigAwsHACmd()
	} else if args[0] == "existing_node" {
		writer.Printf("Generating initial automate high availablity configuration for existing infra nodes deployment")
		return runInitConfigExistingNodeHACmd()
	} else {
		msg := "Incorrect argument expected is any of deplyment mode like aws or existing_node. "
		writer.Printf(msg)
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	}
}
