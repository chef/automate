// Copyright © 2017 Chef Software

package main

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var initConfigHAFlags = struct {
	path                                 string
	SecretsKeyFile                       string
	SecretsStoreFile                     string
	Architecture                         string
	WorkspacePath                        string
	SshUser                              string
	SshKeyFile                           string
	BackupMount                          string
	AutomateInstanceCount                string
	AutomateConfigFile                   string
	ChefServerInstanceCount              string
	ElasticSearchInstanceCount           string
	PostgresqlInstanceCount              string
	AwsProfile                           string
	AwsRegion                            string
	AwsSshKeyPairName                    string
	AwsAutomateServerInstaceType         string
	AwsChefServerInstanceType            string
	AwsElasticSearchServerInstaceType    string
	AwsPostgresqlServerInstanceType      string
	AwsAutomateLBCertificateARN          string
	AwsChefServerLBCertificateARN        string
	AwsAutomateEbsVolumeIops             string
	AwsAutomateEbsVolumeSize             string
	AwsAutomateEbsVolumeType             string
	AwsChefEbsVolumeIops                 string
	AwsChefEbsVolumeSize                 string
	AwsChefEbsVolumeType                 string
	AwsEsEbsVolumeIops                   string
	AwsEsEbsVolumeSize                   string
	AwsEsEbsVolumeType                   string
	AwsPgsEbsVolumeIops                  string
	AwsPgsEbsVolumeSize                  string
	AwsPgsEbsVolumeType                  string
	ExistingNodesAutomateIPs             string
	ExistingNodesAutomatePrivateIPs      string
	ExistingNodesChefServerPrivateIPs    string
	ExistingNodesElasticsearchIPs        string
	ExistingNodesElasticsearchPrivateIPs string
	ExistingNodesPostgresqlIPs           string
	ExistingNodesPostgresqlPrivateIps    string
}{}

func init() {
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.path,
		"file",
		"config.toml",
		"File path to write the config")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.SecretsKeyFile,
		"secrets_key_file",
		"/etc/chef-automate/secrets.key",
		"automate secret keys")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.SecretsStoreFile,
		"secrets_store_file",
		"secrets.json",
		"automate store file keys")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.Architecture,
		"architecture",
		"aws",
		"deployment architecture")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.WorkspacePath,
		"workspace_path",
		"/src",
		"workspace path")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.SshUser,
		"ssh_user",
		"centos",
		"ssh user name default centos")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.SshKeyFile,
		"ssh_key_file",
		"/src/a2ha-jay-singapore.pem",
		"ssh pem file path")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.BackupMount,
		"backup_mount",
		"/mnt/automate_backups",
		"backup mount path")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AutomateInstanceCount,
		"instance_count",
		"1",
		"No of instances need to be up for automate")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AutomateConfigFile,
		"config_file",
		"configs/automate.toml",
		"Path for automate configuration file")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.ChefServerInstanceCount,
		"instance_count_chef",
		"1",
		"No of instances need to be up for chef server")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.ElasticSearchInstanceCount,
		"instance_count_es",
		"1",
		"No of instances need to be up for Elastic Search")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.PostgresqlInstanceCount,
		"instance_count_pgsql",
		"1",
		"No of instances need to be up for postgresql")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsProfile,
		"profile",
		"default",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsRegion,
		"region",
		"ap-southeast-1",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsSshKeyPairName,
		"ssh_key_pair_name",
		"a2ha-jay-singapore",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsAutomateServerInstaceType,
		"automate_server_instance_type",
		"t3a.medium",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsChefServerInstanceType,
		"chef_server_instance_type",
		"t3a.medium",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsElasticSearchServerInstaceType,
		"elasticsearch_server_instance_type",
		"m5a.large",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPostgresqlServerInstanceType,
		"postgresql_server_instance_type",
		"t3a.medium",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsAutomateLBCertificateARN,
		"automate_lb_certificate_arn",
		"arn:aws:acm....",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsChefServerLBCertificateARN,
		"chef_server_lb_certificate_arn",
		"arn:aws:acm....",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsAutomateEbsVolumeIops,
		"automate_ebs_volume_iops",
		"100",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsAutomateEbsVolumeSize,
		"automate_ebs_volume_size",
		"50",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsAutomateEbsVolumeType,
		"automate_ebs_volume_type",
		"gp2",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsChefEbsVolumeIops,
		"chef_ebs_volume_iops",
		"100",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsChefEbsVolumeSize,
		"chef_ebs_volume_size",
		"50",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsChefEbsVolumeType,
		"chef_ebs_volume_type",
		"gp2",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsEsEbsVolumeIops,
		"elasticsearch_ebs_volume_iops",
		"100",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsEsEbsVolumeSize,
		"elasticsearch_ebs_volume_size",
		"50",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsEsEbsVolumeType,
		"elasticsearch_ebs_volume_type",
		"gp2",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeIops,
		"postgresql_ebs_volume_iops",
		"100",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeSize,
		"postgresql_ebs_volume_size",
		"50",
		"Aws profile")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"postgresql_ebs_volume_type",
		"gp2",
		"Aws profile")
	RootCmd.AddCommand(initConfigHACmd)
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"automate_ips",
		"[]",
		"Automate cluster public ips addresses")
	RootCmd.AddCommand(initConfigHACmd)
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"automate_private_ips",
		"[]",
		"Automate cluster private ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"chef_server_ips",
		"[]",
		"Chef server cluster public ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"chef_server_private_ips",
		"[]",
		"Chef server cluster private ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"elasticsearch_ips",
		"[]",
		"elastic search cluster public ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"elasticsearch_private_ips",
		"[]",
		"elastic search cluster private ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"postgresql_ips",
		"[]",
		"postgresql search cluster public ips addresses")
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAFlags.AwsPgsEbsVolumeType,
		"postgresql_private_ips",
		"[]",
		"postgresql search cluster private ips addresses")
	RootCmd.AddCommand(initConfigHACmd)
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
