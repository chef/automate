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

type Commons struct {
	SecretsKeyFile   string
	SecretsStoreFile string
	Architecture     string
	WorkspacePath    string
	SshUser          string
	SshKeyFile       string
	SudoPassword     string
	BackupMount      string
}

type Automate struct {
	AutomateAdminPassword string
	AutomateFQDN          string
	AutomateTeamsPort     string
	AutomateInstanceCount string
	AutomateConfigFile    string
}

type InitConfigHAFlags struct {
	ArchitectureCommons               *Commons
	AutomateConfig                    *Automate
	ChefServerInstanceCount           string `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount        string `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount           string `toml:"postgresql.config.instance_count"`
	AwsProfile                        string `toml:"aws.config.profile"`
	AwsRegion                         string `toml:"aws.config.region"`
	AmiFilterName                     string `toml:"ami_filter_name"`
	AmiFilterVirtType                 string `toml:"ami_filter_virt_type"`
	AmiFilterOwner                    string `toml:"ami_filter_owner"`
	AmiId                             string `toml:"ami_id"`
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
}

type InitConfigHAExistingInfraFlags struct {
	ArchitectureCommons                  *Commons
	AutomateConfig                       *Automate
	ChefServerInstanceCount              string   `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount           string   `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount              string   `toml:"postgresql.config.instance_count"`
	ExistingInfraAutomateIPs             []string `toml:"existing_infra.config.automate_ips"`
	ExistingInfraAutomatePrivateIPs      []string `toml:"existing_infra.config.chef_server_ips"`
	ExistingInfraChefServerIPs           []string `toml:"existing_infra.config.chef_server_ips"`
	ExistingInfraChefServerPrivateIPs    []string `toml:"existing_infra.config.chef_server_private_ips"`
	ExistingInfraElasticsearchIPs        []string `toml:"existing_infra.config.elasticsearch_ips"`
	ExistingInfraElasticsearchPrivateIPs []string `toml:"existing_infra.config.elasticsearch_private_ips"`
	ExistingInfraPostgresqlIPs           []string `toml:"existing_infra.config.postgresql_ips"`
	ExistingInfraPostgresqlPrivateIps    []string `toml:"existing_infra.config.postgresql_private_ips"`
}

func init() {
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAPathFlags.path,
		"file",
		"config.toml",
		"File path to write the config")
	initConfigHACmd.SetUsageTemplate(UsageTemplate)
	RootCmd.AddCommand(initConfigHACmd)
}

var initConfigHACmd = &cobra.Command{
	Use:   "init-config-ha",
	Short: "Initialize default config for HA",
	Long:  "Initialized default configuration for HA and save it to a file.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runInitConfigHACmd,
}

func runInitConfigHACmd(cmd *cobra.Command, args []string) error {
	if len(args) == 0 {
		msg := "one argument expected, please refer help doc."
		writer.Printf("%s\n\n%s\n", msg, UsageTemplate)
		return nil
	} else if args[0] == "aws" {
		writer.Printf("Generating initial automate high availability configuration for AWS deployment\n")
		return runInitConfigAwsHACmd()
	} else if args[0] == "deploy" {
		return readConfigAndWriteToFile()
	} else if args[0] == "existing_infra" {
		writer.Printf("Generating initial automate high availability configuration for existing infra nodes deployment\n")
		return runInitConfigExistingNodeHACmd()
	} else {
		msg := "Incorrect argument, please refer help doc."
		return status.Wrap(errors.New(msg), status.ConfigError, UsageTemplate)
	}
}
