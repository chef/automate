// Copyright © 2017 Chef Software

package main

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var initConfigHAPathFlags = struct {
	path string
}{}

var initConfigHabA2HAPathFlag = struct {
	a2haDirPath string
}{}

type CertByIP struct {
	IP         string `toml:"ip"`
	PrivateKey string `toml:"private_key"`
	PublicKey  string `toml:"public_key"`
	NodesDn    string `toml:"nodes_dn,omitempty"`
}

type AwsConfigToml struct {
	Architecture AwsArchitectureToml `toml:"architecture"`
	Automate     AwsAutomateToml     `toml:"automate"`
	ChefServer   ChefServerToml      `toml:"chef_server"`
	Opensearch   OpensearchToml      `toml:"opensearch"`
	Postgresql   PostgresqlToml      `toml:"postgresql"`
	Aws          AwsToml             `toml:"aws"`
}

type ExistingInfraConfigToml struct {
	Architecture  ExistingInfraArchitectureToml `toml:"architecture"`
	Automate      ExistingInfraAutomateToml     `toml:"automate"`
	ChefServer    ChefServerToml                `toml:"chef_server"`
	Opensearch    OpensearchToml                `toml:"opensearch"`
	Postgresql    PostgresqlToml                `toml:"postgresql"`
	ExistingInfra ExistingInfraToml             `toml:"existing_infra"`
	ExternalDB    ExternalDBToml                `toml:"external"`
	ObjectStorage ObjectStorageToml             `toml:"object_storage"`
}

type AwsToml struct {
	Config ConfigToml `toml:"config"`
}

type ConfigToml struct {
	Profile                      string   `toml:"profile"`
	Region                       string   `toml:"region"`
	AwsVpcId                     string   `toml:"aws_vpc_id"`
	AwsCidrBlockAddr             string   `toml:"aws_cidr_block_addr"`
	PrivateCustomSubnets         []string `toml:"private_custom_subnets"`
	PublicCustomSubnets          []string `toml:"public_custom_subnets"`
	SSHKeyPairName               string   `toml:"ssh_key_pair_name"`
	SetupManagedServices         bool     `toml:"setup_managed_services"`
	OpensearchDomainName         string   `toml:"managed_opensearch_domain_name"`
	OpensearchDomainUrl          string   `toml:"managed_opensearch_domain_url"`
	OpensearchUsername           string   `toml:"managed_opensearch_username"`
	OpensearchUserPassword       string   `toml:"managed_opensearch_user_password"`
	OpensearchCertificate        string   `toml:"managed_opensearch_certificate"`
	AwsOsSnapshotRoleArn         string   `toml:"aws_os_snapshot_role_arn"`
	OsUserAccessKeyId            string   `toml:"os_snapshot_user_access_key_id"`
	OsUserAccessKeySecret        string   `toml:"os_snapshot_user_access_key_secret"`
	RDSInstanceUrl               string   `toml:"managed_rds_instance_url"`
	RDSSuperUserName             string   `toml:"managed_rds_superuser_username"`
	RDSSuperUserPassword         string   `toml:"managed_rds_superuser_password"`
	RDSDBUserName                string   `toml:"managed_rds_dbuser_username"`
	RDSDBUserPassword            string   `toml:"managed_rds_dbuser_password"`
	RDSCertificate               string   `toml:"managed_rds_certificate"`
	AmiFilterName                string   `toml:"ami_filter_name"`
	AmiFilterVirtType            string   `toml:"ami_filter_virt_type"`
	AmiFilterOwner               string   `toml:"ami_filter_owner"`
	AmiID                        string   `toml:"ami_id"`
	LBAccessLogs                 string   `toml:"lb_access_logs"`
	DeleteOnTermination          bool     `toml:"delete_on_termination"`
	AutomateServerInstanceType   string   `toml:"automate_server_instance_type"`
	ChefServerInstanceType       string   `toml:"chef_server_instance_type"`
	OpensearchServerInstanceType string   `toml:"opensearch_server_instance_type"`
	PostgresqlServerInstanceType string   `toml:"postgresql_server_instance_type"`
	AutomateLbCertificateArn     string   `toml:"automate_lb_certificate_arn"`
	ChefServerLbCertificateArn   string   `toml:"chef_server_lb_certificate_arn"`
	AutomateEbsVolumeIops        string   `toml:"automate_ebs_volume_iops"`
	AutomateEbsVolumeSize        string   `toml:"automate_ebs_volume_size"`
	AutomateEbsVolumeType        string   `toml:"automate_ebs_volume_type"`
	ChefEbsVolumeIops            string   `toml:"chef_ebs_volume_iops"`
	ChefEbsVolumeSize            string   `toml:"chef_ebs_volume_size"`
	ChefEbsVolumeType            string   `toml:"chef_ebs_volume_type"`
	OpensearchEbsVolumeIops      string   `toml:"opensearch_ebs_volume_iops"`
	OpensearchEbsVolumeSize      string   `toml:"opensearch_ebs_volume_size"`
	OpensearchEbsVolumeType      string   `toml:"opensearch_ebs_volume_type"`
	PostgresqlEbsVolumeIops      string   `toml:"postgresql_ebs_volume_iops"`
	PostgresqlEbsVolumeSize      string   `toml:"postgresql_ebs_volume_size"`
	PostgresqlEbsVolumeType      string   `toml:"postgresql_ebs_volume_type"`
	XContact                     string   `toml:"X-Contact"`
	XDept                        string   `toml:"X-Dept"`
	XProject                     string   `toml:"X-Project"`
	XProduction                  string   `toml:"X-Production"`
	XCustomer                    string   `toml:"X-Customer"`
	AwsAutomateRoute53Prefix     string   `toml:"aws_automate_route53_prefix"`
	AwsChefServerRoute53Prefix   string   `toml:"aws_chef_server_route53_prefix"`
	AwsRoute53HostedZone         string   `toml:"aws_route53_hosted_zone"`
	PostrgesqlDbIdentifier       string   `toml:"postgresql_db_identifier"`
	ElasticsearchDomainName      string   `toml:"elasticsearch_domain_name"`
	RDSInstanceType              string   `toml:"rds_postgresql_instance_type"`
	RDSRestoreIdentifier         string   `toml:"rds_postgresql_restore_identifier"`
	DatadogAPIKey                string   `toml:"datadog_api_key"`
	UseExistingManagedInfra      bool     `toml:"use_existing_managed_infra"`
}

type AwsArchitectureToml struct {
	ConfigInitials AwsConfigInitialsToml `toml:"aws"`
}

type AwsConfigInitialsToml struct {
	SecretsKeyFile              string `toml:"secrets_key_file"`
	SecretsStoreFile            string `toml:"secrets_store_file"`
	Architecture                string `toml:"architecture"`
	WorkspacePath               string `toml:"workspace_path"`
	SSHUser                     string `toml:"ssh_user"`
	SSHGroupName                string `toml:"ssh_group_name"`
	SSHKeyFile                  string `toml:"ssh_key_file"`
	SSHPort                     string `toml:"ssh_port"`
	LoggingMonitoringManagement string `toml:"logging_monitoring_management"`
	NewElk                      string `toml:"new_elk"`
	ExistingElkInstanceIP       string `toml:"existing_elk_instance_ip"`
	ExistingElkPort             string `toml:"existing_elk_port"`
	ExistingElkCert             string `toml:"existing_elk_cert"`
	ExistingElkUsername         string `toml:"existing_elk_username"`
	ExistingElkPassword         string `toml:"existing_elk_password"`
	BackupMount                 string `toml:"backup_mount"`
	BackupConfig                string `toml:"backup_config"`
	S3BucketName                string `toml:"s3_bucketName"`
	HabitatUIDGid               string `toml:"habitat_uid_gid"`
}

type ExistingInfraArchitectureToml struct {
	ConfigInitials ExistingInfraConfigInitialsToml `toml:"existing_infra"`
}

type ExistingInfraConfigInitialsToml struct {
	SecretsKeyFile              string `toml:"secrets_key_file,omitempty"`
	SecretsStoreFile            string `toml:"secrets_store_file,omitempty"`
	Architecture                string `toml:"architecture,omitempty"`
	WorkspacePath               string `toml:"workspace_path,omitempty"`
	SSHUser                     string `toml:"ssh_user,omitempty"`
	SSHGroupName                string `toml:"ssh_group_name,omitempty"`
	SSHKeyFile                  string `toml:"ssh_key_file,omitempty"`
	SSHPort                     string `toml:"ssh_port,omitempty"`
	LoggingMonitoringManagement string `toml:"logging_monitoring_management,omitempty"`
	NewElk                      string `toml:"new_elk,omitempty"`
	ExistingElkInstanceIP       string `toml:"existing_elk_instance_ip,omitempty"`
	ExistingElkPort             string `toml:"existing_elk_port,omitempty"`
	ExistingElkCert             string `toml:"existing_elk_cert,omitempty"`
	ExistingElkUsername         string `toml:"existing_elk_username,omitempty"`
	ExistingElkPassword         string `toml:"existing_elk_password,omitempty"`
	BackupMount                 string `toml:"backup_mount,omitempty"`
	HabitatUIDGid               string `toml:"habitat_uid_gid,omitempty"`
	BackupConfig                string `toml:"backup_config,omitempty"`
}

type ExistingInfraAutomateToml struct {
	Config ExistingInfraAutomateConfigToml `toml:"config"`
}

type ExistingInfraAutomateConfigToml struct {
	AdminPassword     string     `toml:"admin_password"`
	Fqdn              string     `toml:"fqdn"`
	InstanceCount     string     `toml:"instance_count"`
	TeamsPort         string     `toml:"teams_port,omitempty"`
	ConfigFile        string     `toml:"config_file"`
	EnableCustomCerts bool       `toml:"enable_custom_certs"`
	RootCA            string     `toml:"root_ca"`
	PrivateKey        string     `toml:"private_key"`
	PublicKey         string     `toml:"public_key"`
	CertsByIP         []CertByIP `toml:"certs_by_ip"`
}

type AwsAutomateToml struct {
	Config AwsAutomateConfigToml `toml:"config"`
}

type AwsAutomateConfigToml struct {
	AdminPassword     string     `toml:"admin_password"`
	Fqdn              string     `toml:"fqdn"`
	AutomateSetupType string     `toml:"automate_setup_type"`
	InstanceCount     string     `toml:"instance_count"`
	TeamsPort         string     `toml:"teams_port"`
	ConfigFile        string     `toml:"config_file"`
	EnableCustomCerts bool       `toml:"enable_custom_certs"`
	RootCA            string     `toml:"root_ca"`
	PrivateKey        string     `toml:"private_key"`
	PublicKey         string     `toml:"public_key"`
	CertsByIP         []CertByIP `toml:"certs_by_ip"`
}

type ChefServerToml struct {
	Config ChefServerConfigToml `toml:"config"`
}

type ChefServerConfigToml struct {
	InstanceCount     string     `toml:"instance_count"`
	Fqdn              string     `toml:"fqdn"`
	RootCA            string     `toml:"lb_root_ca"`
	EnableCustomCerts bool       `toml:"enable_custom_certs"`
	PrivateKey        string     `toml:"private_key"`
	PublicKey         string     `toml:"public_key"`
	CertsByIP         []CertByIP `toml:"certs_by_ip"`
}

type OpensearchToml struct {
	Config OsConfigToml `toml:"config"`
}

type OsConfigToml struct {
	InstanceCount     string     `toml:"instance_count"`
	EnableCustomCerts bool       `toml:"enable_custom_certs"`
	RootCA            string     `toml:"root_ca"`
	AdminCert         string     `toml:"admin_cert"`
	AdminKey          string     `toml:"admin_key"`
	PrivateKey        string     `toml:"private_key"`
	PublicKey         string     `toml:"public_key"`
	AdminDn           string     `toml:"admin_dn"`
	NodesDn           string     `toml:"nodes_dn"`
	CertsByIP         []CertByIP `toml:"certs_by_ip"`
}

type PostgresqlToml struct {
	Config PgConfigToml `toml:"config"`
}

type PgConfigToml struct {
	InstanceCount     string     `toml:"instance_count"`
	EnableCustomCerts bool       `toml:"enable_custom_certs"`
	RootCA            string     `toml:"root_ca"`
	PrivateKey        string     `toml:"private_key"`
	PublicKey         string     `toml:"public_key"`
	CertsByIP         []CertByIP `toml:"certs_by_ip"`
}

type ExistingInfraToml struct {
	Config ExistingInfraIpsToml `toml:"config"`
}

type ExistingInfraIpsToml struct {
	AutomatePrivateIps   []string `toml:"automate_private_ips"`
	ChefServerPrivateIps []string `toml:"chef_server_private_ips"`
	OpensearchPrivateIps []string `toml:"opensearch_private_ips"`
	PostgresqlPrivateIps []string `toml:"postgresql_private_ips"`
}

type ExternalDBToml struct {
	Database ExternalDbToml `toml:"database"`
}

type ExternalDbToml struct {
	Type       string                 `toml:"type"`
	PostgreSQL ExternalPostgreSQLToml `toml:"postgre_sql"`
	Opensearch ExternalOpensearchToml `toml:"open_search"`
}

type ExternalPostgreSQLToml struct {
	PostgreSQLInstanceURL       string `toml:"instance_url"`
	PostgreSQLSuperUserName     string `toml:"superuser_username"`
	PostgreSQLSuperUserPassword string `toml:"superuser_password"`
	PostgreSQLDBUserName        string `toml:"dbuser_username"`
	PostgreSQLDBUserPassword    string `toml:"dbuser_password"`
	PostgreSQLRootCert          string `toml:"postgresql_root_cert"`
}

type ExternalOpensearchToml struct {
	OpensearchInstanceURL       string          `toml:"opensearch_domain_url"`
	OpensearchSuperUserName     string          `toml:"opensearch_username"`
	OpensearchSuperUserPassword string          `toml:"opensearch_user_password"`
	OpensearchRootCert          string          `toml:"opensearch_root_cert"`
	OpensearchDomainName        string          `toml:"opensearch_domain_name"`
	AWS                         ExternalAwsToml `toml:"aws"`
}

type ExternalAwsToml struct {
	AwsOsSnapshotRoleArn  string `toml:"aws_os_snapshot_role_arn"`
	OsUserAccessKeyId     string `toml:"os_snapshot_user_access_key_id"`
	OsUserAccessKeySecret string `toml:"os_snapshot_user_access_key_secret"`
}

type ObjectStorageToml struct {
	Config ObjectStorageConfigToml `toml:"config"`
}

type ObjectStorageConfigToml struct {
	BucketName string `toml:"bucket_name"`
	AccessKey  string `toml:"access_key"`
	SecretKey  string `toml:"secret_key"`
	Endpoint   string `toml:"endpoint"`
	Region     string `toml:"region"`
}

func init() {
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAPathFlags.path,
		"file",
		"config.toml",
		"File path to write the config")
	initConfigHACmd.SetUsageTemplate(UsageTemplate)

	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHabA2HAPathFlag.a2haDirPath,
		"path",
		"/hab/a2_deploy_workspace/",
		"a2ha hab workspace dir path")
	initConfigHACmd.SetUsageTemplate(UsageTemplate)
	initConfigHACmd.PersistentFlags().SetAnnotation("path", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(initConfigHACmd)
}

var initConfigHACmd = &cobra.Command{
	Use:   "init-config-ha",
	Short: "Initialize default config for Automate HA",
	Long:  "Initialized default configuration for HA and save it to a file.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
		docs.Compatibility:       docs.CompatiblewithHA,
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
	} else if args[0] == "existing_infra" {
		writer.Printf("Generating initial automate high availability configuration for existing infra nodes deployment\n")
		return runInitConfigExistingNodeHACmd()
	} else {
		msg := "Incorrect argument, please refer help doc."
		return status.Wrap(errors.New(msg), status.ConfigError, UsageTemplate)
	}
}
