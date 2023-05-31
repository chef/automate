package config

import (
	"container/list"
	"errors"
)

func (c *HaDeployConfig) Verify(configFile string) error {
	errorList := list.New()

	// Check if config file path is empty
	if len(configFile) < 1 {
		errorList.PushBack("config file path is empty")
	}

	// Parse the config file
	haDeployConfig, err := c.Parse(configFile)
	if err != nil {
		errorList.PushBack(err.Error())
	}

	if haDeployConfig.Architecture == nil {
		return errors.New("architecture cannot be nil")
	}

	// Validate ExistingInfra (on prem)
	if haDeployConfig.Architecture.ExistingInfra != nil {
		c.verifyConfigInitials(haDeployConfig.Architecture.ExistingInfra, errorList)
		validateExistingInfraBackupConfig(haDeployConfig, errorList)
		c.verifyExistingInfraSettings(haDeployConfig.ExistingInfra.Config, errorList)

		// on prem aws or self-managed
		if isExternalDb(haDeployConfig.External.Database) {
			c.verifyExternalPgSettings(haDeployConfig.External.Database.PostgreSQL, errorList)
			c.verifyExternalOsSettings(haDeployConfig.External.Database.OpenSearch, errorList)
			c.verifyAwsExternalOsSettings(haDeployConfig.External.Database.OpenSearch.Aws, errorList)
		}
	}

	// Validate Aws
	if haDeployConfig.Architecture.Aws != nil {
		c.verifyConfigInitials(haDeployConfig.Architecture.Aws, errorList)
		validateAwsBackupConfig(haDeployConfig, errorList)
		c.validateAwsSettings(haDeployConfig.Aws.Config, errorList)
	}

	// Validate common fields
	c.verifyAutomateSettings(haDeployConfig.Automate.Config, errorList)
	c.verifyChefServerSettings(haDeployConfig.ChefServer.Config, errorList)
	c.verifyOpensearchSettings(haDeployConfig.Opensearch.Config, errorList)
	c.verifyPostgresqlSettings(haDeployConfig.Postgresql.Config, errorList)

	return getSingleErrorFromList(errorList)

}

func (c *HaDeployConfig) verifyConfigInitials(configInitials *ConfigInitials, errorList *list.List) {
	validateRequiredStringTypeField(configInitials.SecretsKeyFile, "secrets_key_file", errorList)
	validateRequiredStringTypeField(configInitials.SecretsStoreFile, "secrets_store_file", errorList)
	validateRequiredStringTypeField(configInitials.Architecture, "Architecture", errorList)
	validateRequiredStringTypeField(configInitials.WorkspacePath, "workspace_path", errorList)
	validateRequiredStringTypeField(configInitials.SSHUser, "ssh_user", errorList)
	validateRequiredPathField(configInitials.SSHKeyFile, "ssh_key_file", errorList)
	validateStringTypeField(configInitials.SSHGroupName, "ssh_group_name", errorList)
	validateStringTypeField(configInitials.LoggingMonitoringManagement, "logging_monitoring_management", errorList)
	validateStringTypeField(configInitials.HabitatUIDGid, "habitat_uid_gid", errorList)
	validateBackupMount(configInitials.BackupMount, errorList)
}

func (c *HaDeployConfig) verifyObjectStorage(objectStorage *ConfigObjectStorage, errorList *list.List) {
	validateRequiredStringTypeField(objectStorage.AccessKey, "access_key", errorList)
	validateRequiredStringTypeField(objectStorage.SecretKey, "secret_key", errorList)
	validateRequiredStringTypeField(objectStorage.BucketName, "bucket_name", errorList)
	validateRequiredStringTypeField(objectStorage.Endpoint, "endpoint", errorList)
}

func (c *HaDeployConfig) verifyAutomateSettings(automateSettings *ConfigAutomateSettings, errorList *list.List) {

	validateUrl(automateSettings.Fqdn, "automate fqdn", errorList)
	validateAutomateAdminPassword(automateSettings, errorList)
	validateRequiredNumberField(automateSettings.InstanceCount, "automate instance_count", errorList)
	validateRequiredBooleanField(automateSettings.EnableCustomCerts, "automate enable_custom_certs", errorList)
	validateConfigFile(automateSettings.ConfigFile, errorList)
	validateNumberField(automateSettings.TeamsPort, "teams_port", errorList)
	if automateSettings.EnableCustomCerts {
		validateAutomateCerts(automateSettings, errorList)
		validateCertsByIP(*automateSettings.CertsByIP, "automate ip", errorList)
	}
}

func (c *HaDeployConfig) verifyChefServerSettings(chefServerSettings *ConfigSettings, errorList *list.List) {

	validateRequiredNumberField(chefServerSettings.InstanceCount, "automate instance_count", errorList)
	validateRequiredBooleanField(chefServerSettings.EnableCustomCerts, "automate enable_custom_certs", errorList)

	if chefServerSettings.EnableCustomCerts {
		validateChefServerCerts(chefServerSettings, errorList)
		validateCertsByIP(*chefServerSettings.CertsByIP, "chef server ip", errorList)
	}
}

func (c *HaDeployConfig) verifyOpensearchSettings(opensearchSettings *ConfigOpensearchSettings, errorList *list.List) {

	validateRequiredNumberField(opensearchSettings.InstanceCount, "opensearch instance_count", errorList)
	validateRequiredBooleanField(opensearchSettings.EnableCustomCerts, "opensearch enable_custom_certs", errorList)

	if opensearchSettings.EnableCustomCerts {
		validateOpensearchCerts(opensearchSettings, errorList)
		validateCertsByIP(*opensearchSettings.CertsByIP, "opensearch ip", errorList)
	}
}

func (c *HaDeployConfig) verifyPostgresqlSettings(postgresqlSettings *ConfigSettings, errorList *list.List) {
	validateRequiredNumberField(postgresqlSettings.InstanceCount, "postgresql instance_count", errorList)
	validateRequiredBooleanField(postgresqlSettings.EnableCustomCerts, "postgresql enable_custom_certs", errorList)

	if postgresqlSettings.EnableCustomCerts {
		validatePostgresqlCerts(postgresqlSettings, errorList)
		validateCertsByIP(*postgresqlSettings.CertsByIP, "postgresql ip", errorList)
	}
}

func (c *HaDeployConfig) verifyExistingInfraSettings(existingInfraSettings *ConfigExistingInfraSettings, errorList *list.List) {
	// validate automate Ips
	validateRequiredStringListField(existingInfraSettings.AutomatePrivateIps, "automate_private_ips", errorList)
	validateIPList(existingInfraSettings.AutomatePrivateIps, "automate private ip", errorList)

	// validate chef server Ips
	validateRequiredStringListField(existingInfraSettings.ChefServerPrivateIps, "chef_server_private_ips", errorList)
	validateIPList(existingInfraSettings.ChefServerPrivateIps, "chef server private ip", errorList)

	if !isExternalDb(c.External.Database) {
		// validate opensearch Ips
		validateRequiredStringListField(existingInfraSettings.OpensearchPrivateIps, "opensearch_private_ips", errorList)
		validateIPList(existingInfraSettings.OpensearchPrivateIps, "opensearch private ip", errorList)
		// validate postgresql Ips
		validateRequiredStringListField(existingInfraSettings.PostgresqlPrivateIps, "postgresql_private_ips", errorList)
		validateIPList(existingInfraSettings.PostgresqlPrivateIps, "postgresql private ip", errorList)
	}
}

func (c *HaDeployConfig) verifyExternalPgSettings(externalPostgresqlSettings *ExternalPgSettings, errorList *list.List) {
	validateRequiredStringTypeField(externalPostgresqlSettings.DbuserUsername, "dbuser_username", errorList)
	validateRequiredStringTypeField(externalPostgresqlSettings.DbuserPassword, "dbuser_password", errorList)
	validateRequiredStringTypeField(externalPostgresqlSettings.InstanceURL, "instance_url", errorList)
	validateStringTypeField(externalPostgresqlSettings.PostgreSQLCertificate, "postgresql_certificate", errorList)
	// In the case of AWS-managed RDS, it can be nil
	if isExternalDbSelfManaged(c.External.Database) {
		validateRequiredStringTypeField(externalPostgresqlSettings.PostgresqlRootCert, "postgresql_root_cert", errorList)
	}
	validateRequiredStringTypeField(externalPostgresqlSettings.SuperuserPassword, "superuser_password", errorList)
	validateRequiredStringTypeField(externalPostgresqlSettings.SuperuserUsername, "superuser_username", errorList)
}

func (c *HaDeployConfig) verifyExternalOsSettings(externalOpensearchSettings *ExternalOsSettings, errorList *list.List) {
	validateStringTypeField(externalOpensearchSettings.OpensearchCertificate, "opensearch_certificate", errorList)
	validateRequiredStringTypeField(externalOpensearchSettings.OpensearchDomainName, "opensearch_domain_name", errorList)
	validateUrl(externalOpensearchSettings.OpensearchDomainURL, "opensearch_domain_url", errorList)
	// In the case of AWS-managed Opensearch, it can be nil
	if isExternalDbSelfManaged(c.External.Database) {
		validateRequiredStringTypeField(externalOpensearchSettings.OpensearchRootCert, "opensearch_root_cert", errorList)
	}
	validateRequiredStringTypeField(externalOpensearchSettings.OpensearchUserPassword, "opensearch_user_password", errorList)
	validateRequiredStringTypeField(externalOpensearchSettings.OpensearchUsername, "opensearch_username", errorList)
}

func (c *HaDeployConfig) verifyAwsExternalOsSettings(awsExternalOpensearchSettings *AwsExternalOsSettings, errorList *list.List) {
	validateRequiredStringTypeField(awsExternalOpensearchSettings.AwsOsSnapshotRoleArn, "aws_os_snapshot_role_arn", errorList)
	validateRequiredStringTypeField(awsExternalOpensearchSettings.OsSnapshotUserAccessKeyID, "os_snapshot_user_access_key_id", errorList)
	validateRequiredStringTypeField(awsExternalOpensearchSettings.OsSnapshotUserAccessKeySecret, "os_snapshot_user_access_key_secret", errorList)
}

func (c *HaDeployConfig) validateAwsSettings(aws *ConfigAwsSettings, errorList *list.List) {
	validateRequiredStringTypeField(aws.Profile, "aws profile name", errorList)
	validateRequiredStringTypeField(aws.Region, "aws region", errorList)
	validateRequiredStringTypeField(aws.AwsVpcID, "aws aws_vpc_id", errorList)

	if aws.AwsCidrBlockAddr == "" {
		validateRequiredStringListField(aws.PrivateCustomSubnets, "aws private_custom_subnets", errorList)
		validateRequiredStringListField(aws.PublicCustomSubnets, "aws public_custom_subnets", errorList)
	}
	validateRequiredStringTypeField(aws.SSHKeyPairName, "aws ssh_key_pair_name", errorList)
	validateStringBasedBoolean(aws.LbAccessLogs, "aws lb_access_logs", errorList)
	validateRequiredBooleanField(aws.DeleteOnTermination, "aws delete_on_termination", errorList)

	validateRequiredStringTypeField(aws.AutomateServerInstanceType, "aws automate_server_instance_type", errorList)
	validateRequiredStringTypeField(aws.ChefServerInstanceType, "aws chef_server_instance_type", errorList)

	validateRequiredStringTypeField(aws.OpensearchServerInstanceType, "aws opensearch_server_instance_type", errorList)
	validateRequiredStringTypeField(aws.PostgresqlServerInstanceType, "aws postgresql_server_instance_type", errorList)

	validateRequiredStringTypeField(aws.AutomateLbCertificateArn, "aws automate_lb_certificate_arn", errorList)
	validateRequiredStringTypeField(aws.ChefServerLbCertificateArn, "aws chef_server_lb_certificate_arn", errorList)

	validateRequiredNumberField(aws.AutomateEbsVolumeIops, "aws automate_ebs_volume_iops", errorList)
	validateRequiredNumberField(aws.AutomateEbsVolumeSize, "aws automate_ebs_volume_size", errorList)
	validateRequiredStringTypeField(aws.AutomateEbsVolumeType, "aws automate_ebs_volume_type", errorList)

	validateRequiredNumberField(aws.ChefEbsVolumeIops, "aws chef_ebs_volume_iops", errorList)
	validateRequiredNumberField(aws.ChefEbsVolumeSize, "aws chef_ebs_volume_size", errorList)
	validateRequiredStringTypeField(aws.ChefEbsVolumeType, "aws chef_ebs_volume_type", errorList)

	validateRequiredBooleanField(aws.SetupManagedServices, "aws setup_managed_services", errorList)

	if !aws.SetupManagedServices {
		validateOsPgConfig(aws, errorList)
	} else {
		// validate aws managed services
		validateStringTypeField(aws.ManagedOpensearchCertificate, "aws managed_opensearch_certificate", errorList)
		validateRequiredStringTypeField(aws.ManagedOpensearchDomainName, "aws managed_opensearch_domain_name", errorList)
		validateUrl(aws.ManagedOpensearchDomainURL, "aws managed_opensearch_domain_url", errorList)
		validateRequiredStringTypeField(aws.ManagedOpensearchUserPassword, "aws managed_opensearch_user_password", errorList)
		validateRequiredStringTypeField(aws.ManagedOpensearchUsername, "aws managed_opensearch_username", errorList)
		validateStringTypeField(aws.ManagedRdsCertificate, "aws managed_rds_certificate", errorList)
		validateRequiredStringTypeField(aws.ManagedRdsDbuserPassword, "aws managed_rds_dbuser_password", errorList)
		validateRequiredStringTypeField(aws.ManagedRdsDbuserUsername, "aws managed_rds_dbuser_username", errorList)
		validateUrl(aws.ManagedRdsInstanceURL, "aws managed_rds_instance_url", errorList)
		validateRequiredStringTypeField(aws.ManagedRdsSuperuserPassword, "aws managed_rds_superuser_password", errorList)
		validateRequiredStringTypeField(aws.ManagedRdsSuperuserUsername, "aws managed_rds_superuser_username", errorList)
		validateStringTypeField(aws.AwsOsSnapshotRoleArn, "aws aws_os_snapshot_role_arn", errorList)
		validateStringTypeField(aws.OsSnapshotUserAccessKeyID, "aws os_snapshot_user_access_key_id", errorList)
		validateStringTypeField(aws.OsSnapshotUserAccessKeySecret, "aws os_snapshot_user_access_key_secret", errorList)
		validateOsPgConfig(aws, errorList)
	}
}

func validateOsPgConfig(aws *ConfigAwsSettings, errorList *list.List) {
	validateRequiredNumberField(aws.OpensearchEbsVolumeIops, "aws opensearch_ebs_volume_iops", errorList)
	validateRequiredNumberField(aws.OpensearchEbsVolumeSize, "aws opensearch_ebs_volume_size", errorList)
	validateRequiredStringTypeField(aws.OpensearchEbsVolumeType, "aws opensearch_ebs_volume_type", errorList)
	validateRequiredNumberField(aws.PostgresqlEbsVolumeIops, "aws postgresql_ebs_volume_iops", errorList)
	validateRequiredNumberField(aws.PostgresqlEbsVolumeSize, "aws postgresql_ebs_volume_size", errorList)
	validateRequiredStringTypeField(aws.PostgresqlEbsVolumeType, "aws postgresql_ebs_volume_type", errorList)
}
