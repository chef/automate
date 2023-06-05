package config

import (
	"container/list"
	"errors"
)

func (c *HaDeployConfig) Verify() error {
	errorList := list.New()

	if !c.IsValidHaDeployConfig() {
		return errors.New("invalid ha deploy config")
	}

	// Validate ExistingInfra (on prem)
	if c.IsExistingInfra() {
		if err := c.verifyConfigInitials(c.Architecture.ExistingInfra); err != nil {
			errorList.PushBack(err)
		}

		if err := c.validateExistingInfraBackupConfig(); err != nil {
			errorList.PushBack(err)
		}

		if err := c.verifyExistingInfraSettings(c.ExistingInfra.Config); err != nil {
			errorList.PushBack(err)
		}
	}

	// Validate Aws
	if c.IsAws() {
		if err := c.verifyConfigInitials(c.Architecture.Aws); err != nil {
			errorList.PushBack(err)
		}

		if err := c.validateAwsBackupConfig(); err != nil {
			errorList.PushBack(err)
		}

		if err := c.validateAwsSettings(c.Aws.Config); err != nil {
			errorList.PushBack(err)
		}
	}

	// Validate common fields
	if err := c.verifyAutomateSettings(); err != nil {
		errorList.PushBack(err)
	}
	if err := c.verifyChefServerSettings(); err != nil {
		errorList.PushBack(err)
	}
	if err := c.verifyOpensearchSettings(); err != nil {
		errorList.PushBack(err)
	}
	if err := c.verifyPostgresqlSettings(); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) ParseAndVerify(configFile string) error {
	// Check if config file path is empty
	if len(configFile) < 1 {
		return errors.New("config file path is empty")
	}
	// Parse the config file
	err := c.Parse(configFile)
	if err != nil {
		return err
	}
	if err := c.Verify(); err != nil {
		return err
	}
	return nil
}

func (c *HaDeployConfig) verifyConfigInitials(configInitials *ConfigInitials) error {
	errorList := list.New()

	if err := validateRequiredStringTypeField(configInitials.SecretsKeyFile, "secrets_key_file"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(configInitials.SecretsStoreFile, "secrets_store_file"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(configInitials.Architecture, "architecture", "aws", "existing_nodes"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(configInitials.WorkspacePath, "workspace_path", "/hab/a2_deploy_workspace"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(configInitials.SSHUser, "ssh_user"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredPathField(configInitials.SSHKeyFile, "ssh_key_file"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(configInitials.SSHGroupName, "ssh_group_name"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(configInitials.LoggingMonitoringManagement, "logging_monitoring_management"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(configInitials.HabitatUIDGid, "habitat_uid_gid"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(configInitials.BackupMount, "backup_mount"); err != nil {
		errorList.PushBack(err)
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) validateExistingInfraBackupConfig() error {
	// validate existing infra backup config
	backupConfig := c.Architecture.ExistingInfra.BackupConfig
	if backupConfig != "object_storage" && backupConfig != "file_system" {
		return errors.New("invalid or empty backup_config")
	}

	if backupConfig == "object_storage" {
		if err := c.verifyObjectStorage(c.ObjectStorage.Config); err != nil {
			return err
		}
	}

	return nil
}

func (c *HaDeployConfig) validateAwsBackupConfig() error {
	// validate aws backup config
	if err := checkForValidS3Bucket(c); err != nil {
		return err
	}
	if c.Aws.Config.SetupManagedServices {
		if err := validateRequiredStringTypeField(c.Architecture.Aws.BackupConfig, "backup_config", "s3"); err != nil {
			return err
		}
	} else {
		if err := validateRequiredStringTypeField(c.Architecture.Aws.BackupConfig, "backup_config", "s3", "efs"); err != nil {
			return err
		}
	}
	return nil
}

func (c *HaDeployConfig) verifyObjectStorage(objectStorage *ConfigObjectStorage) error {
	errorList := list.New()
	if err := validateRequiredStringTypeField(objectStorage.AccessKey, "access_key"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(objectStorage.SecretKey, "secret_key"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(objectStorage.BucketName, "bucket_name"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(objectStorage.Endpoint, "endpoint"); err != nil {
		errorList.PushBack(err)
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyAutomateSettings() error {
	automateSettings := c.Automate.Config
	errorList := list.New()

	if err := validateUrl(automateSettings.Fqdn, "automate fqdn"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateAutomateAdminPassword(automateSettings); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredNumberField(automateSettings.InstanceCount, "automate instance_count"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredBooleanField(automateSettings.EnableCustomCerts, "automate enable_custom_certs"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(automateSettings.ConfigFile, "config_file", "configs/automate.toml"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateNumberField(automateSettings.TeamsPort, "teams_port"); err != nil {
		errorList.PushBack(err)
	}
	if automateSettings.EnableCustomCerts {
		if err := validateAutomateCerts(automateSettings); err != nil {
			errorList.PushBack(err)
		}
		if err := validateCertsByIP(automateSettings.CertsByIP, "automate ip"); err != nil {
			errorList.PushBack(err)
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyChefServerSettings() error {
	chefServerSettings := c.ChefServer.Config
	errorList := list.New()

	if err := validateRequiredNumberField(chefServerSettings.InstanceCount, "chef server instance_count"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredBooleanField(chefServerSettings.EnableCustomCerts, "chef server enable_custom_certs"); err != nil {
		errorList.PushBack(err)
	}
	if chefServerSettings.EnableCustomCerts {
		if err := validateChefServerCerts(chefServerSettings); err != nil {
			errorList.PushBack(err)
		}
		if err := validateCertsByIP(chefServerSettings.CertsByIP, "chef server ip"); err != nil {
			errorList.PushBack(err)
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyOpensearchSettings() error {
	opensearchSettings := c.Opensearch.Config
	errorList := list.New()

	if err := validateRequiredNumberField(opensearchSettings.InstanceCount, "opensearch instance_count"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredBooleanField(opensearchSettings.EnableCustomCerts, "opensearch enable_custom_certs"); err != nil {
		errorList.PushBack(err)
	}
	if opensearchSettings.EnableCustomCerts {
		if err := validateOpensearchCerts(opensearchSettings); err != nil {
			errorList.PushBack(err)
		}
		if err := validateCertsByIP(opensearchSettings.CertsByIP, "opensearch ip"); err != nil {
			errorList.PushBack(err)
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyPostgresqlSettings() error {
	postgresqlSettings := c.Postgresql.Config
	errorList := list.New()

	if err := validateRequiredNumberField(postgresqlSettings.InstanceCount, "postgresql instance_count"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredBooleanField(postgresqlSettings.EnableCustomCerts, "postgresql enable_custom_certs"); err != nil {
		errorList.PushBack(err)
	}
	if postgresqlSettings.EnableCustomCerts {
		if err := validatePostgresqlCerts(postgresqlSettings); err != nil {
			errorList.PushBack(err)
		}
		if err := validateCertsByIP(postgresqlSettings.CertsByIP, "postgresql ip"); err != nil {
			errorList.PushBack(err)
		}
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExistingInfraSettings(existingInfraSettings *ConfigExistingInfraSettings) error {
	errorList := list.New()

	// validate automate IPs
	if err := validateRequiredStringListField(existingInfraSettings.AutomatePrivateIps, "automate_private_ips"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateIPList(existingInfraSettings.AutomatePrivateIps, "automate private ip"); err != nil {
		errorList.PushBack(err)
	}

	// validate chef server IPs
	if err := validateRequiredStringListField(existingInfraSettings.ChefServerPrivateIps, "chef_server_private_ips"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateIPList(existingInfraSettings.ChefServerPrivateIps, "chef server private ip"); err != nil {
		errorList.PushBack(err)
	}

	if c.IsExternalDb() {
		// validate opensearch IPs
		if err := validateRequiredStringListField(existingInfraSettings.OpensearchPrivateIps, "opensearch_private_ips"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateIPList(existingInfraSettings.OpensearchPrivateIps, "opensearch private ip"); err != nil {
			errorList.PushBack(err)
		}

		// validate postgresql IPs
		if err := validateRequiredStringListField(existingInfraSettings.PostgresqlPrivateIps, "postgresql_private_ips"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateIPList(existingInfraSettings.PostgresqlPrivateIps, "postgresql private ip"); err != nil {
			errorList.PushBack(err)
		}

		// on-prem, AWS, or self-managed
		if err := c.verifyExternalPgSettings(c.External.Database.PostgreSQL); err != nil {
			errorList.PushBack(err)
		}
		if err := c.verifyExternalOsSettings(c.External.Database.OpenSearch); err != nil {
			errorList.PushBack(err)
		}

		if isAwsExternalOsConfigured(c) {
			if err := c.verifyAwsExternalOsSettings(c.External.Database.OpenSearch.Aws); err != nil {
				errorList.PushBack(err)
			}
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExternalPgSettings(externalPostgresqlSettings *ExternalPgSettings) error {
	errorList := list.New()

	err := validateRequiredStringTypeField(externalPostgresqlSettings.DbuserUsername, "dbuser_username")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(externalPostgresqlSettings.DbuserPassword, "dbuser_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(externalPostgresqlSettings.InstanceURL, "instance_url")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateStringTypeField(externalPostgresqlSettings.PostgreSQLCertificate, "postgresql_certificate")
	if err != nil {
		errorList.PushBack(err)
	}

	// In the case of AWS-managed RDS, it can be nil
	if c.IsExternalDbSelfManaged() {
		err = validateRequiredStringTypeField(externalPostgresqlSettings.PostgresqlRootCert, "postgresql_root_cert")
		if err != nil {
			errorList.PushBack(err)
		}
	}

	err = validateRequiredStringTypeField(externalPostgresqlSettings.SuperuserPassword, "superuser_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(externalPostgresqlSettings.SuperuserUsername, "superuser_username")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExternalOsSettings(externalOpensearchSettings *ExternalOsSettings) error {
	errorList := list.New()

	err := validateStringTypeField(externalOpensearchSettings.OpensearchCertificate, "opensearch_certificate")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(externalOpensearchSettings.OpensearchDomainName, "opensearch_domain_name")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateUrl(externalOpensearchSettings.OpensearchDomainURL, "opensearch_domain_url")
	if err != nil {
		errorList.PushBack(err)
	}

	// In the case of AWS-managed Opensearch, it can be nil
	if c.IsExternalDbSelfManaged() {
		err = validateRequiredStringTypeField(externalOpensearchSettings.OpensearchRootCert, "opensearch_root_cert")
		if err != nil {
			errorList.PushBack(err)
		}
	}

	err = validateRequiredStringTypeField(externalOpensearchSettings.OpensearchUserPassword, "opensearch_user_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(externalOpensearchSettings.OpensearchUsername, "opensearch_username")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyAwsExternalOsSettings(awsExternalOpensearchSettings *AwsExternalOsSettings) error {
	errorList := list.New()

	err := validateRequiredStringTypeField(awsExternalOpensearchSettings.AwsOsSnapshotRoleArn, "aws_os_snapshot_role_arn")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(awsExternalOpensearchSettings.OsSnapshotUserAccessKeyID, "os_snapshot_user_access_key_id")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(awsExternalOpensearchSettings.OsSnapshotUserAccessKeySecret, "os_snapshot_user_access_key_secret")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) validateAwsSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateCommonAwsSettings(aws); err != nil {
		errorList.PushBack(err)
	}

	if !aws.SetupManagedServices {
		if err := validateAwsOsPgConfig(aws); err != nil {
			errorList.PushBack(err)
		}
	} else {
		// validate aws managed services
		if err := validateAwsManagedServices(aws); err != nil {
			errorList.PushBack(err)
		}
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) IsValidHaDeployConfig() bool {
	return c != nil && c.Architecture != nil
}

func (c *HaDeployConfig) IsExistingInfra() bool {
	return c.Architecture != nil && c.Architecture.ExistingInfra != nil
}

func (c *HaDeployConfig) IsAws() bool {
	return c.Architecture != nil && c.Architecture.Aws != nil
}

func (c *HaDeployConfig) IsExternalDb() bool {
	return c.External != nil && c.External.Database != nil &&
		(c.External.Database.Type == "aws" || c.External.Database.Type == "self-managed")
}

func (c *HaDeployConfig) IsExternalDbSelfManaged() bool {
	return c.IsExternalDb() && c.External.Database.Type == "self-managed"
}

func validateAwsOsPgConfig(aws *ConfigAwsSettings) error {
	errorList := list.New()

	err := validateRequiredNumberField(aws.OpensearchEbsVolumeIops, "aws opensearch_ebs_volume_iops")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredNumberField(aws.OpensearchEbsVolumeSize, "aws opensearch_ebs_volume_size")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(aws.OpensearchEbsVolumeType, "aws opensearch_ebs_volume_type")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredNumberField(aws.PostgresqlEbsVolumeIops, "aws postgresql_ebs_volume_iops")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredNumberField(aws.PostgresqlEbsVolumeSize, "aws postgresql_ebs_volume_size")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredStringTypeField(aws.PostgresqlEbsVolumeType, "aws postgresql_ebs_volume_type")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func validateAwsManagedServices(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateStringTypeField(aws.ManagedOpensearchCertificate, "aws managed_opensearch_certificate"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedOpensearchDomainName, "aws managed_opensearch_domain_name"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateUrl(aws.ManagedOpensearchDomainURL, "aws managed_opensearch_domain_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedOpensearchUserPassword, "aws managed_opensearch_user_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedOpensearchUsername, "aws managed_opensearch_username"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(aws.ManagedRdsCertificate, "aws managed_rds_certificate"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedRdsDbuserPassword, "aws managed_rds_dbuser_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedRdsDbuserUsername, "aws managed_rds_dbuser_username"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateUrl(aws.ManagedRdsInstanceURL, "aws managed_rds_instance_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedRdsSuperuserPassword, "aws managed_rds_superuser_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredStringTypeField(aws.ManagedRdsSuperuserUsername, "aws managed_rds_superuser_username"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(aws.AwsOsSnapshotRoleArn, "aws aws_os_snapshot_role_arn"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(aws.OsSnapshotUserAccessKeyID, "aws os_snapshot_user_access_key_id"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateStringTypeField(aws.OsSnapshotUserAccessKeySecret, "aws os_snapshot_user_access_key_secret"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateAwsOsPgConfig(aws); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func validateCommonAwsSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateRequiredStringTypeField(aws.Profile, "aws profile name"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.Region, "aws region"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.AwsVpcID, "aws aws_vpc_id"); err != nil {
		errorList.PushBack(err)
	}

	if aws.AwsCidrBlockAddr == "" {
		if err := validateRequiredStringListField(aws.PrivateCustomSubnets, "aws private_custom_subnets"); err != nil {
			errorList.PushBack(err)
		}

		if err := validateRequiredStringListField(aws.PublicCustomSubnets, "aws public_custom_subnets"); err != nil {
			errorList.PushBack(err)
		}
	}

	if err := validateRequiredStringTypeField(aws.SSHKeyPairName, "aws ssh_key_pair_name"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateStringBasedBoolean(aws.LbAccessLogs, "aws lb_access_logs"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredBooleanField(aws.DeleteOnTermination, "aws delete_on_termination"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.AutomateServerInstanceType, "aws automate_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.ChefServerInstanceType, "aws chef_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.OpensearchServerInstanceType, "aws opensearch_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.PostgresqlServerInstanceType, "aws postgresql_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.AutomateLbCertificateArn, "aws automate_lb_certificate_arn"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.ChefServerLbCertificateArn, "aws chef_server_lb_certificate_arn"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredNumberField(aws.AutomateEbsVolumeIops, "aws automate_ebs_volume_iops"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredNumberField(aws.AutomateEbsVolumeSize, "aws automate_ebs_volume_size"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.AutomateEbsVolumeType, "aws automate_ebs_volume_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredNumberField(aws.ChefEbsVolumeIops, "aws chef_ebs_volume_iops"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredNumberField(aws.ChefEbsVolumeSize, "aws chef_ebs_volume_size"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredStringTypeField(aws.ChefEbsVolumeType, "aws chef_ebs_volume_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredBooleanField(aws.SetupManagedServices, "aws setup_managed_services"); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}
