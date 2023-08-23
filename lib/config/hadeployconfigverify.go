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
		if err := c.verifyExistingInfra(); err != nil {
			errorList.PushBack(err)
		}
	}

	// Validate Aws
	if c.IsAws() {
		if err := c.verifyAws(); err != nil {
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

	// no need to verify os ans pg config for aws and on-prem managed db
	if (c.IsAws() && c.Aws.Config.SetupManagedServices) || (c.IsExistingInfra() && c.IsExternalDb()) {
		return getSingleErrorFromList(errorList)
	}

	if err := c.verifyOpensearchSettings(); err != nil {
		errorList.PushBack(err)
	}
	if err := c.verifyPostgresqlSettings(); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExistingInfra() error {
	errorList := list.New()
	if err := c.verifyConfigInitials(c.Architecture.ExistingInfra); err != nil {
		errorList.PushBack(err)
	}

	if err := c.validateExistingInfraBackupConfig(); err != nil {
		errorList.PushBack(err)
	}

	if err := c.verifyExistingInfraSettings(c.ExistingInfra.Config); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyAws() error {
	errorList := list.New()
	if err := c.verifyConfigInitials(c.Architecture.Aws); err != nil {
		errorList.PushBack(err)
	}

	if err := c.validateAwsBackupConfig(); err != nil {
		errorList.PushBack(err)
	}

	if err := c.validateAwsSettings(c.Aws.Config); err != nil {
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
	return c.Verify()
}

func (c *HaDeployConfig) verifyConfigInitials(configInitials *ConfigInitials) error {
	errorList := list.New()

	if err := validateRequiredString(configInitials.SecretsKeyFile, "secrets_key_file"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(configInitials.SecretsStoreFile, "secrets_store_file"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(configInitials.Architecture, "architecture", "aws", "existing_nodes", "deployment"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(configInitials.WorkspacePath, "workspace_path", "/hab/a2_deploy_workspace"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(configInitials.SSHUser, "ssh_user"); err != nil {
		errorList.PushBack(err)
	}
	if err := validatePort(configInitials.SSHPort, "ssh_port", false); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredPathField(configInitials.SSHKeyFile, "ssh_key_file"); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) validateExistingInfraBackupConfig() error {
	// validate existing infra backup config
	backupConfig := c.Architecture.ExistingInfra.BackupConfig
	// backup config is optional
	if backupConfig == "" {
		return nil
	}

	if err := validateRequiredString(backupConfig, "backup_config", "object_storage", "file_system"); err != nil {
		return err
	}

	if backupConfig == "file_system" {
		if err := validateRequiredString(c.Architecture.ExistingInfra.BackupMount, "backup_mount"); err != nil {
			return err
		}
	}

	if backupConfig == "object_storage" {
		if err := c.verifyObjectStorage(c.ObjectStorage.Config); err != nil {
			return err
		}
	}

	return nil
}

func (c *HaDeployConfig) validateAwsBackupConfig() error {
	// backup config is optional
	if c.Architecture.Aws.BackupConfig == "" {
		return nil
	}
	// validate aws backup config
	if err := checkForValidS3Bucket(c); err != nil {
		return err
	}
	if c.Aws.Config.SetupManagedServices {
		if err := validateRequiredString(c.Architecture.Aws.BackupConfig, "backup_config", "s3"); err != nil {
			return err
		}
	} else {
		if err := validateRequiredString(c.Architecture.Aws.BackupConfig, "backup_config", "s3", "efs"); err != nil {
			return err
		}
	}
	return nil
}

func (c *HaDeployConfig) verifyObjectStorage(objectStorage *ConfigObjectStorage) error {
	errorList := list.New()
	if err := validateRequiredString(objectStorage.BucketName, "bucket_name"); err != nil {
		errorList.PushBack(err)
	}
	if objectStorage.Location == GCS_STORAGE {
		if err := c.verifyGcsStorage(objectStorage.GcpServiceAccount); err != nil {
			errorList.PushBack(err)
		}
	}
	if objectStorage.Location == "s3" {
		if err := validateRequiredString(objectStorage.AccessKey, "access_key"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateRequiredString(objectStorage.SecretKey, "secret_key"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateS3Endpoint(objectStorage.Endpoint); err != nil {
			errorList.PushBack(err)
		}
		if objectStorage.Region != "" {
			if err := validateS3AWSRegion(objectStorage.Region); err != nil {
				errorList.PushBack(err)
			}
		}
	}
	if objectStorage.Location == AWS_S3 {
		if err := validateRequiredString(objectStorage.AccessKey, "access_key"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateRequiredString(objectStorage.SecretKey, "secret_key"); err != nil {
			errorList.PushBack(err)
		}
		if err := validateS3Endpoint(objectStorage.Endpoint); err != nil {
			errorList.PushBack(err)
		}
		if objectStorage.Region != "" {
			if err := validateS3AWSRegion(objectStorage.Region); err != nil {
				errorList.PushBack(err)
			}
		}
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyAutomateSettings() error {
	automateSettings := c.Automate.Config
	errorList := list.New()

	if err := validateFQDN(automateSettings.Fqdn, "automate fqdn"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateAutomateAdminPassword(automateSettings); err != nil {
		errorList.PushBack(err)
	}
	if err := validateNumberField(automateSettings.InstanceCount, "automate instance_count", true); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(automateSettings.ConfigFile, "config_file", "configs/automate.toml"); err != nil {
		errorList.PushBack(err)
	}
	if err := validatePort(automateSettings.TeamsPort, "teams_port", false); err != nil {
		errorList.PushBack(err)
	}
	if automateSettings.FqdnRootCA != "" {
		if err := validateFqdnRootCA(automateSettings.FqdnRootCA, AUTOMATE); err != nil {
			errorList.PushBack(err)
		}
	}
	if automateSettings.EnableCustomCerts {
		if err := validateAutomateCerts(c); err != nil {
			errorList.PushBack(err)
		}
		if c.IsExistingInfra() {
			if err := validateCertsByIP(automateSettings.CertsByIP, "automate ip"); err != nil {
				errorList.PushBack(err)
			}
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyChefServerSettings() error {
	chefServerSettings := c.ChefServer.Config
	errorList := list.New()

	if chefServerSettings.ChefServerFqdn != "" {
		if err := validateFQDN(chefServerSettings.ChefServerFqdn, "chef-infra-server fqdn"); err != nil {
			errorList.PushBack(err)
		}
	}

	if chefServerSettings.FqdnRootCA != "" {
		if err := validateFqdnRootCA(chefServerSettings.FqdnRootCA, CHEFSERVER); err != nil {
			errorList.PushBack(err)
		}
	}

	if err := validateNumberField(chefServerSettings.InstanceCount, "chef server instance_count", true); err != nil {
		errorList.PushBack(err)
	}

	if chefServerSettings.EnableCustomCerts {
		if err := validateChefServerCerts(c); err != nil {
			errorList.PushBack(err)
		}
		if c.IsExistingInfra() {
			if err := validateCertsByIP(chefServerSettings.CertsByIP, "chef server ip"); err != nil {
				errorList.PushBack(err)
			}
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyOpensearchSettings() error {
	opensearchSettings := c.Opensearch.Config
	errorList := list.New()

	if err := validateNumberField(opensearchSettings.InstanceCount, "opensearch instance_count", true); err != nil {
		errorList.PushBack(err)
	}
	if opensearchSettings.EnableCustomCerts {
		if err := validateOpensearchCerts(c); err != nil {
			errorList.PushBack(err)
		}
		if c.IsExistingInfra() {
			if err := validateCertsByIP(opensearchSettings.CertsByIP, "opensearch ip"); err != nil {
				errorList.PushBack(err)
			}
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyPostgresqlSettings() error {
	postgresqlSettings := c.Postgresql.Config
	errorList := list.New()

	if err := validateNumberField(postgresqlSettings.InstanceCount, "postgresql instance_count", true); err != nil {
		errorList.PushBack(err)
	}
	if postgresqlSettings.EnableCustomCerts {
		if err := validatePostgresqlCerts(c); err != nil {
			errorList.PushBack(err)
		}
		if c.IsExistingInfra() {
			if err := validateCertsByIP(postgresqlSettings.CertsByIP, "postgresql ip"); err != nil {
				errorList.PushBack(err)
			}
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

	// existing infra
	if !c.IsExternalDb() {
		if err := c.verifyNonExternalDb(existingInfraSettings); err != nil {
			errorList.PushBack(err)
		}
	}

	if c.IsExternalDb() {
		if err := c.verifyExternalDb(existingInfraSettings); err != nil {
			errorList.PushBack(err)
		}
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExternalDb(existingInfraSettings *ConfigExistingInfraSettings) error {
	errorList := list.New()
	// on-prem, AWS, or self-managed
	if err := c.verifyExternalPgSettings(c.External.Database.PostgreSQL); err != nil {
		errorList.PushBack(err)
	}
	if err := c.verifyExternalOsSettings(c.External.Database.OpenSearch); err != nil {
		errorList.PushBack(err)
	}

	if c.IsAwsExternalOsConfigured() {
		if err := c.verifyAwsExternalOsSettings(c.External.Database.OpenSearch.Aws); err != nil {
			errorList.PushBack(err)
		}
	}
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyNonExternalDb(existingInfraSettings *ConfigExistingInfraSettings) error {
	errorList := list.New()
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
	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExternalPgSettings(externalPostgresqlSettings *ExternalPgSettings) error {
	errorList := list.New()

	err := validateRequiredString(externalPostgresqlSettings.DbuserUsername, "dbuser_username")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(externalPostgresqlSettings.DbuserPassword, "dbuser_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateUrlWithPort(externalPostgresqlSettings.InstanceURL, "instance_url")
	if err != nil {
		errorList.PushBack(err)
	}

	if c.IsExternalDbSelfManaged() {
		err = validateRequiredString(externalPostgresqlSettings.PostgresqlRootCert, "postgresql_root_cert")
		if err != nil {
			errorList.PushBack(err)
		}
	}

	err = validateRequiredString(externalPostgresqlSettings.SuperuserPassword, "superuser_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(externalPostgresqlSettings.SuperuserUsername, "superuser_username")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyExternalOsSettings(externalOpensearchSettings *ExternalOsSettings) error {
	errorList := list.New()

	err := validateRequiredString(externalOpensearchSettings.OpensearchDomainName, "opensearch_domain_name")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateFQDN(externalOpensearchSettings.OpensearchDomainURL, "opensearch_domain_url")
	if err != nil {
		errorList.PushBack(err)
	}

	if c.IsExternalDbSelfManaged() {
		err = validateRequiredString(externalOpensearchSettings.OpensearchRootCert, "opensearch_root_cert")
		if err != nil {
			errorList.PushBack(err)
		}
	}

	err = validateRequiredString(externalOpensearchSettings.OpensearchUserPassword, "opensearch_user_password")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(externalOpensearchSettings.OpensearchUsername, "opensearch_username")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyAwsExternalOsSettings(awsExternalOpensearchSettings *AwsExternalOsSettings) error {
	errorList := list.New()

	err := validateRequiredString(awsExternalOpensearchSettings.AwsOsSnapshotRoleArn, "aws_os_snapshot_role_arn")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(awsExternalOpensearchSettings.OsSnapshotUserAccessKeyID, "os_snapshot_user_access_key_id")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(awsExternalOpensearchSettings.OsSnapshotUserAccessKeySecret, "os_snapshot_user_access_key_secret")
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
		(c.External.Database.Type == AWS || c.External.Database.Type == SELF_MANAGED)
}

func (c *HaDeployConfig) IsAwsExternalOsConfigured() bool {
	return c.External != nil && c.External.Database != nil &&
		c.External.Database.OpenSearch != nil && c.External.Database.OpenSearch.Aws != nil &&
		c.External.Database.OpenSearch.Aws.AwsOsSnapshotRoleArn != ""
}

func (c *HaDeployConfig) IsExternalDbSelfManaged() bool {
	return c.IsExternalDb() && c.External.Database.Type == SELF_MANAGED
}

func validateAwsOsPgConfig(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateAwsDbInstanceType(aws); err != nil {
		errorList.PushBack(err)
	}

	err := validateNumberField(aws.OpensearchEbsVolumeIops, "aws opensearch_ebs_volume_iops", true)
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateNumberField(aws.OpensearchEbsVolumeSize, "aws opensearch_ebs_volume_size", true)
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(aws.OpensearchEbsVolumeType, "aws opensearch_ebs_volume_type")
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateNumberField(aws.PostgresqlEbsVolumeIops, "aws postgresql_ebs_volume_iops", true)
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateNumberField(aws.PostgresqlEbsVolumeSize, "aws postgresql_ebs_volume_size", true)
	if err != nil {
		errorList.PushBack(err)
	}

	err = validateRequiredString(aws.PostgresqlEbsVolumeType, "aws postgresql_ebs_volume_type")
	if err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func validateAwsManagedServices(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateRequiredString(aws.ManagedOpensearchDomainName, "aws managed_opensearch_domain_name"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateFQDN(aws.ManagedOpensearchDomainURL, "aws managed_opensearch_domain_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedOpensearchUserPassword, "aws managed_opensearch_user_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedOpensearchUsername, "aws managed_opensearch_username"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedRdsDbuserPassword, "aws managed_rds_dbuser_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedRdsDbuserUsername, "aws managed_rds_dbuser_username"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateUrlWithPort(aws.ManagedRdsInstanceURL, "aws managed_rds_instance_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedRdsSuperuserPassword, "aws managed_rds_superuser_password"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(aws.ManagedRdsSuperuserUsername, "aws managed_rds_superuser_username"); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func validateCommonAwsSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateRequiredString(aws.Region, "aws region"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.AwsVpcID, "aws aws_vpc_id"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateAwsCidrBlockSettings(aws); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.SSHKeyPairName, "aws ssh_key_pair_name"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateStringBasedBoolean(aws.LbAccessLogs, "aws lb_access_logs", true); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.AmiID, "aws ami_id"); err != nil {
		errorList.PushBack(err)
	}

	if err := awsAutomateSettings(aws); err != nil {
		errorList.PushBack(err)
	}

	if err := awsChefSettings(aws); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func validateAwsDbInstanceType(aws *ConfigAwsSettings) error {
	errorList := list.New()
	if err := validateRequiredString(aws.OpensearchServerInstanceType, "aws opensearch_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.PostgresqlServerInstanceType, "aws postgresql_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}
	return getSingleErrorFromList(errorList)
}

func validateAwsCidrBlockSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()
	if aws.AwsCidrBlockAddr == "" {
		if err := validateRequiredStringListField(aws.PrivateCustomSubnets, "aws private_custom_subnets", 3); err != nil {
			errorList.PushBack(err)
		}

		// Commenting the code, as per https://chefio.atlassian.net/browse/CHEF-3646 public subnets are not mandatory.
		//if err := validateRequiredStringListField(aws.PublicCustomSubnets, "aws public_custom_subnets", 3); err != nil {
		//	errorList.PushBack(err)
		//}
	} else {
		if err := validateRequiredString(aws.AwsCidrBlockAddr, "aws aws_cidr_block_addr"); err != nil {
			errorList.PushBack(err)
		}
	}

	return getSingleErrorFromList(errorList)
}

func awsAutomateSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateRequiredString(aws.AutomateServerInstanceType, "aws automate_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.AutomateLbCertificateArn, "aws automate_lb_certificate_arn"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateNumberField(aws.AutomateEbsVolumeIops, "aws automate_ebs_volume_iops", true); err != nil {
		errorList.PushBack(err)
	}

	if err := validateNumberField(aws.AutomateEbsVolumeSize, "aws automate_ebs_volume_size", true); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.AutomateEbsVolumeType, "aws automate_ebs_volume_type"); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)

}

func awsChefSettings(aws *ConfigAwsSettings) error {
	errorList := list.New()

	if err := validateNumberField(aws.ChefEbsVolumeIops, "aws chef_ebs_volume_iops", true); err != nil {
		errorList.PushBack(err)
	}

	if err := validateNumberField(aws.ChefEbsVolumeSize, "aws chef_ebs_volume_size", true); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.ChefEbsVolumeType, "aws chef_ebs_volume_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.ChefServerInstanceType, "aws chef_server_instance_type"); err != nil {
		errorList.PushBack(err)
	}

	if err := validateRequiredString(aws.ChefServerLbCertificateArn, "aws chef_server_lb_certificate_arn"); err != nil {
		errorList.PushBack(err)
	}

	return getSingleErrorFromList(errorList)
}

func (c *HaDeployConfig) verifyGcsStorage(gcp *GcpServiceAccount) error {
	errorList := list.New()
	if err := validateRequiredString(gcp.Type, "type"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.ProjectID, "project_id"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.PrivateKeyID, "private_key_id"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.PrivateKey, "private_key"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.TokenURI, "token_uri"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.ClientID, "client_id"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.ClientEmail, "client_email"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.AuthURI, "auth_uri"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.AuthProviderX509CertURL, "auth_provider_x509_cert_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.ClientX509CertURL, "client_x509_cert_url"); err != nil {
		errorList.PushBack(err)
	}
	if err := validateRequiredString(gcp.UniverseDomain, "universe_domain"); err != nil {
		errorList.PushBack(err)
	}
	return getSingleErrorFromList(errorList)
}
