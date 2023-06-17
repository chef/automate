package main

import (
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/stretchr/testify/assert"
)

func TestCopyCertsByIP(t *testing.T) {
	// Prepare test data
	existing := []CertByIP{
		{
			IP:         "10.0.0.1",
			PrivateKey: "/existing/private/key1",
			PublicKey:  "/existing/public/key1",
			NodesDn:    "/existing/nodes/dn1",
		},
		{
			IP:         "10.0.0.2",
			PrivateKey: "/existing/private/key2",
			PublicKey:  "/existing/public/key2",
			NodesDn:    "/existing/nodes/dn2",
		},
	}

	haDeploy := &[]config.CertByIP{}

	// Call the function
	CopyCertsByIP(haDeploy, existing)

	// Assert the values in haDeploy
	assert.Len(t, *haDeploy, 2)

	// Assert the copied values for the first CertByIP
	assert.Equal(t, "10.0.0.1", (*haDeploy)[0].IP)
	assert.Equal(t, "/existing/private/key1", (*haDeploy)[0].PrivateKey)
	assert.Equal(t, "/existing/public/key1", (*haDeploy)[0].PublicKey)
	assert.Equal(t, "/existing/nodes/dn1", (*haDeploy)[0].NodesDn)

	// Assert the copied values for the second CertByIP
	assert.Equal(t, "10.0.0.2", (*haDeploy)[1].IP)
	assert.Equal(t, "/existing/private/key2", (*haDeploy)[1].PrivateKey)
	assert.Equal(t, "/existing/public/key2", (*haDeploy)[1].PublicKey)
	assert.Equal(t, "/existing/nodes/dn2", (*haDeploy)[1].NodesDn)
}

func TestCopyEc2InstanceConfig(t *testing.T) {
	// Mock input values
	haDeployConfig := &config.HaDeployConfig{}

	awsEc2InstanceConfig := &AwsConfigToml{
		Aws: AwsToml{
			Config: ConfigToml{
				AmiID:                        "ami-08d4ac5b634553e16",
				DeleteOnTermination:          true,
				AutomateServerInstanceType:   "t3.medium",
				ChefServerInstanceType:       "t3.medium",
				PostgresqlServerInstanceType: "m5.large",
				OpensearchServerInstanceType: "m5.large",
				AutomateLbCertificateArn:     "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				ChefServerLbCertificateArn:   "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				AutomateEbsVolumeIops:        "100",
				AutomateEbsVolumeSize:        "50",
				AutomateEbsVolumeType:        "gp3",
				ChefEbsVolumeIops:            "100",
				ChefEbsVolumeSize:            "50",
				ChefEbsVolumeType:            "gp3",
				OpensearchEbsVolumeIops:      "100",
				OpensearchEbsVolumeSize:      "50",
				OpensearchEbsVolumeType:      "gp3",
				PostgresqlEbsVolumeIops:      "100",
				PostgresqlEbsVolumeSize:      "50",
				PostgresqlEbsVolumeType:      "gp3",
				LBAccessLogs:                 "false",
			},
		},
	}

	// Call the function
	haDeployConfig = CopyEc2InstanceConfig(haDeployConfig, awsEc2InstanceConfig)

	awsConfigSetting := haDeployConfig.Aws.Config
	// Verify the copied values
	assert.Equal(t, awsConfigSetting.AmiID, awsEc2InstanceConfig.Aws.Config.AmiID)
	assert.Equal(t, awsConfigSetting.DeleteOnTermination, awsEc2InstanceConfig.Aws.Config.DeleteOnTermination)
	assert.Equal(t, awsConfigSetting.AutomateServerInstanceType, awsEc2InstanceConfig.Aws.Config.AutomateServerInstanceType)
	assert.Equal(t, awsConfigSetting.ChefServerInstanceType, awsEc2InstanceConfig.Aws.Config.ChefServerInstanceType)
	assert.Equal(t, awsConfigSetting.PostgresqlServerInstanceType, awsEc2InstanceConfig.Aws.Config.PostgresqlServerInstanceType)
	assert.Equal(t, awsConfigSetting.OpensearchServerInstanceType, awsEc2InstanceConfig.Aws.Config.OpensearchServerInstanceType)
	assert.Equal(t, awsConfigSetting.AutomateLbCertificateArn, awsEc2InstanceConfig.Aws.Config.AutomateLbCertificateArn)
	assert.Equal(t, awsConfigSetting.ChefServerLbCertificateArn, awsEc2InstanceConfig.Aws.Config.ChefServerLbCertificateArn)
	assert.Equal(t, awsConfigSetting.AutomateEbsVolumeIops, awsEc2InstanceConfig.Aws.Config.AutomateEbsVolumeIops)
	assert.Equal(t, awsConfigSetting.AutomateEbsVolumeSize, awsEc2InstanceConfig.Aws.Config.AutomateEbsVolumeSize)
	assert.Equal(t, awsConfigSetting.AutomateEbsVolumeType, awsEc2InstanceConfig.Aws.Config.AutomateEbsVolumeType)
	assert.Equal(t, awsConfigSetting.ChefEbsVolumeIops, awsEc2InstanceConfig.Aws.Config.ChefEbsVolumeIops)
	assert.Equal(t, awsConfigSetting.ChefEbsVolumeSize, awsEc2InstanceConfig.Aws.Config.ChefEbsVolumeSize)
	assert.Equal(t, awsConfigSetting.ChefEbsVolumeType, awsEc2InstanceConfig.Aws.Config.ChefEbsVolumeType)
	assert.Equal(t, awsConfigSetting.OpensearchEbsVolumeIops, awsEc2InstanceConfig.Aws.Config.OpensearchEbsVolumeIops)
	assert.Equal(t, awsConfigSetting.OpensearchEbsVolumeSize, awsEc2InstanceConfig.Aws.Config.OpensearchEbsVolumeSize)
	assert.Equal(t, awsConfigSetting.OpensearchEbsVolumeType, awsEc2InstanceConfig.Aws.Config.OpensearchEbsVolumeType)
	assert.Equal(t, awsConfigSetting.PostgresqlEbsVolumeIops, awsEc2InstanceConfig.Aws.Config.PostgresqlEbsVolumeIops)
	assert.Equal(t, awsConfigSetting.PostgresqlEbsVolumeSize, awsEc2InstanceConfig.Aws.Config.PostgresqlEbsVolumeSize)
	assert.Equal(t, awsConfigSetting.PostgresqlEbsVolumeType, awsEc2InstanceConfig.Aws.Config.PostgresqlEbsVolumeType)
	assert.Equal(t, awsConfigSetting.LbAccessLogs, awsEc2InstanceConfig.Aws.Config.LBAccessLogs)

}

func TestCopyManagedServices(t *testing.T) {

	haDeployConfig := &config.HaDeployConfig{}

	awsManagedServicesConfig := &AwsConfigToml{
		Aws: AwsToml{
			Config: ConfigToml{
				OpensearchDomainName:   "opensearch-domain",
				OpensearchDomainUrl:    "opensearch-url",
				OpensearchUserPassword: "opensearch-password",
				OpensearchUsername:     "opensearch-username",
				OpensearchCertificate:  "opensearch-certificate",
				OsUserAccessKeyId:      "access-key-id",
				OsUserAccessKeySecret:  "access-key-secret",
				AwsOsSnapshotRoleArn:   "snapshot-role-arn",
				RDSCertificate:         "rds-certificate",
				RDSDBUserPassword:      "rds-db-password",
				RDSDBUserName:          "rds-db-username",
				RDSInstanceUrl:         "rds-instance-url",
				RDSSuperUserPassword:   "rds-superuser-password",
				RDSSuperUserName:       "rds-superuser-username",
			},
		},
	}

	haDeployConfig = CopyManagedServices(haDeployConfig, awsManagedServicesConfig)
	haConfigAws := haDeployConfig.Aws.Config
	// Assert the copied values
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OpensearchDomainName, haConfigAws.ManagedOpensearchDomainName)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OpensearchDomainUrl, haConfigAws.ManagedOpensearchDomainURL)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OpensearchUserPassword, haConfigAws.ManagedOpensearchUserPassword)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OpensearchUsername, haConfigAws.ManagedOpensearchUsername)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OpensearchCertificate, haConfigAws.ManagedOpensearchCertificate)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OsUserAccessKeyId, haConfigAws.OsSnapshotUserAccessKeyID)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.OsUserAccessKeySecret, haConfigAws.OsSnapshotUserAccessKeySecret)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.AwsOsSnapshotRoleArn, haConfigAws.AwsOsSnapshotRoleArn)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSCertificate, haConfigAws.ManagedRdsCertificate)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSDBUserPassword, haConfigAws.ManagedRdsDbuserPassword)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSDBUserName, haConfigAws.ManagedRdsDbuserUsername)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSInstanceUrl, haConfigAws.ManagedRdsInstanceURL)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSSuperUserPassword, haConfigAws.ManagedRdsSuperuserPassword)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSSuperUserName, haConfigAws.ManagedRdsSuperuserUsername)

}

func TestCopyAwsNetworkConfig(t *testing.T) {

	haDeployConfig := &config.HaDeployConfig{}
	awsNetworkConfig := &AwsConfigToml{
		Aws: AwsToml{
			Config: ConfigToml{
				Profile:              "my-profile",
				Region:               "us-west-2",
				AwsVpcId:             "vpc-12345",
				AwsCidrBlockAddr:     "10.0.0.0/16",
				PrivateCustomSubnets: []string{"subnet-12345", "subnet-67890"},
				PublicCustomSubnets:  []string{"subnet-54321", "subnet-09876"},
				SSHKeyPairName:       "my-keypair",
			},
		},
	}

	haDeployConfig = CopyAwsNetworkConfig(haDeployConfig, awsNetworkConfig)
	haConfigAws := haDeployConfig.Aws.Config
	// Assert the copied values
	assert.Equal(t, awsNetworkConfig.Aws.Config.Profile, haConfigAws.Profile)
	assert.Equal(t, awsNetworkConfig.Aws.Config.Region, haConfigAws.Region)
	assert.Equal(t, awsNetworkConfig.Aws.Config.AwsVpcId, haConfigAws.AwsVpcID)
	assert.Equal(t, awsNetworkConfig.Aws.Config.AwsCidrBlockAddr, haConfigAws.AwsCidrBlockAddr)
	assert.Equal(t, awsNetworkConfig.Aws.Config.PrivateCustomSubnets, haConfigAws.PrivateCustomSubnets)
	assert.Equal(t, awsNetworkConfig.Aws.Config.PublicCustomSubnets, haConfigAws.PublicCustomSubnets)
	assert.Equal(t, awsNetworkConfig.Aws.Config.SSHKeyPairName, haConfigAws.SSHKeyPairName)
}

func TestCopyAwsConfig(t *testing.T) {
	haDeployConfig := &config.HaDeployConfig{}
	awsConfig := &AwsConfigToml{
		Aws: AwsToml{
			Config: ConfigToml{
				Profile:                      "my-profile",
				Region:                       "us-west-2",
				AwsVpcId:                     "vpc-12345",
				AwsCidrBlockAddr:             "10.0.0.0/16",
				PrivateCustomSubnets:         []string{"subnet-12345", "subnet-67890"},
				PublicCustomSubnets:          []string{"subnet-54321", "subnet-09876"},
				SSHKeyPairName:               "my-keypair",
				SetupManagedServices:         true,
				OpensearchDomainName:         "opensearch-domain",
				OpensearchDomainUrl:          "opensearch-url",
				OpensearchUserPassword:       "opensearch-password",
				OpensearchUsername:           "opensearch-username",
				OpensearchCertificate:        "opensearch-certificate",
				OsUserAccessKeyId:            "access-key-id",
				OsUserAccessKeySecret:        "access-key-secret",
				AwsOsSnapshotRoleArn:         "snapshot-role-arn",
				RDSCertificate:               "rds-certificate",
				RDSDBUserPassword:            "rds-db-password",
				RDSDBUserName:                "rds-db-username",
				RDSInstanceUrl:               "rds-instance-url",
				RDSSuperUserPassword:         "rds-superuser-password",
				RDSSuperUserName:             "rds-superuser-username",
				AmiID:                        "ami-08d4ac5b634553e16",
				DeleteOnTermination:          true,
				AutomateServerInstanceType:   "t3.medium",
				ChefServerInstanceType:       "t3.medium",
				PostgresqlServerInstanceType: "m5.large",
				OpensearchServerInstanceType: "m5.large",
				AutomateLbCertificateArn:     "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				ChefServerLbCertificateArn:   "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				AutomateEbsVolumeIops:        "100",
				AutomateEbsVolumeSize:        "50",
				AutomateEbsVolumeType:        "gp3",
				ChefEbsVolumeIops:            "100",
				ChefEbsVolumeSize:            "50",
				ChefEbsVolumeType:            "gp3",
				OpensearchEbsVolumeIops:      "100",
				OpensearchEbsVolumeSize:      "50",
				OpensearchEbsVolumeType:      "gp3",
				PostgresqlEbsVolumeIops:      "100",
				PostgresqlEbsVolumeSize:      "50",
				PostgresqlEbsVolumeType:      "gp3",
				LBAccessLogs:                 "false",
			},
		},
	}
	haDeployConfig = CopyAwsConfig(haDeployConfig, awsConfig)

	// aws := haDeployConfig.Aws.Config
	// assert.Equal(t, "my-profile", aws.Profile)
	// assert.Equal(t, "us-west-2", aws.Region)
	// assert.Equal(t, "vpc-12345", aws.AwsVpcID)
	// assert.Equal(t, "10.0.0.0/16", aws.AwsCidrBlockAddr)
	// assert.Equal(t, []string{"subnet-12345", "subnet-67890"}, aws.PrivateCustomSubnets)
	// assert.Equal(t, []string{"subnet-54321", "subnet-09876"}, aws.PublicCustomSubnets)
	// assert.Equal(t, "my-keypair", aws.SSHKeyPairName)
	// assert.Equal(t, "ami-08d4ac5b634553e16", aws.AmiID)
	// assert.Equal(t, true, aws.DeleteOnTermination)
	// assert.Equal(t, "t3.medium", aws.AutomateServerInstanceType)
	// assert.Equal(t, "t3.medium", aws.ChefServerInstanceType)
	// assert.Equal(t, "m5.large", aws.PostgresqlServerInstanceType)
	// assert.Equal(t, "m5.large", aws.OpensearchServerInstanceType)
	// assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.AutomateLbCertificateArn)
	// assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.ChefServerLbCertificateArn)
	// assert.Equal(t, "100", aws.AutomateEbsVolumeIops)
	// assert.Equal(t, "50", aws.AutomateEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.AutomateEbsVolumeType)
	// assert.Equal(t, "100", aws.ChefEbsVolumeIops)
	// assert.Equal(t, "50", aws.ChefEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.ChefEbsVolumeType)
	// assert.Equal(t, "100", aws.OpensearchEbsVolumeIops)
	// assert.Equal(t, "50", aws.OpensearchEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.OpensearchEbsVolumeType)
	// assert.Equal(t, "100", aws.PostgresqlEbsVolumeIops)
	// assert.Equal(t, "50", aws.PostgresqlEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.PostgresqlEbsVolumeType)
	// assert.Equal(t, "false", aws.LbAccessLogs)
	// assert.Equal(t, "opensearch-domain", aws.ManagedOpensearchDomainName)
	// assert.Equal(t, "opensearch-url", aws.ManagedOpensearchDomainURL)
	// assert.Equal(t, "opensearch-password", aws.ManagedOpensearchUserPassword)
	// assert.Equal(t, "opensearch-username", aws.ManagedOpensearchUsername)
	// assert.Equal(t, "opensearch-certificate", aws.ManagedOpensearchCertificate)
	// assert.Equal(t, "access-key-id", aws.OsSnapshotUserAccessKeyID)
	// assert.Equal(t, "access-key-secret", aws.OsSnapshotUserAccessKeySecret)
	// assert.Equal(t, "snapshot-role-arn", aws.AwsOsSnapshotRoleArn)
	// assert.Equal(t, "rds-certificate", aws.ManagedRdsCertificate)
	// assert.Equal(t, "rds-db-password", aws.ManagedRdsDbuserPassword)
	// assert.Equal(t, "rds-db-username", aws.ManagedRdsDbuserUsername)
	// assert.Equal(t, "rds-instance-url", aws.ManagedRdsInstanceURL)
	// assert.Equal(t, "rds-superuser-password", aws.ManagedRdsSuperuserPassword)
	// assert.Equal(t, "rds-superuser-username", aws.ManagedRdsSuperuserUsername)
}

func TestCopyConfigInitials(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}
	existingInfraConfig := &ExistingInfraConfigToml{
		Architecture: ExistingInfraArchitectureToml{
			ConfigInitials: ExistingInfraConfigInitialsToml{
				SSHUser:                     "existing-ssh-user",
				Architecture:                "existing-architecture",
				BackupConfig:                "object_storage",
				BackupMount:                 "/existing/backup/mount",
				HabitatUIDGid:               "existing-habitat-uid-gid",
				LoggingMonitoringManagement: "true",
				SSHGroupName:                "existing-ssh-group",
				SSHKeyFile:                  "/existing/ssh/key/file",
				SSHPort:                     "22",
				SecretsKeyFile:              "/existing/secrets/key/file",
				SecretsStoreFile:            "/existing/secrets/store/file",
				WorkspacePath:               "/existing/workspace/path",
			},
		},
	}

	awsConfig := &AwsConfigToml{
		Architecture: AwsArchitectureToml{
			ConfigInitials: AwsConfigInitialsToml{
				SSHUser:                     "aws-ssh-user",
				Architecture:                "aws-architecture",
				BackupConfig:                "s3",
				S3BucketName:                "aws-s3-bucket",
				BackupMount:                 "/aws/backup/mount",
				HabitatUIDGid:               "aws-habitat-uid-gid",
				LoggingMonitoringManagement: "false",
				SSHGroupName:                "aws-ssh-group",
				SSHKeyFile:                  "/aws/ssh/key/file",
				SSHPort:                     "2222",
				SecretsKeyFile:              "/aws/secrets/key/file",
				SecretsStoreFile:            "/aws/secrets/store/file",
				WorkspacePath:               "/aws/workspace/path",
			},
		},
	}

	// Test when awsConfig is not nil
	haDeployConfig = CopyConfigInitials(haDeployConfig, nil, awsConfig)
	haDeployConfigConfigInitialsAws := haDeployConfig.Architecture.Aws
	// Assert the copied values from awsConfig.ConfigInitials
	assert.Equal(t, "aws-ssh-user", haDeployConfigConfigInitialsAws.SSHUser)
	assert.Equal(t, "aws-architecture", haDeployConfigConfigInitialsAws.Architecture)
	assert.Equal(t, "s3", haDeployConfigConfigInitialsAws.BackupConfig)
	assert.Equal(t, "aws-s3-bucket", haDeployConfigConfigInitialsAws.S3BucketName)
	assert.Equal(t, "/aws/backup/mount", haDeployConfigConfigInitialsAws.BackupMount)
	assert.Equal(t, "aws-habitat-uid-gid", haDeployConfigConfigInitialsAws.HabitatUIDGid)
	assert.Equal(t, "false", haDeployConfigConfigInitialsAws.LoggingMonitoringManagement)
	assert.Equal(t, "aws-ssh-group", haDeployConfigConfigInitialsAws.SSHGroupName)
	assert.Equal(t, "/aws/ssh/key/file", haDeployConfigConfigInitialsAws.SSHKeyFile)
	assert.Equal(t, "2222", haDeployConfigConfigInitialsAws.SSHPort)
	assert.Equal(t, "/aws/secrets/key/file", haDeployConfigConfigInitialsAws.SecretsKeyFile)
	assert.Equal(t, "/aws/secrets/store/file", haDeployConfigConfigInitialsAws.SecretsStoreFile)
	assert.Equal(t, "/aws/workspace/path", haDeployConfigConfigInitialsAws.WorkspacePath)
	assert.Equal(t, "", haDeployConfigConfigInitialsAws.SudoPassword)

	// Test when awsConfig is nil
	haDeployConfig = CopyConfigInitials(haDeployConfig, existingInfraConfig, nil)
	haDeployConfigConfigInitialsExistingInfra := haDeployConfig.Architecture.ExistingInfra
	// Assert the copied values from existingInfraConfig.ConfigInitials
	assert.Equal(t, "existing-ssh-user", haDeployConfigConfigInitialsExistingInfra.SSHUser)
	assert.Equal(t, "existing-architecture", haDeployConfigConfigInitialsExistingInfra.Architecture)
	assert.Equal(t, "object_storage", haDeployConfigConfigInitialsExistingInfra.BackupConfig)
	assert.Equal(t, "/existing/backup/mount", haDeployConfigConfigInitialsExistingInfra.BackupMount)
	assert.Equal(t, "existing-habitat-uid-gid", haDeployConfigConfigInitialsExistingInfra.HabitatUIDGid)
	assert.Equal(t, "true", haDeployConfigConfigInitialsExistingInfra.LoggingMonitoringManagement)
	assert.Equal(t, "existing-ssh-group", haDeployConfigConfigInitialsExistingInfra.SSHGroupName)
	assert.Equal(t, "/existing/ssh/key/file", haDeployConfigConfigInitialsExistingInfra.SSHKeyFile)
	assert.Equal(t, "22", haDeployConfigConfigInitialsExistingInfra.SSHPort)
	assert.Equal(t, "/existing/secrets/key/file", haDeployConfigConfigInitialsExistingInfra.SecretsKeyFile)
	assert.Equal(t, "/existing/secrets/store/file", haDeployConfigConfigInitialsExistingInfra.SecretsStoreFile)
	assert.Equal(t, "/existing/workspace/path", haDeployConfigConfigInitialsExistingInfra.WorkspacePath)
	assert.Equal(t, "", haDeployConfigConfigInitialsExistingInfra.SudoPassword)

}

func TestCopyConfigObjectStorage(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}

	existingInfraConfig := &ExistingInfraConfigToml{
		ObjectStorage: ObjectStorageToml{
			Config: ObjectStorageConfigToml{
				AccessKey:  "existing-access-key",
				BucketName: "existing-bucket",
				Endpoint:   "existing-endpoint",
				Region:     "existing-region",
				SecretKey:  "existing-secret-key",
			},
		},
	}

	// Invoke the function
	haDeployConfig = CopyConfigObjectStorage(haDeployConfig, existingInfraConfig)
	haDeployConfigObjectStorageConfig := haDeployConfig.ObjectStorage.Config
	// Assert the copied values
	assert.Equal(t, "existing-access-key", haDeployConfigObjectStorageConfig.AccessKey)
	assert.Equal(t, "existing-bucket", haDeployConfigObjectStorageConfig.BucketName)
	assert.Equal(t, "existing-endpoint", haDeployConfigObjectStorageConfig.Endpoint)
	assert.Equal(t, "existing-region", haDeployConfigObjectStorageConfig.Region)
	assert.Equal(t, "existing-secret-key", haDeployConfigObjectStorageConfig.SecretKey)

}

func TestCopyAutomateSettings(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}

	existingInfraConfig := &ExistingInfraConfigToml{
		Automate: AutomateToml{
			Config: AutomateConfigToml{
				AdminPassword:     "existing-admin-password",
				ConfigFile:        "/existing/config/file",
				EnableCustomCerts: true,
				Fqdn:              "existing-fqdn",
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				TeamsPort:         "8080",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
	}

	awsConfig := &AwsConfigToml{
		Automate: AwsAutomateToml{
			Config: AwsAutomateConfigToml{
				AdminPassword:     "aws-admin-password",
				ConfigFile:        "/aws/config/file",
				EnableCustomCerts: false,
				Fqdn:              "aws-fqdn",
				InstanceCount:     "5",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
				RootCA:            "/aws/root/ca",
				TeamsPort:         "9090",
			},
		},
	}

	// Test when awsConfig is not nil
	haDeployConfig = CopyAutomateSettings(haDeployConfig, nil, awsConfig)

	haDeployConfigAutomateSettingsAws := haDeployConfig.Automate.Config
	// Assert the copied values from awsConfig.Automate.Config
	assert.Equal(t, "aws-admin-password", haDeployConfigAutomateSettingsAws.AdminPassword)
	assert.Equal(t, "/aws/config/file", haDeployConfigAutomateSettingsAws.ConfigFile)
	assert.Equal(t, false, haDeployConfigAutomateSettingsAws.EnableCustomCerts)
	assert.Equal(t, "aws-fqdn", haDeployConfigAutomateSettingsAws.Fqdn)
	assert.Equal(t, "5", haDeployConfigAutomateSettingsAws.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployConfigAutomateSettingsAws.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployConfigAutomateSettingsAws.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployConfigAutomateSettingsAws.RootCA)
	assert.Equal(t, "9090", haDeployConfigAutomateSettingsAws.TeamsPort)

	// Test when awsConfig is nil
	haDeployConfig = CopyAutomateSettings(haDeployConfig, existingInfraConfig, nil)
	haDeployConfigAutomateSettingsExistingInfra := haDeployConfig.Automate.Config

	// Assert the copied values from existingInfraConfig.Automate.Config
	assert.Equal(t, "existing-admin-password", haDeployConfigAutomateSettingsExistingInfra.AdminPassword)
	assert.Equal(t, "/existing/config/file", haDeployConfigAutomateSettingsExistingInfra.ConfigFile)
	assert.Equal(t, true, haDeployConfigAutomateSettingsExistingInfra.EnableCustomCerts)
	assert.Equal(t, "existing-fqdn", haDeployConfigAutomateSettingsExistingInfra.Fqdn)
	assert.Equal(t, "3", haDeployConfigAutomateSettingsExistingInfra.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployConfigAutomateSettingsExistingInfra.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfigAutomateSettingsExistingInfra.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployConfigAutomateSettingsExistingInfra.RootCA)
	assert.Equal(t, "8080", haDeployConfigAutomateSettingsExistingInfra.TeamsPort)

	// Assert the copied CertsByIP values from existingInfraConfig.Automate.Config
	assert.Len(t, (*haDeployConfigAutomateSettingsExistingInfra.CertsByIP), 1)
	assert.Equal(t, "10.0.0.1", (*haDeployConfigAutomateSettingsExistingInfra.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfigAutomateSettingsExistingInfra.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfigAutomateSettingsExistingInfra.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfigAutomateSettingsExistingInfra.CertsByIP)[0].NodesDn)

}

func TestCopyChefServerSettings(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}

	existingInfraConfig := &ExistingInfraConfigToml{
		ChefServer: ChefServerToml{
			Config: ChefServerConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
	}

	awsConfig := &AwsConfigToml{
		ChefServer: ChefServerToml{
			Config: ChefServerConfigToml{
				EnableCustomCerts: false,
				InstanceCount:     "5",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
			},
		},
	}

	// Test when awsConfig is not nil
	haDeployConfig = CopyChefServerSettings(haDeployConfig, nil, awsConfig)
	haDeployChefServerSettingsAws := haDeployConfig.ChefServer.Config

	// Assert the copied values from awsConfig.ChefServer.Config
	assert.Equal(t, false, haDeployChefServerSettingsAws.EnableCustomCerts)
	assert.Equal(t, "5", haDeployChefServerSettingsAws.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployChefServerSettingsAws.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployChefServerSettingsAws.PublicKey)

	// Test when awsConfig is nil
	haDeployConfig = CopyChefServerSettings(haDeployConfig, existingInfraConfig, nil)
	haDeployChefServerSettingsExistingInfra := haDeployConfig.ChefServer.Config
	// Assert the copied values from existingInfraConfig.ChefServer.Config
	assert.Equal(t, true, haDeployChefServerSettingsExistingInfra.EnableCustomCerts)
	assert.Equal(t, "3", haDeployChefServerSettingsExistingInfra.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployChefServerSettingsExistingInfra.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployChefServerSettingsExistingInfra.PublicKey)

	// Assert the copied CertsByIP values from existingInfraConfig.ChefServer.Config
	assert.Len(t, *haDeployChefServerSettingsExistingInfra.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployChefServerSettingsExistingInfra.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployChefServerSettingsExistingInfra.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployChefServerSettingsExistingInfra.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployChefServerSettingsExistingInfra.CertsByIP)[0].NodesDn)

}

func TestCopyPostgresqlSettings(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}

	existingInfraConfig := &ExistingInfraConfigToml{
		Postgresql: PostgresqlToml{
			Config: PgConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
	}

	awsConfig := &AwsConfigToml{
		Postgresql: PostgresqlToml{
			Config: PgConfigToml{
				EnableCustomCerts: false,
				InstanceCount:     "5",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
				RootCA:            "/aws/root/ca",
			},
		},
	}

	// Test when awsConfig is not nil
	haDeployConfig = CopyPostgresqlSettings(haDeployConfig, nil, awsConfig)
	haDeployPostgresqlSettingsAws := haDeployConfig.Postgresql.Config
	// Assert the copied values from awsConfig.Postgresql.Config
	assert.Equal(t, false, haDeployPostgresqlSettingsAws.EnableCustomCerts)
	assert.Equal(t, "5", haDeployPostgresqlSettingsAws.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployPostgresqlSettingsAws.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployPostgresqlSettingsAws.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployPostgresqlSettingsAws.RootCA)

	// Test when awsConfig is nil
	haDeployConfig = CopyPostgresqlSettings(haDeployConfig, existingInfraConfig, nil)
	haDeployPostgresqlSettingsExistingInfra := haDeployConfig.Postgresql.Config
	// Assert the copied values from existingInfraConfig.Postgresql.Config
	assert.Equal(t, true, haDeployPostgresqlSettingsExistingInfra.EnableCustomCerts)
	assert.Equal(t, "3", haDeployPostgresqlSettingsExistingInfra.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployPostgresqlSettingsExistingInfra.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployPostgresqlSettingsExistingInfra.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployPostgresqlSettingsExistingInfra.RootCA)

	// Assert the copied CertsByIP values from existingInfraConfig.Postgresql.Config
	assert.Len(t, *haDeployPostgresqlSettingsExistingInfra.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployPostgresqlSettingsExistingInfra.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployPostgresqlSettingsExistingInfra.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployPostgresqlSettingsExistingInfra.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployPostgresqlSettingsExistingInfra.CertsByIP)[0].NodesDn)

}

func TestCopyOpensearchSettings(t *testing.T) {
	// Prepare test data
	haDeployConfig := &config.HaDeployConfig{}

	existingInfraConfig := &ExistingInfraConfigToml{
		Opensearch: OpensearchToml{
			Config: OsConfigToml{
				AdminCert:         "/existing/admin/cert",
				AdminDn:           "/existing/admin/dn",
				AdminKey:          "/existing/admin/key",
				EnableCustomCerts: true,
				InstanceCount:     "3",
				NodesDn:           "/existing/nodes/dn",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
	}

	awsConfig := &AwsConfigToml{
		Opensearch: OpensearchToml{
			Config: OsConfigToml{
				AdminCert:         "/aws/admin/cert",
				AdminDn:           "/aws/admin/dn",
				AdminKey:          "/aws/admin/key",
				EnableCustomCerts: false,
				InstanceCount:     "5",
				NodesDn:           "/aws/nodes/dn",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
				RootCA:            "/aws/root/ca",
			},
		},
	}

	// Test when awsConfig is not nil
	haDeployConfig = CopyOpensearchSettings(haDeployConfig, nil, awsConfig)
	haDeployOpensearchSettingsAws := haDeployConfig.Opensearch.Config
	// Assert the copied values from awsConfig.Opensearch.Config
	assert.Equal(t, "/aws/admin/cert", haDeployOpensearchSettingsAws.AdminCert)
	assert.Equal(t, "/aws/admin/dn", haDeployOpensearchSettingsAws.AdminDn)
	assert.Equal(t, "/aws/admin/key", haDeployOpensearchSettingsAws.AdminKey)
	assert.Equal(t, false, haDeployOpensearchSettingsAws.EnableCustomCerts)
	assert.Equal(t, "5", haDeployOpensearchSettingsAws.InstanceCount)
	assert.Equal(t, "/aws/nodes/dn", haDeployOpensearchSettingsAws.NodesDn)
	assert.Equal(t, "/aws/private/key", haDeployOpensearchSettingsAws.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployOpensearchSettingsAws.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployOpensearchSettingsAws.RootCA)

	// Test when awsConfig is nil
	haDeployConfig = CopyOpensearchSettings(haDeployConfig, existingInfraConfig, nil)
	haDeployOpensearchSettingsExistingInfra := haDeployConfig.Opensearch.Config
	// Assert the copied values from existingInfraConfig.Opensearch.Config
	assert.Equal(t, "/existing/admin/cert", haDeployOpensearchSettingsExistingInfra.AdminCert)
	assert.Equal(t, "/existing/admin/dn", haDeployOpensearchSettingsExistingInfra.AdminDn)
	assert.Equal(t, "/existing/admin/key", haDeployOpensearchSettingsExistingInfra.AdminKey)
	assert.Equal(t, true, haDeployOpensearchSettingsExistingInfra.EnableCustomCerts)
	assert.Equal(t, "3", haDeployOpensearchSettingsExistingInfra.InstanceCount)
	assert.Equal(t, "/existing/nodes/dn", haDeployOpensearchSettingsExistingInfra.NodesDn)
	assert.Equal(t, "/existing/private/key", haDeployOpensearchSettingsExistingInfra.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployOpensearchSettingsExistingInfra.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployOpensearchSettingsExistingInfra.RootCA)

	// Assert the copied CertsByIP values from existingInfraConfig.Opensearch.Config
	assert.Len(t, *haDeployOpensearchSettingsExistingInfra.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployOpensearchSettingsExistingInfra.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployOpensearchSettingsExistingInfra.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployOpensearchSettingsExistingInfra.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployOpensearchSettingsExistingInfra.CertsByIP)[0].NodesDn)
}

func TestCopyExistingInfraSettings(t *testing.T) {
	// Create a sample existing infra configuration

	existingInfraConfig := &ExistingInfraConfigToml{
		ExistingInfra: ExistingInfraToml{
			Config: ExistingInfraIpsToml{
				AutomatePrivateIps:   []string{"10.0.0.1", "10.0.0.2"},
				ChefServerPrivateIps: []string{"10.0.1.1", "10.0.1.2"},
				PostgresqlPrivateIps: []string{"10.0.2.1", "10.0.2.2"},
				OpensearchPrivateIps: []string{"10.0.3.1", "10.0.3.2"},
			},
		},
	}
	haDeployConfig := &config.HaDeployConfig{}
	// Call the CopyExistingInfraSettings function
	haDeployConfig = CopyExistingInfraSettings(haDeployConfig, existingInfraConfig)
	haDeployExistingInfraSettings := haDeployConfig.ExistingInfra.Config
	// Verify the copied values
	assert.Equal(t, []string{"10.0.0.1", "10.0.0.2"}, haDeployExistingInfraSettings.AutomatePrivateIps, "AutomatePrivateIps does not match")
	assert.Equal(t, []string{"10.0.1.1", "10.0.1.2"}, haDeployExistingInfraSettings.ChefServerPrivateIps, "ChefServerPrivateIps does not match")
	assert.Equal(t, []string{"10.0.2.1", "10.0.2.2"}, haDeployExistingInfraSettings.PostgresqlPrivateIps, "PostgresqlPrivateIps  does not match")
	assert.Equal(t, []string{"10.0.3.1", "10.0.3.2"}, haDeployExistingInfraSettings.OpensearchPrivateIps, "OpensearchPrivateIps  does not match")
}

func TestCopyExternalOsSettings(t *testing.T) {
	// Create a sample existing infra configuration

	existingInfraConfig := &ExistingInfraConfigToml{
		ExternalDB: ExternalDBToml{
			Database: ExternalDbToml{
				Opensearch: ExternalOpensearchToml{
					OpensearchRootCert:          "root-cert",
					OpensearchDomainName:        "domain",
					OpensearchInstanceURL:       "instance-url",
					OpensearchSuperUserPassword: "superuser-password",
					OpensearchSuperUserName:     "superuser-name",
					AWS:                         ExternalAwsToml{AwsOsSnapshotRoleArn: "snapshot-role-arn", OsUserAccessKeyId: "access-key-id", OsUserAccessKeySecret: "access-key-secret"},
				},
			},
		},
	}

	// Call the CopyExternalOsSettings function
	haDeployConfig := &config.HaDeployConfig{}
	haDeployConfig = CopyExternalOsSettings(haDeployConfig, existingInfraConfig)
	haDeployExternalOsSettings := haDeployConfig.External.Database.OpenSearch
	// Verify the copied values
	assert.Equal(t, "root-cert", haDeployExternalOsSettings.OpensearchRootCert, "OpensearchRootCert does not match")
	assert.Equal(t, "domain", haDeployExternalOsSettings.OpensearchDomainName, "OpensearchDomainName does not match")
	assert.Equal(t, "instance-url", haDeployExternalOsSettings.OpensearchDomainURL, "OpensearchDomainURL does not match")
	assert.Equal(t, "superuser-password", haDeployExternalOsSettings.OpensearchUserPassword, "OpensearchUserPassword does not match")
	assert.Equal(t, "superuser-name", haDeployExternalOsSettings.OpensearchUsername, "OpensearchUsername does not match")
	assert.Equal(t, "snapshot-role-arn", haDeployExternalOsSettings.Aws.AwsOsSnapshotRoleArn, "AwsOsSnapshotRoleArn does not match")
	assert.Equal(t, "access-key-id", haDeployExternalOsSettings.Aws.OsSnapshotUserAccessKeyID, "OsSnapshotUserAccessKeyID does not match")
	assert.Equal(t, "access-key-secret", haDeployExternalOsSettings.Aws.OsSnapshotUserAccessKeySecret, "OsSnapshotUserAccessKeySecret does not match")
}

func TestCopyExternalPgSettings(t *testing.T) {
	// Create a sample existing infra configuration
	existingInfraConfig := &ExistingInfraConfigToml{
		ExternalDB: ExternalDBToml{
			Database: ExternalDbToml{
				PostgreSQL: ExternalPostgreSQLToml{
					PostgreSQLDBUserPassword:    "dbuser-password",
					PostgreSQLDBUserName:        "dbuser-name",
					PostgreSQLInstanceURL:       "instance-url",
					PostgreSQLRootCert:          "root-cert",
					PostgreSQLSuperUserPassword: "superuser-password",
					PostgreSQLSuperUserName:     "superuser-name",
				},
			},
		},
	}

	haDeployConfig := &config.HaDeployConfig{}

	// Call the CopyExternalPgSettings function
	haDeployConfig = CopyExternalPgSettings(haDeployConfig, existingInfraConfig)
	haDeployExternalPgSettings := haDeployConfig.External.Database.PostgreSQL
	// Verify the copied values
	assert.Equal(t, "dbuser-password", haDeployExternalPgSettings.DbuserPassword, "DbuserPassword does not match")
	assert.Equal(t, "dbuser-name", haDeployExternalPgSettings.DbuserUsername, "DbuserUsername does not match")
	assert.Equal(t, "instance-url", haDeployExternalPgSettings.InstanceURL, "InstanceURL does not match")
	assert.Equal(t, "root-cert", haDeployExternalPgSettings.PostgresqlRootCert, "PostgresqlRootCert does not match")
	assert.Equal(t, "superuser-password", haDeployExternalPgSettings.SuperuserPassword, "SuperuserPassword does not match")
	assert.Equal(t, "superuser-name", haDeployExternalPgSettings.SuperuserUsername, "SuperuserUsername does not match")

}

func TestCopyAws(t *testing.T) {
	// Call the CopyAws function
	haDeployConfig := &config.HaDeployConfig{}

	awsConfig := &AwsConfigToml{
		Architecture: AwsArchitectureToml{
			ConfigInitials: AwsConfigInitialsToml{
				SSHUser:                     "aws-ssh-user",
				Architecture:                "aws-architecture",
				BackupConfig:                "s3",
				S3BucketName:                "aws-s3-bucket",
				BackupMount:                 "/aws/backup/mount",
				HabitatUIDGid:               "aws-habitat-uid-gid",
				LoggingMonitoringManagement: "false",
				SSHGroupName:                "aws-ssh-group",
				SSHKeyFile:                  "/aws/ssh/key/file",
				SSHPort:                     "2222",
				SecretsKeyFile:              "/aws/secrets/key/file",
				SecretsStoreFile:            "/aws/secrets/store/file",
				WorkspacePath:               "/aws/workspace/path",
			},
		},
		Automate: AwsAutomateToml{
			Config: AwsAutomateConfigToml{
				AdminPassword:     "aws-admin-password",
				ConfigFile:        "/aws/config/file",
				EnableCustomCerts: false,
				Fqdn:              "aws-fqdn",
				InstanceCount:     "5",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
				RootCA:            "/aws/root/ca",
				TeamsPort:         "9090",
			},
		},
		ChefServer: ChefServerToml{
			Config: ChefServerConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
			},
		},
		Postgresql: PostgresqlToml{
			Config: PgConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
			},
		},
		Opensearch: OpensearchToml{
			Config: OsConfigToml{
				AdminCert:         "/existing/admin/cert",
				AdminDn:           "/existing/admin/dn",
				AdminKey:          "/existing/admin/key",
				EnableCustomCerts: true,
				InstanceCount:     "3",
				NodesDn:           "/existing/nodes/dn",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
			},
		},
		Aws: AwsToml{
			Config: ConfigToml{
				Profile:                      "my-profile",
				Region:                       "us-west-2",
				AwsVpcId:                     "vpc-12345",
				AwsCidrBlockAddr:             "10.0.0.0/16",
				PrivateCustomSubnets:         []string{"subnet-12345", "subnet-67890"},
				PublicCustomSubnets:          []string{"subnet-54321", "subnet-09876"},
				SSHKeyPairName:               "my-keypair",
				AmiID:                        "ami-08d4ac5b634553e16",
				DeleteOnTermination:          true,
				SetupManagedServices:         true,
				AutomateServerInstanceType:   "t3.medium",
				ChefServerInstanceType:       "t3.medium",
				PostgresqlServerInstanceType: "m5.large",
				OpensearchServerInstanceType: "m5.large",
				AutomateLbCertificateArn:     "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				ChefServerLbCertificateArn:   "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				AutomateEbsVolumeIops:        "100",
				AutomateEbsVolumeSize:        "50",
				AutomateEbsVolumeType:        "gp3",
				ChefEbsVolumeIops:            "100",
				ChefEbsVolumeSize:            "50",
				ChefEbsVolumeType:            "gp3",
				OpensearchEbsVolumeIops:      "100",
				OpensearchEbsVolumeSize:      "50",
				OpensearchEbsVolumeType:      "gp3",
				PostgresqlEbsVolumeIops:      "100",
				PostgresqlEbsVolumeSize:      "50",
				PostgresqlEbsVolumeType:      "gp3",
				LBAccessLogs:                 "false",
				OpensearchDomainName:         "opensearch-domain",
				OpensearchDomainUrl:          "opensearch-url",
				OpensearchUserPassword:       "opensearch-password",
				OpensearchUsername:           "opensearch-username",
				OpensearchCertificate:        "opensearch-certificate",
				OsUserAccessKeyId:            "access-key-id",
				OsUserAccessKeySecret:        "access-key-secret",
				AwsOsSnapshotRoleArn:         "snapshot-role-arn",
				RDSCertificate:               "rds-certificate",
				RDSDBUserPassword:            "rds-db-password",
				RDSDBUserName:                "rds-db-username",
				RDSInstanceUrl:               "rds-instance-url",
				RDSSuperUserPassword:         "rds-superuser-password",
				RDSSuperUserName:             "rds-superuser-username",
			},
		},
	}

	haDeployConfig = CopyAws(haDeployConfig, awsConfig)

	// Verify the copied values in haDeployConfig
	arch := haDeployConfig.Architecture
	assert.Equal(t, "aws-ssh-user", arch.Aws.SSHUser)
	assert.Equal(t, "aws-architecture", arch.Aws.Architecture)
	assert.Equal(t, "s3", arch.Aws.BackupConfig)
	assert.Equal(t, "aws-s3-bucket", arch.Aws.S3BucketName)
	assert.Equal(t, "/aws/backup/mount", arch.Aws.BackupMount)
	assert.Equal(t, "aws-habitat-uid-gid", arch.Aws.HabitatUIDGid)
	assert.Equal(t, "false", arch.Aws.LoggingMonitoringManagement)
	assert.Equal(t, "aws-ssh-group", arch.Aws.SSHGroupName)
	assert.Equal(t, "/aws/ssh/key/file", arch.Aws.SSHKeyFile)
	assert.Equal(t, "2222", arch.Aws.SSHPort)
	assert.Equal(t, "/aws/secrets/key/file", arch.Aws.SecretsKeyFile)
	assert.Equal(t, "/aws/secrets/store/file", arch.Aws.SecretsStoreFile)
	assert.Equal(t, "/aws/workspace/path", arch.Aws.WorkspacePath)

	automate := haDeployConfig.Automate.Config
	assert.Equal(t, "aws-admin-password", automate.AdminPassword)
	assert.Equal(t, "/aws/config/file", automate.ConfigFile)
	assert.Equal(t, false, automate.EnableCustomCerts)
	assert.Equal(t, "aws-fqdn", automate.Fqdn)
	assert.Equal(t, "5", automate.InstanceCount)
	assert.Equal(t, "/aws/private/key", automate.PrivateKey)
	assert.Equal(t, "/aws/public/key", automate.PublicKey)
	assert.Equal(t, "/aws/root/ca", automate.RootCA)
	assert.Equal(t, "9090", automate.TeamsPort)

	chefServer := haDeployConfig.ChefServer.Config
	assert.Equal(t, true, chefServer.EnableCustomCerts)
	assert.Equal(t, "3", chefServer.InstanceCount)
	assert.Equal(t, "/existing/private/key", chefServer.PrivateKey)
	assert.Equal(t, "/existing/public/key", chefServer.PublicKey)

	postgresql := haDeployConfig.Postgresql.Config
	assert.Equal(t, true, postgresql.EnableCustomCerts)
	assert.Equal(t, "3", postgresql.InstanceCount)
	assert.Equal(t, "/existing/private/key", postgresql.PrivateKey)
	assert.Equal(t, "/existing/public/key", postgresql.PublicKey)
	assert.Equal(t, "/existing/root/ca", postgresql.RootCA)

	opensearch := haDeployConfig.Opensearch.Config
	assert.Equal(t, "/existing/admin/cert", opensearch.AdminCert)
	assert.Equal(t, "/existing/admin/dn", opensearch.AdminDn)
	assert.Equal(t, "/existing/admin/key", opensearch.AdminKey)
	assert.Equal(t, true, opensearch.EnableCustomCerts)
	assert.Equal(t, "3", opensearch.InstanceCount)
	assert.Equal(t, "/existing/nodes/dn", opensearch.NodesDn)
	assert.Equal(t, "/existing/private/key", opensearch.PrivateKey)
	assert.Equal(t, "/existing/public/key", opensearch.PublicKey)
	assert.Equal(t, "/existing/root/ca", opensearch.RootCA)

	// aws := haDeployConfig.Aws.Config
	// assert.Equal(t, "my-profile", aws.Profile)
	// assert.Equal(t, "us-west-2", aws.Region)
	// assert.Equal(t, "vpc-12345", aws.AwsVpcID)
	// assert.Equal(t, "10.0.0.0/16", aws.AwsCidrBlockAddr)
	// assert.Equal(t, []string{"subnet-12345", "subnet-67890"}, aws.PrivateCustomSubnets)
	// assert.Equal(t, []string{"subnet-54321", "subnet-09876"}, aws.PublicCustomSubnets)
	// assert.Equal(t, "my-keypair", aws.SSHKeyPairName)
	// assert.Equal(t, "ami-08d4ac5b634553e16", aws.AmiID)
	// assert.Equal(t, true, aws.DeleteOnTermination)
	// assert.Equal(t, "t3.medium", aws.AutomateServerInstanceType)
	// assert.Equal(t, "t3.medium", aws.ChefServerInstanceType)
	// assert.Equal(t, "m5.large", aws.PostgresqlServerInstanceType)
	// assert.Equal(t, "m5.large", aws.OpensearchServerInstanceType)
	// assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.AutomateLbCertificateArn)
	// assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.ChefServerLbCertificateArn)
	// assert.Equal(t, "100", aws.AutomateEbsVolumeIops)
	// assert.Equal(t, "50", aws.AutomateEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.AutomateEbsVolumeType)
	// assert.Equal(t, "100", aws.ChefEbsVolumeIops)
	// assert.Equal(t, "50", aws.ChefEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.ChefEbsVolumeType)
	// assert.Equal(t, "100", aws.OpensearchEbsVolumeIops)
	// assert.Equal(t, "50", aws.OpensearchEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.OpensearchEbsVolumeType)
	// assert.Equal(t, "100", aws.PostgresqlEbsVolumeIops)
	// assert.Equal(t, "50", aws.PostgresqlEbsVolumeSize)
	// assert.Equal(t, "gp3", aws.PostgresqlEbsVolumeType)
	// assert.Equal(t, "false", aws.LbAccessLogs)
	// assert.Equal(t, "opensearch-domain", aws.ManagedOpensearchDomainName)
	// assert.Equal(t, "opensearch-url", aws.ManagedOpensearchDomainURL)
	// assert.Equal(t, "opensearch-password", aws.ManagedOpensearchUserPassword)
	// assert.Equal(t, "opensearch-username", aws.ManagedOpensearchUsername)
	// assert.Equal(t, "opensearch-certificate", aws.ManagedOpensearchCertificate)
	// assert.Equal(t, "access-key-id", aws.OsSnapshotUserAccessKeyID)
	// assert.Equal(t, "access-key-secret", aws.OsSnapshotUserAccessKeySecret)
	// assert.Equal(t, "snapshot-role-arn", aws.AwsOsSnapshotRoleArn)
	// assert.Equal(t, "rds-certificate", aws.ManagedRdsCertificate)
	// assert.Equal(t, "rds-db-password", aws.ManagedRdsDbuserPassword)
	// assert.Equal(t, "rds-db-username", aws.ManagedRdsDbuserUsername)
	// assert.Equal(t, "rds-instance-url", aws.ManagedRdsInstanceURL)
	// assert.Equal(t, "rds-superuser-password", aws.ManagedRdsSuperuserPassword)
	// assert.Equal(t, "rds-superuser-username", aws.ManagedRdsSuperuserUsername)
}
