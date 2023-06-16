package main

import (
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/stretchr/testify/assert"
)

var awsEc2InstanceConfig = &AwsConfigToml{
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

var awsManagedServicesConfig = &AwsConfigToml{
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

var awsNetworkConfig = &AwsConfigToml{
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
	var awsConfigSetting = &config.ConfigAwsSettings{}

	// Call the function
	CopyEc2InstanceConfig(awsConfigSetting, awsEc2InstanceConfig)

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

	haConfigAws := &config.ConfigAwsSettings{}

	CopyManagedServices(haConfigAws, awsManagedServicesConfig)

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
	haConfigAws := &config.ConfigAwsSettings{}

	CopyAwsNetworkConfig(haConfigAws, awsNetworkConfig)

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
	haConfigAws := &config.ConfigAwsSettings{}
	// Test copying Managed Services
	awsManagedServicesConfig.Aws.Config.SetupManagedServices = true
	CopyAwsConfig(haConfigAws, awsManagedServicesConfig)
	assert.Equal(t, awsManagedServicesConfig.Aws.Config.RDSSuperUserPassword, haConfigAws.ManagedRdsSuperuserPassword)
}

func TestCopyConfigInitials(t *testing.T) {
	// Prepare test data
	haDeployConfigConfigInitials := &config.ConfigInitials{}
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
	CopyConfigInitials(haDeployConfigConfigInitials, nil, awsConfig)

	// Assert the copied values from awsConfig.ConfigInitials
	assert.Equal(t, "aws-ssh-user", haDeployConfigConfigInitials.SSHUser)
	assert.Equal(t, "aws-architecture", haDeployConfigConfigInitials.Architecture)
	assert.Equal(t, "s3", haDeployConfigConfigInitials.BackupConfig)
	assert.Equal(t, "aws-s3-bucket", haDeployConfigConfigInitials.S3BucketName)
	assert.Equal(t, "/aws/backup/mount", haDeployConfigConfigInitials.BackupMount)
	assert.Equal(t, "aws-habitat-uid-gid", haDeployConfigConfigInitials.HabitatUIDGid)
	assert.Equal(t, "false", haDeployConfigConfigInitials.LoggingMonitoringManagement)
	assert.Equal(t, "aws-ssh-group", haDeployConfigConfigInitials.SSHGroupName)
	assert.Equal(t, "/aws/ssh/key/file", haDeployConfigConfigInitials.SSHKeyFile)
	assert.Equal(t, "2222", haDeployConfigConfigInitials.SSHPort)
	assert.Equal(t, "/aws/secrets/key/file", haDeployConfigConfigInitials.SecretsKeyFile)
	assert.Equal(t, "/aws/secrets/store/file", haDeployConfigConfigInitials.SecretsStoreFile)
	assert.Equal(t, "/aws/workspace/path", haDeployConfigConfigInitials.WorkspacePath)
	assert.Equal(t, "", haDeployConfigConfigInitials.SudoPassword)

	// Test when awsConfig is nil
	CopyConfigInitials(haDeployConfigConfigInitials, existingInfraConfig, nil)

	// Assert the copied values from existingInfraConfig.ConfigInitials
	assert.Equal(t, "existing-ssh-user", haDeployConfigConfigInitials.SSHUser)
	assert.Equal(t, "existing-architecture", haDeployConfigConfigInitials.Architecture)
	assert.Equal(t, "object_storage", haDeployConfigConfigInitials.BackupConfig)
	assert.Equal(t, "/existing/backup/mount", haDeployConfigConfigInitials.BackupMount)
	assert.Equal(t, "existing-habitat-uid-gid", haDeployConfigConfigInitials.HabitatUIDGid)
	assert.Equal(t, "true", haDeployConfigConfigInitials.LoggingMonitoringManagement)
	assert.Equal(t, "existing-ssh-group", haDeployConfigConfigInitials.SSHGroupName)
	assert.Equal(t, "/existing/ssh/key/file", haDeployConfigConfigInitials.SSHKeyFile)
	assert.Equal(t, "22", haDeployConfigConfigInitials.SSHPort)
	assert.Equal(t, "/existing/secrets/key/file", haDeployConfigConfigInitials.SecretsKeyFile)
	assert.Equal(t, "/existing/secrets/store/file", haDeployConfigConfigInitials.SecretsStoreFile)
	assert.Equal(t, "/existing/workspace/path", haDeployConfigConfigInitials.WorkspacePath)
	assert.Equal(t, "", haDeployConfigConfigInitials.SudoPassword)
}

func TestCopyConfigObjectStorage(t *testing.T) {
	// Prepare test data
	haDeployConfigObjectStorageConfig := &config.ConfigObjectStorage{}
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
	CopyConfigObjectStorage(haDeployConfigObjectStorageConfig, existingInfraConfig)

	// Assert the copied values
	assert.Equal(t, "existing-access-key", haDeployConfigObjectStorageConfig.AccessKey)
	assert.Equal(t, "existing-bucket", haDeployConfigObjectStorageConfig.BucketName)
	assert.Equal(t, "existing-endpoint", haDeployConfigObjectStorageConfig.Endpoint)
	assert.Equal(t, "existing-region", haDeployConfigObjectStorageConfig.Region)
	assert.Equal(t, "existing-secret-key", haDeployConfigObjectStorageConfig.SecretKey)
}

func TestCopyAutomateSettings(t *testing.T) {
	// Prepare test data
	haDeployConfigAutomateSettings := &config.ConfigAutomateSettings{}
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
	CopyAutomateSettings(haDeployConfigAutomateSettings, nil, awsConfig)

	// Assert the copied values from awsConfig.Automate.Config
	assert.Equal(t, "aws-admin-password", haDeployConfigAutomateSettings.AdminPassword)
	assert.Equal(t, "/aws/config/file", haDeployConfigAutomateSettings.ConfigFile)
	assert.Equal(t, false, haDeployConfigAutomateSettings.EnableCustomCerts)
	assert.Equal(t, "aws-fqdn", haDeployConfigAutomateSettings.Fqdn)
	assert.Equal(t, "5", haDeployConfigAutomateSettings.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployConfigAutomateSettings.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployConfigAutomateSettings.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployConfigAutomateSettings.RootCA)
	assert.Equal(t, "9090", haDeployConfigAutomateSettings.TeamsPort)

	// Test when awsConfig is nil
	CopyAutomateSettings(haDeployConfigAutomateSettings, existingInfraConfig, nil)

	// Assert the copied values from existingInfraConfig.Automate.Config
	assert.Equal(t, "existing-admin-password", haDeployConfigAutomateSettings.AdminPassword)
	assert.Equal(t, "/existing/config/file", haDeployConfigAutomateSettings.ConfigFile)
	assert.Equal(t, true, haDeployConfigAutomateSettings.EnableCustomCerts)
	assert.Equal(t, "existing-fqdn", haDeployConfigAutomateSettings.Fqdn)
	assert.Equal(t, "3", haDeployConfigAutomateSettings.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployConfigAutomateSettings.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfigAutomateSettings.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployConfigAutomateSettings.RootCA)
	assert.Equal(t, "8080", haDeployConfigAutomateSettings.TeamsPort)

	// Assert the copied CertsByIP values from existingInfraConfig.Automate.Config
	assert.Len(t, (*haDeployConfigAutomateSettings.CertsByIP), 1)
	assert.Equal(t, "10.0.0.1", (*haDeployConfigAutomateSettings.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfigAutomateSettings.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfigAutomateSettings.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfigAutomateSettings.CertsByIP)[0].NodesDn)

}

func TestCopyChefServerSettings(t *testing.T) {
	// Prepare test data
	haDeployChefServerSettings := &config.ConfigSettings{}
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
	CopyChefServerSettings(haDeployChefServerSettings, nil, awsConfig)

	// Assert the copied values from awsConfig.ChefServer.Config
	assert.Equal(t, false, haDeployChefServerSettings.EnableCustomCerts)
	assert.Equal(t, "5", haDeployChefServerSettings.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployChefServerSettings.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployChefServerSettings.PublicKey)

	// Test when awsConfig is nil
	CopyChefServerSettings(haDeployChefServerSettings, existingInfraConfig, nil)

	// Assert the copied values from existingInfraConfig.ChefServer.Config
	assert.Equal(t, true, haDeployChefServerSettings.EnableCustomCerts)
	assert.Equal(t, "3", haDeployChefServerSettings.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployChefServerSettings.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployChefServerSettings.PublicKey)

	// Assert the copied CertsByIP values from existingInfraConfig.ChefServer.Config
	assert.Len(t, *haDeployChefServerSettings.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployChefServerSettings.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployChefServerSettings.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployChefServerSettings.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployChefServerSettings.CertsByIP)[0].NodesDn)
}

func TestCopyPostgresqlSettings(t *testing.T) {
	// Prepare test data
	haDeployPostgresqlSettings := &config.ConfigSettings{}
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
	CopyPostgresqlSettings(haDeployPostgresqlSettings, nil, awsConfig)

	// Assert the copied values from awsConfig.Postgresql.Config
	assert.Equal(t, false, haDeployPostgresqlSettings.EnableCustomCerts)
	assert.Equal(t, "5", haDeployPostgresqlSettings.InstanceCount)
	assert.Equal(t, "/aws/private/key", haDeployPostgresqlSettings.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployPostgresqlSettings.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployPostgresqlSettings.RootCA)

	// Test when awsConfig is nil
	CopyPostgresqlSettings(haDeployPostgresqlSettings, existingInfraConfig, nil)

	// Assert the copied values from existingInfraConfig.Postgresql.Config
	assert.Equal(t, true, haDeployPostgresqlSettings.EnableCustomCerts)
	assert.Equal(t, "3", haDeployPostgresqlSettings.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployPostgresqlSettings.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployPostgresqlSettings.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployPostgresqlSettings.RootCA)

	// Assert the copied CertsByIP values from existingInfraConfig.Postgresql.Config
	assert.Len(t, *haDeployPostgresqlSettings.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployPostgresqlSettings.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployPostgresqlSettings.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployPostgresqlSettings.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployPostgresqlSettings.CertsByIP)[0].NodesDn)
}

func TestCopyOpensearchSettings(t *testing.T) {
	// Prepare test data
	haDeployOpensearchSettings := &config.ConfigOpensearchSettings{}
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
	CopyOpensearchSettings(haDeployOpensearchSettings, nil, awsConfig)

	// Assert the copied values from awsConfig.Opensearch.Config
	assert.Equal(t, "/aws/admin/cert", haDeployOpensearchSettings.AdminCert)
	assert.Equal(t, "/aws/admin/dn", haDeployOpensearchSettings.AdminDn)
	assert.Equal(t, "/aws/admin/key", haDeployOpensearchSettings.AdminKey)
	assert.Equal(t, false, haDeployOpensearchSettings.EnableCustomCerts)
	assert.Equal(t, "5", haDeployOpensearchSettings.InstanceCount)
	assert.Equal(t, "/aws/nodes/dn", haDeployOpensearchSettings.NodesDn)
	assert.Equal(t, "/aws/private/key", haDeployOpensearchSettings.PrivateKey)
	assert.Equal(t, "/aws/public/key", haDeployOpensearchSettings.PublicKey)
	assert.Equal(t, "/aws/root/ca", haDeployOpensearchSettings.RootCA)

	// Test when awsConfig is nil
	CopyOpensearchSettings(haDeployOpensearchSettings, existingInfraConfig, nil)

	// Assert the copied values from existingInfraConfig.Opensearch.Config
	assert.Equal(t, "/existing/admin/cert", haDeployOpensearchSettings.AdminCert)
	assert.Equal(t, "/existing/admin/dn", haDeployOpensearchSettings.AdminDn)
	assert.Equal(t, "/existing/admin/key", haDeployOpensearchSettings.AdminKey)
	assert.Equal(t, true, haDeployOpensearchSettings.EnableCustomCerts)
	assert.Equal(t, "3", haDeployOpensearchSettings.InstanceCount)
	assert.Equal(t, "/existing/nodes/dn", haDeployOpensearchSettings.NodesDn)
	assert.Equal(t, "/existing/private/key", haDeployOpensearchSettings.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployOpensearchSettings.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployOpensearchSettings.RootCA)

	// Assert the copied CertsByIP values from existingInfraConfig.Opensearch.Config
	assert.Len(t, *haDeployOpensearchSettings.CertsByIP, 1)
	assert.Equal(t, "10.0.0.1", (*haDeployOpensearchSettings.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployOpensearchSettings.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployOpensearchSettings.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployOpensearchSettings.CertsByIP)[0].NodesDn)
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
		ExternalDB: ExternalDBToml{
			Database: ExternalDbToml{
				Type: "aws",
			},
		},
	}

	// Create a sample HA deploy existing infra settings
	haDeployExistingInfraSettings := &config.ConfigExistingInfraSettings{}

	// Call the CopyExistingInfraSettings function
	CopyExistingInfraSettings(haDeployExistingInfraSettings, existingInfraConfig)

	// Verify the copied values
	assert.Equal(t, []string{"10.0.0.1", "10.0.0.2"}, haDeployExistingInfraSettings.AutomatePrivateIps, "AutomatePrivateIps does not match")
	assert.Equal(t, []string{"10.0.1.1", "10.0.1.2"}, haDeployExistingInfraSettings.ChefServerPrivateIps, "ChefServerPrivateIps does not match")
	assert.Nil(t, haDeployExistingInfraSettings.PostgresqlPrivateIps, "PostgresqlPrivateIps should be nil")
	assert.Nil(t, haDeployExistingInfraSettings.OpensearchPrivateIps, "OpensearchPrivateIps should be nil")

	// Test case when ExternalDB is set to self-managed
	existingInfraConfig.ExternalDB.Database.Type = "self-managed"
	CopyExistingInfraSettings(haDeployExistingInfraSettings, existingInfraConfig)

	// Verify that PostgresqlPrivateIps and OpensearchPrivateIps are not copied
	assert.Nil(t, haDeployExistingInfraSettings.PostgresqlPrivateIps, "PostgresqlPrivateIps should be nil when ExternalDB is set to a different type")
	assert.Nil(t, haDeployExistingInfraSettings.OpensearchPrivateIps, "OpensearchPrivateIps should be nil when ExternalDB is set to a different type")

	// Reset ExternalDB type
	existingInfraConfig.ExternalDB.Database.Type = ""
	CopyExistingInfraSettings(haDeployExistingInfraSettings, existingInfraConfig)
	// Verify the copied values
	assert.Equal(t, []string{"10.0.2.1", "10.0.2.2"}, haDeployExistingInfraSettings.PostgresqlPrivateIps, "PostgresqlPrivateIps does not match")
	assert.Equal(t, []string{"10.0.3.1", "10.0.3.2"}, haDeployExistingInfraSettings.OpensearchPrivateIps, "OpensearchPrivateIps does not match")
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

	// Create a sample HA deploy external OS settings
	haDeployExternalOsSettings := &config.ExternalOsSettings{}

	// Call the CopyExternalOsSettings function
	CopyExternalOsSettings(haDeployExternalOsSettings, existingInfraConfig)

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

	// Create a sample HA deploy external PG settings
	haDeployExternalPgSettings := &config.ExternalPgSettings{}

	// Call the CopyExternalPgSettings function
	CopyExternalPgSettings(haDeployExternalPgSettings, existingInfraConfig)

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
	var haConfig = &config.HaDeployConfig{}

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
	}

	result := CopyAws(haConfig, awsConfig)

	// Verify the copied values
	assert.Equal(t, result.Aws.Config.AmiID, awsConfig.Aws.Config.AmiID)
	assert.Equal(t, result.Aws.Config.DeleteOnTermination, awsConfig.Aws.Config.DeleteOnTermination)
	assert.Equal(t, result.Aws.Config.AutomateServerInstanceType, awsConfig.Aws.Config.AutomateServerInstanceType)
}
