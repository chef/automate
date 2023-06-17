package main

import (
	"errors"

	"github.com/chef/automate/lib/config"
)

func PopulateHaCommonConfig() (*config.HaDeployConfig, error) {
	var haConfig *config.HaDeployConfig
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return nil, err
	}

	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
	}
	sshUtil := NewSSHUtil(sshConfig)
	configPuller := NewPullConfigs(infra, sshUtil)

	existingInfraConfig, err := configPuller.fetchInfraConfig()
	if err != nil {
		return nil, err
	}
	if existingInfraConfig != nil {
		return CopyExistingInfra(haConfig, existingInfraConfig), nil
	}

	awsConfig, err := configPuller.fetchAwsConfig()
	if err != nil {
		return nil, err
	}

	if awsConfig != nil {
		return CopyAws(haConfig, awsConfig), nil
	}

	return nil, errors.New("deployed config was not found")
}

func CopyCertsByIP(haDeploy *[]config.CertByIP, existing []CertByIP) {
	for _, certByIP := range existing {
		haCertByIP := config.CertByIP{
			IP:         certByIP.IP,
			PrivateKey: certByIP.PrivateKey,
			PublicKey:  certByIP.PublicKey,
			NodesDn:    certByIP.NodesDn,
		}
		*haDeploy = append(*haDeploy, haCertByIP)
	}
}

func CopyExternalPgSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	existingInfraExternalPgSettings := existingInfraConfig.ExternalDB.Database.PostgreSQL
	haDeployConfig.External = &config.ExternalSettings{
		Database: &config.ExternalDBSettings{
			PostgreSQL: &config.ExternalPgSettings{
				DbuserPassword:     existingInfraExternalPgSettings.PostgreSQLDBUserPassword,
				DbuserUsername:     existingInfraExternalPgSettings.PostgreSQLDBUserName,
				InstanceURL:        existingInfraExternalPgSettings.PostgreSQLInstanceURL,
				PostgresqlRootCert: existingInfraExternalPgSettings.PostgreSQLRootCert,
				SuperuserPassword:  existingInfraExternalPgSettings.PostgreSQLSuperUserPassword,
				SuperuserUsername:  existingInfraExternalPgSettings.PostgreSQLSuperUserName,
			},
		},
	}
	return haDeployConfig
}

func CopyExternalOsSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	existingInfraExternalOsSettings := existingInfraConfig.ExternalDB.Database.Opensearch
	haDeployConfig.External = &config.ExternalSettings{
		Database: &config.ExternalDBSettings{
			OpenSearch: &config.ExternalOsSettings{
				OpensearchRootCert:     existingInfraExternalOsSettings.OpensearchRootCert,
				OpensearchDomainName:   existingInfraExternalOsSettings.OpensearchDomainName,
				OpensearchDomainURL:    existingInfraExternalOsSettings.OpensearchInstanceURL,
				OpensearchUserPassword: existingInfraExternalOsSettings.OpensearchSuperUserPassword,
				OpensearchUsername:     existingInfraExternalOsSettings.OpensearchSuperUserName,
				Aws: &config.AwsExternalOsSettings{
					AwsOsSnapshotRoleArn:          existingInfraExternalOsSettings.AWS.AwsOsSnapshotRoleArn,
					OsSnapshotUserAccessKeyID:     existingInfraExternalOsSettings.AWS.OsUserAccessKeyId,
					OsSnapshotUserAccessKeySecret: existingInfraExternalOsSettings.AWS.OsUserAccessKeySecret,
				},
			},
		},
	}
	return haDeployConfig
}

func CopyExistingInfraSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	existingInfraSettings := existingInfraConfig.ExistingInfra.Config
	haDeployConfig.ExistingInfra = &config.ExistingInfraSettings{
		Config: &config.ConfigExistingInfraSettings{
			AutomatePrivateIps:   existingInfraSettings.AutomatePrivateIps,
			ChefServerPrivateIps: existingInfraSettings.ChefServerPrivateIps,
			PostgresqlPrivateIps: existingInfraSettings.PostgresqlPrivateIps,
			OpensearchPrivateIps: existingInfraSettings.OpensearchPrivateIps,
		},
	}
	return haDeployConfig
}

func CopyOpensearchSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) *config.HaDeployConfig {

	if awsConfig != nil {
		awsConfigOpensearchSettings := awsConfig.Opensearch.Config
		haDeployConfig.Opensearch = &config.OpensearchSettings{
			Config: &config.ConfigOpensearchSettings{
				AdminCert:         awsConfigOpensearchSettings.AdminCert,
				AdminDn:           awsConfigOpensearchSettings.AdminDn,
				AdminKey:          awsConfigOpensearchSettings.AdminKey,
				EnableCustomCerts: awsConfigOpensearchSettings.EnableCustomCerts,
				InstanceCount:     awsConfigOpensearchSettings.InstanceCount,
				NodesDn:           awsConfigOpensearchSettings.NodesDn,
				PrivateKey:        awsConfigOpensearchSettings.PrivateKey,
				PublicKey:         awsConfigOpensearchSettings.PublicKey,
				RootCA:            awsConfigOpensearchSettings.RootCA,
			},
		}
	}

	if existingInfraConfig != nil {
		existingInfraOpensearchSettings := existingInfraConfig.Opensearch.Config
		haDeployConfig.Opensearch = &config.OpensearchSettings{
			Config: &config.ConfigOpensearchSettings{

				AdminCert:         existingInfraOpensearchSettings.AdminCert,
				AdminDn:           existingInfraOpensearchSettings.AdminDn,
				AdminKey:          existingInfraOpensearchSettings.AdminKey,
				EnableCustomCerts: existingInfraOpensearchSettings.EnableCustomCerts,
				InstanceCount:     existingInfraOpensearchSettings.InstanceCount,
				NodesDn:           existingInfraOpensearchSettings.NodesDn,
				PrivateKey:        existingInfraOpensearchSettings.PrivateKey,
				PublicKey:         existingInfraOpensearchSettings.PublicKey,
				RootCA:            existingInfraOpensearchSettings.RootCA,
			},
		}
		// CertsByIP
		if existingInfraOpensearchSettings.CertsByIP != nil {
			haDeployConfig.Opensearch.Config.CertsByIP = &[]config.CertByIP{}
			CopyCertsByIP(haDeployConfig.Opensearch.Config.CertsByIP, existingInfraOpensearchSettings.CertsByIP)
		}
	}
	return haDeployConfig
}

func CopyPostgresqlSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	if awsConfig != nil {
		awsConfigPostgresqlSettings := awsConfig.Postgresql.Config

		haDeployConfig.Postgresql = &config.PostgresqlSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: awsConfigPostgresqlSettings.EnableCustomCerts,
				InstanceCount:     awsConfigPostgresqlSettings.InstanceCount,
				PrivateKey:        awsConfigPostgresqlSettings.PrivateKey,
				PublicKey:         awsConfigPostgresqlSettings.PublicKey,
				RootCA:            awsConfigPostgresqlSettings.RootCA,
			},
		}
	}

	if existingInfraConfig != nil {
		existingInfraPostgresqlSettings := existingInfraConfig.Postgresql.Config
		haDeployConfig.Postgresql = &config.PostgresqlSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: existingInfraPostgresqlSettings.EnableCustomCerts,
				InstanceCount:     existingInfraPostgresqlSettings.InstanceCount,
				PrivateKey:        existingInfraPostgresqlSettings.PrivateKey,
				PublicKey:         existingInfraPostgresqlSettings.PublicKey,
				RootCA:            existingInfraPostgresqlSettings.RootCA},
		}
		// CertsByIP
		if existingInfraPostgresqlSettings.CertsByIP != nil {
			haDeployConfig.Postgresql.Config.CertsByIP = &[]config.CertByIP{}
			CopyCertsByIP(haDeployConfig.Postgresql.Config.CertsByIP, existingInfraPostgresqlSettings.CertsByIP)
		}
	}
	return haDeployConfig
}

func CopyChefServerSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) *config.HaDeployConfig {

	if awsConfig != nil {
		awsConfigChefServerSettings := awsConfig.ChefServer.Config
		haDeployConfig.ChefServer = &config.ChefServerSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: awsConfigChefServerSettings.EnableCustomCerts,
				InstanceCount:     awsConfigChefServerSettings.InstanceCount,
				PrivateKey:        awsConfigChefServerSettings.PrivateKey,
				PublicKey:         awsConfigChefServerSettings.PublicKey,
			},
		}
	}

	if existingInfraConfig != nil {
		existingInfraChefServerSettings := existingInfraConfig.ChefServer.Config
		haDeployConfig.ChefServer = &config.ChefServerSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: existingInfraChefServerSettings.EnableCustomCerts,
				InstanceCount:     existingInfraChefServerSettings.InstanceCount,
				PrivateKey:        existingInfraChefServerSettings.PrivateKey,
				PublicKey:         existingInfraChefServerSettings.PublicKey,
			},
		}
		// CertsByIP
		if existingInfraChefServerSettings.CertsByIP != nil {
			haDeployConfig.ChefServer.Config.CertsByIP = &[]config.CertByIP{}
			CopyCertsByIP(haDeployConfig.ChefServer.Config.CertsByIP, existingInfraChefServerSettings.CertsByIP)
		}
	}

	return haDeployConfig
}

func CopyAutomateSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) *config.HaDeployConfig {

	if awsConfig != nil {
		awsConfigAutomateSettings := awsConfig.Automate.Config
		haDeployConfig.Automate = &config.AutomateSettings{
			Config: &config.ConfigAutomateSettings{
				AdminPassword:     awsConfigAutomateSettings.AdminPassword,
				ConfigFile:        awsConfigAutomateSettings.ConfigFile,
				EnableCustomCerts: awsConfigAutomateSettings.EnableCustomCerts,
				Fqdn:              awsConfigAutomateSettings.Fqdn,
				InstanceCount:     awsConfigAutomateSettings.InstanceCount,
				PrivateKey:        awsConfigAutomateSettings.PrivateKey,
				PublicKey:         awsConfigAutomateSettings.PublicKey,
				RootCA:            awsConfigAutomateSettings.RootCA,
				TeamsPort:         awsConfigAutomateSettings.TeamsPort,
			},
		}
	}

	if existingInfraConfig != nil {
		existingInfraConfigAutomateSettings := existingInfraConfig.Automate.Config
		haDeployConfig.Automate = &config.AutomateSettings{
			Config: &config.ConfigAutomateSettings{
				AdminPassword:     existingInfraConfigAutomateSettings.AdminPassword,
				ConfigFile:        existingInfraConfigAutomateSettings.ConfigFile,
				EnableCustomCerts: existingInfraConfigAutomateSettings.EnableCustomCerts,
				Fqdn:              existingInfraConfigAutomateSettings.Fqdn,
				InstanceCount:     existingInfraConfigAutomateSettings.InstanceCount,
				PrivateKey:        existingInfraConfigAutomateSettings.PrivateKey,
				PublicKey:         existingInfraConfigAutomateSettings.PublicKey,
				RootCA:            existingInfraConfigAutomateSettings.RootCA,
				TeamsPort:         existingInfraConfigAutomateSettings.TeamsPort,
			},
		}
		// CertsByIP
		if existingInfraConfigAutomateSettings.CertsByIP != nil {
			haDeployConfig.Automate.Config.CertsByIP = &[]config.CertByIP{}
			CopyCertsByIP(haDeployConfig.Automate.Config.CertsByIP, existingInfraConfigAutomateSettings.CertsByIP)
		}
	}
	return haDeployConfig
}

func CopyConfigObjectStorage(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	existingInfraConfigObjectStorageConfig := existingInfraConfig.ObjectStorage.Config
	haDeployConfig.ObjectStorage = &config.ObjectStorage{
		Config: &config.ConfigObjectStorage{
			AccessKey:  existingInfraConfigObjectStorageConfig.AccessKey,
			BucketName: existingInfraConfigObjectStorageConfig.BucketName,
			Endpoint:   existingInfraConfigObjectStorageConfig.Endpoint,
			Region:     existingInfraConfigObjectStorageConfig.Region,
			SecretKey:  existingInfraConfigObjectStorageConfig.SecretKey,
		},
	}
	return haDeployConfig
}

func CopyConfigInitials(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	if awsConfig != nil {
		awsConfigConfigInitials := awsConfig.Architecture.ConfigInitials
		haDeployConfig.Architecture = &config.Architecture{
			Aws: &config.ConfigInitials{
				SSHUser:                     awsConfigConfigInitials.SSHUser,
				Architecture:                awsConfigConfigInitials.Architecture,
				BackupConfig:                awsConfigConfigInitials.BackupConfig,
				S3BucketName:                awsConfigConfigInitials.S3BucketName,
				BackupMount:                 awsConfigConfigInitials.BackupMount,
				HabitatUIDGid:               awsConfigConfigInitials.HabitatUIDGid,
				LoggingMonitoringManagement: awsConfigConfigInitials.LoggingMonitoringManagement,
				SSHGroupName:                awsConfigConfigInitials.SSHGroupName,
				SSHKeyFile:                  awsConfigConfigInitials.SSHKeyFile,
				SSHPort:                     awsConfigConfigInitials.SSHPort,
				SecretsKeyFile:              awsConfigConfigInitials.SecretsKeyFile,
				SecretsStoreFile:            awsConfigConfigInitials.SecretsStoreFile,
				WorkspacePath:               awsConfigConfigInitials.WorkspacePath,
				SudoPassword:                "", // not fetched
			},
		}
	}

	if existingInfraConfig != nil {
		existingInfraConfigConfigInitials := existingInfraConfig.Architecture.ConfigInitials
		haDeployConfig.Architecture = &config.Architecture{
			ExistingInfra: &config.ConfigInitials{
				SSHUser:                     existingInfraConfigConfigInitials.SSHUser,
				Architecture:                existingInfraConfigConfigInitials.Architecture,
				BackupConfig:                existingInfraConfigConfigInitials.BackupConfig,
				BackupMount:                 existingInfraConfigConfigInitials.BackupMount,
				HabitatUIDGid:               existingInfraConfigConfigInitials.HabitatUIDGid,
				LoggingMonitoringManagement: existingInfraConfigConfigInitials.LoggingMonitoringManagement,
				SSHGroupName:                existingInfraConfigConfigInitials.SSHGroupName,
				SSHKeyFile:                  existingInfraConfigConfigInitials.SSHKeyFile,
				SSHPort:                     existingInfraConfigConfigInitials.SSHPort,
				SecretsKeyFile:              existingInfraConfigConfigInitials.SecretsKeyFile,
				SecretsStoreFile:            existingInfraConfigConfigInitials.SecretsStoreFile,
				WorkspacePath:               existingInfraConfigConfigInitials.WorkspacePath,
				SudoPassword:                "", // not fetched
			},
		}
	}

	return haDeployConfig
}

func CopyExistingInfra(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	// ConfigInitials
	haDeployConfig = CopyConfigInitials(haDeployConfig, existingInfraConfig, nil)

	// ConfigObjectStorage
	haDeployConfig = CopyConfigObjectStorage(haDeployConfig, existingInfraConfig)

	// ConfigAutomateSettings
	haDeployConfig = CopyAutomateSettings(haDeployConfig, existingInfraConfig, nil)

	// ChefServerSettings
	haDeployConfig = CopyChefServerSettings(haDeployConfig, existingInfraConfig, nil)

	// PostgresqlSettings
	haDeployConfig = CopyPostgresqlSettings(haDeployConfig, existingInfraConfig, nil)

	// OpensearchSettings
	haDeployConfig = CopyOpensearchSettings(haDeployConfig, existingInfraConfig, nil)

	// ExistingInfraSettings
	haDeployConfig.External.Database.Type = existingInfraConfig.ExternalDB.Database.Type

	haDeployConfig = CopyExistingInfraSettings(haDeployConfig, existingInfraConfig)

	// ExternalDbSettings
	if IsExternalDb(existingInfraConfig) {
		// ExternalPgSettings
		haDeployConfig = CopyExternalPgSettings(haDeployConfig, existingInfraConfig)
		// ExternalOsSettings
		haDeployConfig = CopyExternalOsSettings(haDeployConfig, existingInfraConfig)
	}

	return haDeployConfig
}

func CopyAws(haDeployConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	// ConfigInitials

	haDeployConfig = CopyConfigInitials(haDeployConfig, nil, awsConfig)

	// ConfigAutomateSettings
	haDeployConfig = CopyAutomateSettings(haDeployConfig, nil, awsConfig)

	// ChefServerSettings
	haDeployConfig = CopyChefServerSettings(haDeployConfig, nil, awsConfig)

	// PostgresqlSettings
	haDeployConfig = CopyPostgresqlSettings(haDeployConfig, nil, awsConfig)

	// OpensearchSettings
	haDeployConfig = CopyOpensearchSettings(haDeployConfig, nil, awsConfig)

	haDeployConfig = CopyAwsConfig(haDeployConfig, awsConfig)

	return haDeployConfig
}

func CopyAwsConfig(haDeployConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {

	awsConfigSetting := awsConfig.Aws.Config

	// AWS Network Config
	haDeployConfig = CopyAwsNetworkConfig(haDeployConfig, awsConfig)

	// Managed Services
	if awsConfigSetting.SetupManagedServices {
		haDeployConfig = CopyManagedServices(haDeployConfig, awsConfig)
	}

	// EC2 Instance Config
	haDeployConfig = CopyEc2InstanceConfig(haDeployConfig, awsConfig)

	return haDeployConfig
}

func CopyAwsNetworkConfig(haDeployConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	awsConfigSetting := awsConfig.Aws.Config
	haDeployConfig.Aws = &config.AwsSettings{
		Config: &config.ConfigAwsSettings{
			Profile:              awsConfigSetting.Profile,
			Region:               awsConfigSetting.Region,
			AwsVpcID:             awsConfigSetting.AwsVpcId,
			AwsCidrBlockAddr:     awsConfigSetting.AwsCidrBlockAddr,
			PrivateCustomSubnets: awsConfigSetting.PrivateCustomSubnets,
			PublicCustomSubnets:  awsConfigSetting.PublicCustomSubnets,
			SSHKeyPairName:       awsConfigSetting.SSHKeyPairName,
			SetupManagedServices: awsConfigSetting.SetupManagedServices,
		},
	}

	return haDeployConfig
}

func CopyManagedServices(haDeployConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	awsConfigSetting := awsConfig.Aws.Config

	haDeployConfig.Aws = &config.AwsSettings{
		Config: &config.ConfigAwsSettings{
			ManagedOpensearchDomainName:   awsConfigSetting.OpensearchDomainName,
			ManagedOpensearchDomainURL:    awsConfigSetting.OpensearchDomainUrl,
			ManagedOpensearchUserPassword: awsConfigSetting.OpensearchUserPassword,
			ManagedOpensearchUsername:     awsConfigSetting.OpensearchUsername,
			ManagedOpensearchCertificate:  awsConfigSetting.OpensearchCertificate,
			OsSnapshotUserAccessKeyID:     awsConfigSetting.OsUserAccessKeyId,
			OsSnapshotUserAccessKeySecret: awsConfigSetting.OsUserAccessKeySecret,
			AwsOsSnapshotRoleArn:          awsConfigSetting.AwsOsSnapshotRoleArn,
			ManagedRdsCertificate:         awsConfigSetting.RDSCertificate,
			ManagedRdsDbuserPassword:      awsConfigSetting.RDSDBUserPassword,
			ManagedRdsDbuserUsername:      awsConfigSetting.RDSDBUserName,
			ManagedRdsInstanceURL:         awsConfigSetting.RDSInstanceUrl,
			ManagedRdsSuperuserPassword:   awsConfigSetting.RDSSuperUserPassword,
			ManagedRdsSuperuserUsername:   awsConfigSetting.RDSSuperUserName,
		},
	}
	return haDeployConfig
}

func CopyEc2InstanceConfig(haDeployConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	awsConfigSetting := awsConfig.Aws.Config

	haDeployConfig.Aws = &config.AwsSettings{
		Config: &config.ConfigAwsSettings{
			AmiID:                        awsConfigSetting.AmiID,
			DeleteOnTermination:          awsConfigSetting.DeleteOnTermination,
			AutomateServerInstanceType:   awsConfigSetting.AutomateServerInstanceType,
			ChefServerInstanceType:       awsConfigSetting.ChefServerInstanceType,
			PostgresqlServerInstanceType: awsConfigSetting.PostgresqlServerInstanceType,
			OpensearchServerInstanceType: awsConfigSetting.OpensearchServerInstanceType,
			AutomateLbCertificateArn:     awsConfigSetting.AutomateLbCertificateArn,
			ChefServerLbCertificateArn:   awsConfigSetting.ChefServerLbCertificateArn,
			AutomateEbsVolumeIops:        awsConfigSetting.AutomateEbsVolumeIops,
			AutomateEbsVolumeSize:        awsConfigSetting.AutomateEbsVolumeSize,
			AutomateEbsVolumeType:        awsConfigSetting.AutomateEbsVolumeType,
			ChefEbsVolumeIops:            awsConfigSetting.ChefEbsVolumeIops,
			ChefEbsVolumeSize:            awsConfigSetting.ChefEbsVolumeSize,
			ChefEbsVolumeType:            awsConfigSetting.ChefEbsVolumeType,
			OpensearchEbsVolumeIops:      awsConfigSetting.OpensearchEbsVolumeIops,
			OpensearchEbsVolumeSize:      awsConfigSetting.OpensearchEbsVolumeSize,
			OpensearchEbsVolumeType:      awsConfigSetting.OpensearchEbsVolumeType,
			PostgresqlEbsVolumeIops:      awsConfigSetting.PostgresqlEbsVolumeIops,
			PostgresqlEbsVolumeSize:      awsConfigSetting.PostgresqlEbsVolumeSize,
			PostgresqlEbsVolumeType:      awsConfigSetting.PostgresqlEbsVolumeType,
			LbAccessLogs:                 awsConfigSetting.LBAccessLogs,
		},
	}

	return haDeployConfig
}

func IsExternalDb(existingInfraConfig *ExistingInfraConfigToml) bool {
	return existingInfraConfig.ExternalDB.Database.Type == AWS || existingInfraConfig.ExternalDB.Database.Type == TYPE_SELF_MANAGED
}
