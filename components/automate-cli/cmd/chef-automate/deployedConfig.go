package main

import (
	"errors"

	"github.com/chef/automate/lib/config"
)

var GetModeOfDeployment func() string = getModeOfDeployment

func PopulateHaCommonConfig(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
	modeOfDeployment := GetModeOfDeployment()
	if modeOfDeployment == EXISTING_INFRA_MODE {
		existingInfraConfig, err := configPuller.fetchInfraConfig()
		if err != nil {
			return nil, err
		}
		if existingInfraConfig != nil {
			return CopyExistingInfra(existingInfraConfig), nil
		}
	} else if modeOfDeployment == AWS_MODE {
		awsConfig, err := configPuller.fetchAwsConfig()
		if err != nil {
			return nil, err
		}

		if awsConfig != nil {
			return CopyAws(awsConfig), nil
		}
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

func CopyExistingInfra(existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {

	existingInfraConfigConfigInitials := existingInfraConfig.Architecture.ConfigInitials
	existingInfraConfigObjectStorageConfig := existingInfraConfig.ObjectStorage.Config
	existingInfraConfigAutomateSettings := existingInfraConfig.Automate.Config
	existingInfraChefServerSettings := existingInfraConfig.ChefServer.Config
	existingInfraPostgresqlSettings := existingInfraConfig.Postgresql.Config
	existingInfraOpensearchSettings := existingInfraConfig.Opensearch.Config
	existingInfraSettings := existingInfraConfig.ExistingInfra.Config
	existingInfraExternalPgSettings := existingInfraConfig.ExternalDB.Database.PostgreSQL
	existingInfraExternalOsSettings := existingInfraConfig.ExternalDB.Database.Opensearch

	haDeployConfig := &config.HaDeployConfig{
		Architecture: &config.Architecture{
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
		},
		ObjectStorage: &config.ObjectStorage{
			Config: &config.ConfigObjectStorage{
				AccessKey:  existingInfraConfigObjectStorageConfig.AccessKey,
				BucketName: existingInfraConfigObjectStorageConfig.BucketName,
				Endpoint:   existingInfraConfigObjectStorageConfig.Endpoint,
				Region:     existingInfraConfigObjectStorageConfig.Region,
				SecretKey:  existingInfraConfigObjectStorageConfig.SecretKey,
			},
		},
		Automate: &config.AutomateSettings{
			Config: &config.ConfigAutomateSettings{
				AdminPassword:     existingInfraConfigAutomateSettings.AdminPassword,
				ConfigFile:        existingInfraConfigAutomateSettings.ConfigFile,
				EnableCustomCerts: existingInfraConfigAutomateSettings.EnableCustomCerts,
				Fqdn:              existingInfraConfigAutomateSettings.Fqdn,
				InstanceCount:     existingInfraConfigAutomateSettings.InstanceCount,
				PrivateKey:        existingInfraConfigAutomateSettings.PrivateKey,
				PublicKey:         existingInfraConfigAutomateSettings.PublicKey,
				FqdnRootCA:        existingInfraConfigAutomateSettings.RootCA,
				TeamsPort:         existingInfraConfigAutomateSettings.TeamsPort,
				CertsByIP:         &[]config.CertByIP{},
			},
		},
		ChefServer: &config.ChefServerSettings{
			Config: &config.ConfigChefServerSettings{
				ChefServerFqdn:    existingInfraChefServerSettings.Fqdn,
				FqdnRootCA:        existingInfraChefServerSettings.RootCA,
				EnableCustomCerts: existingInfraChefServerSettings.EnableCustomCerts,
				InstanceCount:     existingInfraChefServerSettings.InstanceCount,
				PrivateKey:        existingInfraChefServerSettings.PrivateKey,
				PublicKey:         existingInfraChefServerSettings.PublicKey,
				CertsByIP:         &[]config.CertByIP{},
			},
		},
		Postgresql: &config.PostgresqlSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: existingInfraPostgresqlSettings.EnableCustomCerts,
				InstanceCount:     existingInfraPostgresqlSettings.InstanceCount,
				PrivateKey:        existingInfraPostgresqlSettings.PrivateKey,
				PublicKey:         existingInfraPostgresqlSettings.PublicKey,
				RootCA:            existingInfraPostgresqlSettings.RootCA,
				CertsByIP:         &[]config.CertByIP{},
			},
		},
		Opensearch: &config.OpensearchSettings{
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
				CertsByIP:         &[]config.CertByIP{},
			},
		},
		ExistingInfra: &config.ExistingInfraSettings{
			Config: &config.ConfigExistingInfraSettings{
				AutomatePrivateIps:   existingInfraSettings.AutomatePrivateIps,
				ChefServerPrivateIps: existingInfraSettings.ChefServerPrivateIps,
				PostgresqlPrivateIps: existingInfraSettings.PostgresqlPrivateIps,
				OpensearchPrivateIps: existingInfraSettings.OpensearchPrivateIps,
			},
		},
		External: &config.ExternalSettings{
			Database: &config.ExternalDBSettings{
				Type: existingInfraConfig.ExternalDB.Database.Type,
				PostgreSQL: &config.ExternalPgSettings{
					DbuserPassword:     existingInfraExternalPgSettings.PostgreSQLDBUserPassword,
					DbuserUsername:     existingInfraExternalPgSettings.PostgreSQLDBUserName,
					InstanceURL:        existingInfraExternalPgSettings.PostgreSQLInstanceURL,
					PostgresqlRootCert: existingInfraExternalPgSettings.PostgreSQLRootCert,
					SuperuserPassword:  existingInfraExternalPgSettings.PostgreSQLSuperUserPassword,
					SuperuserUsername:  existingInfraExternalPgSettings.PostgreSQLSuperUserName,
				},
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
		},
	}

	if existingInfraConfigAutomateSettings.CertsByIP != nil {
		CopyCertsByIP(haDeployConfig.Automate.Config.CertsByIP, existingInfraConfigAutomateSettings.CertsByIP)
	}

	if existingInfraChefServerSettings.CertsByIP != nil {
		CopyCertsByIP(haDeployConfig.ChefServer.Config.CertsByIP, existingInfraChefServerSettings.CertsByIP)
	}

	if existingInfraPostgresqlSettings.CertsByIP != nil {
		CopyCertsByIP(haDeployConfig.Postgresql.Config.CertsByIP, existingInfraPostgresqlSettings.CertsByIP)
	}

	if existingInfraOpensearchSettings.CertsByIP != nil {
		CopyCertsByIP(haDeployConfig.Opensearch.Config.CertsByIP, existingInfraOpensearchSettings.CertsByIP)
	}

	return haDeployConfig
}

func CopyAws(awsConfig *AwsConfigToml) *config.HaDeployConfig {

	awsConfigSetting := awsConfig.Aws.Config
	awsConfigOpensearchSettings := awsConfig.Opensearch.Config
	awsConfigPostgresqlSettings := awsConfig.Postgresql.Config
	awsConfigChefServerSettings := awsConfig.ChefServer.Config
	awsConfigAutomateSettings := awsConfig.Automate.Config
	awsConfigConfigInitials := awsConfig.Architecture.ConfigInitials

	haDeployConfig := &config.HaDeployConfig{
		Architecture: &config.Architecture{
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
		},
		Automate: &config.AutomateSettings{
			Config: &config.ConfigAutomateSettings{
				AdminPassword:     awsConfigAutomateSettings.AdminPassword,
				ConfigFile:        awsConfigAutomateSettings.ConfigFile,
				EnableCustomCerts: awsConfigAutomateSettings.EnableCustomCerts,
				Fqdn:              awsConfigAutomateSettings.Fqdn,
				InstanceCount:     awsConfigAutomateSettings.InstanceCount,
				PrivateKey:        awsConfigAutomateSettings.PrivateKey,
				PublicKey:         awsConfigAutomateSettings.PublicKey,
				FqdnRootCA:        awsConfigAutomateSettings.RootCA,
				TeamsPort:         awsConfigAutomateSettings.TeamsPort,
			},
		},
		ChefServer: &config.ChefServerSettings{
			Config: &config.ConfigChefServerSettings{
				ChefServerFqdn:    awsConfigChefServerSettings.Fqdn,
				FqdnRootCA:        awsConfigChefServerSettings.RootCA,
				EnableCustomCerts: awsConfigChefServerSettings.EnableCustomCerts,
				InstanceCount:     awsConfigChefServerSettings.InstanceCount,
				PrivateKey:        awsConfigChefServerSettings.PrivateKey,
				PublicKey:         awsConfigChefServerSettings.PublicKey,
			},
		},
		Postgresql: &config.PostgresqlSettings{
			Config: &config.ConfigSettings{
				EnableCustomCerts: awsConfigPostgresqlSettings.EnableCustomCerts,
				InstanceCount:     awsConfigPostgresqlSettings.InstanceCount,
				PrivateKey:        awsConfigPostgresqlSettings.PrivateKey,
				PublicKey:         awsConfigPostgresqlSettings.PublicKey,
				RootCA:            awsConfigPostgresqlSettings.RootCA,
			},
		},
		Opensearch: &config.OpensearchSettings{
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
		},
		Aws: &config.AwsSettings{
			Config: &config.ConfigAwsSettings{
				Profile:                       awsConfigSetting.Profile,
				Region:                        awsConfigSetting.Region,
				AwsVpcID:                      awsConfigSetting.AwsVpcId,
				AwsCidrBlockAddr:              awsConfigSetting.AwsCidrBlockAddr,
				PrivateCustomSubnets:          awsConfigSetting.PrivateCustomSubnets,
				PublicCustomSubnets:           awsConfigSetting.PublicCustomSubnets,
				SSHKeyPairName:                awsConfigSetting.SSHKeyPairName,
				SetupManagedServices:          awsConfigSetting.SetupManagedServices,
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
				AmiID:                         awsConfigSetting.AmiID,
				DeleteOnTermination:           awsConfigSetting.DeleteOnTermination,
				AutomateServerInstanceType:    awsConfigSetting.AutomateServerInstanceType,
				ChefServerInstanceType:        awsConfigSetting.ChefServerInstanceType,
				PostgresqlServerInstanceType:  awsConfigSetting.PostgresqlServerInstanceType,
				OpensearchServerInstanceType:  awsConfigSetting.OpensearchServerInstanceType,
				AutomateLbCertificateArn:      awsConfigSetting.AutomateLbCertificateArn,
				ChefServerLbCertificateArn:    awsConfigSetting.ChefServerLbCertificateArn,
				AutomateEbsVolumeIops:         awsConfigSetting.AutomateEbsVolumeIops,
				AutomateEbsVolumeSize:         awsConfigSetting.AutomateEbsVolumeSize,
				AutomateEbsVolumeType:         awsConfigSetting.AutomateEbsVolumeType,
				ChefEbsVolumeIops:             awsConfigSetting.ChefEbsVolumeIops,
				ChefEbsVolumeSize:             awsConfigSetting.ChefEbsVolumeSize,
				ChefEbsVolumeType:             awsConfigSetting.ChefEbsVolumeType,
				OpensearchEbsVolumeIops:       awsConfigSetting.OpensearchEbsVolumeIops,
				OpensearchEbsVolumeSize:       awsConfigSetting.OpensearchEbsVolumeSize,
				OpensearchEbsVolumeType:       awsConfigSetting.OpensearchEbsVolumeType,
				PostgresqlEbsVolumeIops:       awsConfigSetting.PostgresqlEbsVolumeIops,
				PostgresqlEbsVolumeSize:       awsConfigSetting.PostgresqlEbsVolumeSize,
				PostgresqlEbsVolumeType:       awsConfigSetting.PostgresqlEbsVolumeType,
				LbAccessLogs:                  awsConfigSetting.LBAccessLogs,
			},
		},
	}

	return haDeployConfig
}
