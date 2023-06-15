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
		// haConfig.PopulateWithAwsConfig(awsConfig)
		return haConfig, nil
	}

	return nil, errors.New("deployed config was not found")
}

func CopyCertsByIP(existing []CertByIP, haDeploy *[]config.CertByIP) {
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

func CopyExternalPgSettings(haDeployExternalPgSettings *config.ExternalPgSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraExternalPgSettings := existingInfraConfig.ExternalDB.Database.PostgreSQL

	haDeployExternalPgSettings.DbuserPassword = existingInfraExternalPgSettings.PostgreSQLDBUserPassword
	haDeployExternalPgSettings.DbuserUsername = existingInfraExternalPgSettings.PostgreSQLDBUserName
	haDeployExternalPgSettings.InstanceURL = existingInfraExternalPgSettings.PostgreSQLInstanceURL
	haDeployExternalPgSettings.PostgresqlRootCert = existingInfraExternalPgSettings.PostgreSQLRootCert
	haDeployExternalPgSettings.SuperuserPassword = existingInfraExternalPgSettings.PostgreSQLSuperUserPassword
	haDeployExternalPgSettings.SuperuserUsername = existingInfraExternalPgSettings.PostgreSQLSuperUserName

}

func CopyExternalOsSettings(haDeployExternalOsSettings *config.ExternalOsSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraExternalOsSettings := existingInfraConfig.ExternalDB.Database.Opensearch

	haDeployExternalOsSettings.OpensearchRootCert = existingInfraExternalOsSettings.OpensearchRootCert
	haDeployExternalOsSettings.OpensearchDomainName = existingInfraExternalOsSettings.OpensearchDomainName
	haDeployExternalOsSettings.OpensearchDomainURL = existingInfraExternalOsSettings.OpensearchInstanceURL
	haDeployExternalOsSettings.OpensearchUserPassword = existingInfraExternalOsSettings.OpensearchSuperUserPassword
	haDeployExternalOsSettings.OpensearchUsername = existingInfraExternalOsSettings.OpensearchSuperUserName

	if existingInfraExternalOsSettings.AWS.AwsOsSnapshotRoleArn != "" {
		haDeployExternalOsSettings.Aws.AwsOsSnapshotRoleArn = existingInfraExternalOsSettings.AWS.AwsOsSnapshotRoleArn
		haDeployExternalOsSettings.Aws.OsSnapshotUserAccessKeyID = existingInfraExternalOsSettings.AWS.OsUserAccessKeyId
		haDeployExternalOsSettings.Aws.OsSnapshotUserAccessKeySecret = existingInfraExternalOsSettings.AWS.OsUserAccessKeySecret
	}
}

func CopyExistingInfraSettings(haDeployConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraSettings := existingInfraConfig.ExistingInfra.Config
	haDeployExistingInfraSettings := haDeployConfig.ExistingInfra.Config

	haDeployExistingInfraSettings.AutomatePrivateIps = existingInfraSettings.AutomatePrivateIps
	haDeployExistingInfraSettings.ChefServerPrivateIps = existingInfraSettings.ChefServerPrivateIps

	if !haDeployConfig.IsExternalDb() {
		haDeployExistingInfraSettings.PostgresqlPrivateIps = existingInfraSettings.PostgresqlPrivateIps
		haDeployExistingInfraSettings.OpensearchPrivateIps = existingInfraSettings.OpensearchPrivateIps
	}
}

func CopyOpensearchSettings(haDeployOpensearchSettings *config.ConfigOpensearchSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraOpensearchSettings := existingInfraConfig.Opensearch.Config

	haDeployOpensearchSettings.AdminCert = existingInfraOpensearchSettings.AdminCert
	haDeployOpensearchSettings.AdminDn = existingInfraOpensearchSettings.AdminDn
	haDeployOpensearchSettings.AdminKey = existingInfraOpensearchSettings.AdminKey
	haDeployOpensearchSettings.EnableCustomCerts = existingInfraOpensearchSettings.EnableCustomCerts
	haDeployOpensearchSettings.InstanceCount = existingInfraOpensearchSettings.InstanceCount
	haDeployOpensearchSettings.NodesDn = existingInfraOpensearchSettings.NodesDn
	haDeployOpensearchSettings.PrivateKey = existingInfraOpensearchSettings.PrivateKey
	haDeployOpensearchSettings.PublicKey = existingInfraOpensearchSettings.PublicKey
	haDeployOpensearchSettings.RootCA = existingInfraOpensearchSettings.RootCA

	// CertsByIP
	if existingInfraOpensearchSettings.CertsByIP != nil {
		CopyCertsByIP(existingInfraOpensearchSettings.CertsByIP, haDeployOpensearchSettings.CertsByIP)
	}
}

func CopyPostgresqlSettings(haDeployPostgresqlSettings *config.ConfigSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraPostgresqlSettings := existingInfraConfig.Postgresql.Config

	haDeployPostgresqlSettings.EnableCustomCerts = existingInfraPostgresqlSettings.EnableCustomCerts
	haDeployPostgresqlSettings.InstanceCount = existingInfraPostgresqlSettings.InstanceCount
	haDeployPostgresqlSettings.PrivateKey = existingInfraPostgresqlSettings.PublicKey
	haDeployPostgresqlSettings.RootCA = existingInfraPostgresqlSettings.RootCA

	// CertsByIP
	if existingInfraPostgresqlSettings.CertsByIP != nil {
		CopyCertsByIP(existingInfraPostgresqlSettings.CertsByIP, haDeployPostgresqlSettings.CertsByIP)
	}
}

func CopyChefServerSettings(haDeployChefServerSettings *config.ConfigSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraChefServerSettings := existingInfraConfig.ChefServer.Config

	haDeployChefServerSettings.EnableCustomCerts = existingInfraChefServerSettings.EnableCustomCerts
	haDeployChefServerSettings.InstanceCount = existingInfraChefServerSettings.InstanceCount
	haDeployChefServerSettings.PrivateKey = existingInfraChefServerSettings.PrivateKey
	haDeployChefServerSettings.PublicKey = existingInfraChefServerSettings.PublicKey

	// CertsByIP
	if existingInfraChefServerSettings.CertsByIP != nil {
		CopyCertsByIP(existingInfraChefServerSettings.CertsByIP, haDeployChefServerSettings.CertsByIP)
	}
}

func CopyAutomateSettings(haDeployConfigAutomateSettings *config.ConfigAutomateSettings, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraConfigAutomateSettings := existingInfraConfig.Automate.Config

	haDeployConfigAutomateSettings.AdminPassword = existingInfraConfigAutomateSettings.AdminPassword
	haDeployConfigAutomateSettings.ConfigFile = existingInfraConfigAutomateSettings.ConfigFile
	haDeployConfigAutomateSettings.EnableCustomCerts = existingInfraConfigAutomateSettings.EnableCustomCerts
	haDeployConfigAutomateSettings.Fqdn = existingInfraConfigAutomateSettings.Fqdn
	haDeployConfigAutomateSettings.InstanceCount = existingInfraConfigAutomateSettings.InstanceCount
	haDeployConfigAutomateSettings.PrivateKey = existingInfraConfigAutomateSettings.PrivateKey
	haDeployConfigAutomateSettings.PublicKey = existingInfraConfigAutomateSettings.PublicKey
	haDeployConfigAutomateSettings.RootCA = existingInfraConfigAutomateSettings.RootCA
	haDeployConfigAutomateSettings.TeamsPort = existingInfraConfigAutomateSettings.TeamsPort

	// CertsByIP
	if existingInfraConfigAutomateSettings.CertsByIP != nil {
		CopyCertsByIP(existingInfraConfigAutomateSettings.CertsByIP, haDeployConfigAutomateSettings.CertsByIP)
	}

}

func CopyConfigObjectStorage(haDeployConfigObjectStorageConfig *config.ConfigObjectStorage, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraConfigObjectStorageConfig := existingInfraConfig.ObjectStorage.Config

	haDeployConfigObjectStorageConfig.AccessKey = existingInfraConfigObjectStorageConfig.AccessKey
	haDeployConfigObjectStorageConfig.BucketName = existingInfraConfigObjectStorageConfig.BucketName
	haDeployConfigObjectStorageConfig.Endpoint = existingInfraConfigObjectStorageConfig.Endpoint
	haDeployConfigObjectStorageConfig.Region = existingInfraConfigObjectStorageConfig.Region
	haDeployConfigObjectStorageConfig.SecretKey = existingInfraConfigObjectStorageConfig.SecretKey
}

func CopyConfigInitials(haDeployConfigConfigInitials *config.ConfigInitials, existingInfraConfig *ExistingInfraConfigToml) {
	existingInfraConfigConfigInitials := existingInfraConfig.Architecture.ConfigInitials

	haDeployConfigConfigInitials.SSHUser = existingInfraConfigConfigInitials.SSHUser
	haDeployConfigConfigInitials.Architecture = existingInfraConfigConfigInitials.Architecture
	haDeployConfigConfigInitials.BackupConfig = existingInfraConfigConfigInitials.BackupConfig
	haDeployConfigConfigInitials.BackupMount = existingInfraConfigConfigInitials.BackupMount
	haDeployConfigConfigInitials.HabitatUIDGid = existingInfraConfigConfigInitials.HabitatUIDGid
	haDeployConfigConfigInitials.LoggingMonitoringManagement = existingInfraConfigConfigInitials.LoggingMonitoringManagement
	haDeployConfigConfigInitials.SSHGroupName = existingInfraConfigConfigInitials.SSHGroupName
	haDeployConfigConfigInitials.SSHKeyFile = existingInfraConfigConfigInitials.SSHKeyFile
	haDeployConfigConfigInitials.SSHPort = existingInfraConfigConfigInitials.SSHPort
	haDeployConfigConfigInitials.SSHUser = existingInfraConfigConfigInitials.SSHUser
	haDeployConfigConfigInitials.SecretsKeyFile = existingInfraConfigConfigInitials.SecretsKeyFile
	haDeployConfigConfigInitials.SecretsStoreFile = existingInfraConfigConfigInitials.SecretsStoreFile
	haDeployConfigConfigInitials.WorkspacePath = existingInfraConfigConfigInitials.WorkspacePath
	haDeployConfigConfigInitials.SudoPassword = "" // not fetched
}

func CopyExistingInfra(haConfig *config.HaDeployConfig, existingInfraConfig *ExistingInfraConfigToml) *config.HaDeployConfig {
	// ConfigInitials
	CopyConfigInitials(haConfig.Architecture.ExistingInfra, existingInfraConfig)

	// ConfigObjectStorage
	CopyConfigObjectStorage(haConfig.ObjectStorage.Config, existingInfraConfig)

	// ConfigAutomateSettings
	CopyAutomateSettings(haConfig.Automate.Config, existingInfraConfig)

	// ChefServerSettings
	CopyChefServerSettings(haConfig.ChefServer.Config, existingInfraConfig)

	// PostgresqlSettings
	CopyPostgresqlSettings(haConfig.Postgresql.Config, existingInfraConfig)

	// OpensearchSettings
	CopyOpensearchSettings(haConfig.Opensearch.Config, existingInfraConfig)

	// ExistingInfraSettings
	haConfig.External.Database.Type = existingInfraConfig.ExternalDB.Database.Type

	CopyExistingInfraSettings(haConfig, existingInfraConfig)

	// ExternalDbSettings
	if haConfig.IsExternalDb() {
		// ExternalPgSettings
		CopyExternalPgSettings(haConfig.External.Database.PostgreSQL, existingInfraConfig)
		// ExternalOsSettings
		CopyExternalOsSettings(haConfig.External.Database.OpenSearch, existingInfraConfig)
	}

	return haConfig
}
