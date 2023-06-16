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

func CopyOpensearchSettings(haDeployOpensearchSettings *config.ConfigOpensearchSettings, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) {

	if awsConfig != nil {
		awsConfigOpensearchSettings := existingInfraConfig.Opensearch.Config
		awshaDeployOpensearchSettings := haDeployOpensearchSettings

		awshaDeployOpensearchSettings.AdminCert = awsConfigOpensearchSettings.AdminCert
		awshaDeployOpensearchSettings.AdminDn = awsConfigOpensearchSettings.AdminDn
		awshaDeployOpensearchSettings.AdminKey = awsConfigOpensearchSettings.AdminKey
		awshaDeployOpensearchSettings.EnableCustomCerts = awsConfigOpensearchSettings.EnableCustomCerts
		awshaDeployOpensearchSettings.InstanceCount = awsConfigOpensearchSettings.InstanceCount
		awshaDeployOpensearchSettings.NodesDn = awsConfigOpensearchSettings.NodesDn
		awshaDeployOpensearchSettings.PrivateKey = awsConfigOpensearchSettings.PrivateKey
		awshaDeployOpensearchSettings.PublicKey = awsConfigOpensearchSettings.PublicKey
		awshaDeployOpensearchSettings.RootCA = awsConfigOpensearchSettings.RootCA

		// CertsByIP
		if awsConfigOpensearchSettings.CertsByIP != nil {
			CopyCertsByIP(awsConfigOpensearchSettings.CertsByIP, awshaDeployOpensearchSettings.CertsByIP)
		}
		return
	}

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

func CopyPostgresqlSettings(haDeployPostgresqlSettings *config.ConfigSettings, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) {

	if awsConfig != nil {
		awsConfigPostgresqlSettings := awsConfig.Postgresql.Config
		awshaDeployPostgresqlSettings := haDeployPostgresqlSettings

		awshaDeployPostgresqlSettings.EnableCustomCerts = awsConfigPostgresqlSettings.EnableCustomCerts
		awshaDeployPostgresqlSettings.InstanceCount = awsConfigPostgresqlSettings.InstanceCount
		awshaDeployPostgresqlSettings.PrivateKey = awsConfigPostgresqlSettings.PublicKey
		awshaDeployPostgresqlSettings.RootCA = awsConfigPostgresqlSettings.RootCA

		// CertsByIP
		if awsConfigPostgresqlSettings.CertsByIP != nil {
			CopyCertsByIP(awsConfigPostgresqlSettings.CertsByIP, awshaDeployPostgresqlSettings.CertsByIP)
		}
		return
	}

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

func CopyChefServerSettings(haDeployChefServerSettings *config.ConfigSettings, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) {

	if awsConfig != nil {
		awsConfigChefServerSettings := awsConfig.ChefServer.Config
		awshaDeployChefServerSettings := haDeployChefServerSettings

		awshaDeployChefServerSettings.EnableCustomCerts = awsConfigChefServerSettings.EnableCustomCerts
		awshaDeployChefServerSettings.InstanceCount = awsConfigChefServerSettings.InstanceCount
		awshaDeployChefServerSettings.PrivateKey = awsConfigChefServerSettings.PrivateKey
		awshaDeployChefServerSettings.PublicKey = awsConfigChefServerSettings.PublicKey

		// CertsByIP
		if awsConfigChefServerSettings.CertsByIP != nil {
			CopyCertsByIP(awsConfigChefServerSettings.CertsByIP, awshaDeployChefServerSettings.CertsByIP)
		}
		return
	}

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

func CopyAutomateSettings(haDeployConfigAutomateSettings *config.ConfigAutomateSettings, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) {

	if awsConfig != nil {
		awsConfigAutomateSettings := awsConfig.Automate.Config
		awshaDeployConfigAutomateSettings := haDeployConfigAutomateSettings

		awshaDeployConfigAutomateSettings.AdminPassword = awsConfigAutomateSettings.AdminPassword
		awshaDeployConfigAutomateSettings.ConfigFile = awsConfigAutomateSettings.ConfigFile
		awshaDeployConfigAutomateSettings.EnableCustomCerts = awsConfigAutomateSettings.EnableCustomCerts
		awshaDeployConfigAutomateSettings.Fqdn = awsConfigAutomateSettings.Fqdn
		awshaDeployConfigAutomateSettings.InstanceCount = awsConfigAutomateSettings.InstanceCount
		awshaDeployConfigAutomateSettings.PrivateKey = awsConfigAutomateSettings.PrivateKey
		awshaDeployConfigAutomateSettings.PublicKey = awsConfigAutomateSettings.PublicKey
		awshaDeployConfigAutomateSettings.RootCA = awsConfigAutomateSettings.RootCA
		awshaDeployConfigAutomateSettings.TeamsPort = awsConfigAutomateSettings.TeamsPort

		// CertsByIP
		if awsConfigAutomateSettings.CertsByIP != nil {
			CopyCertsByIP(awsConfigAutomateSettings.CertsByIP, awshaDeployConfigAutomateSettings.CertsByIP)
		}
		return
	}

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

func CopyConfigInitials(haDeployConfigConfigInitials *config.ConfigInitials, existingInfraConfig *ExistingInfraConfigToml, awsConfig *AwsConfigToml) {

	if awsConfig != nil {
		awsConfigConfigInitials := awsConfig.Architecture.ConfigInitials
		awshaDeployConfigConfigInitials := haDeployConfigConfigInitials

		awshaDeployConfigConfigInitials.SSHUser = awsConfigConfigInitials.SSHUser
		awshaDeployConfigConfigInitials.Architecture = awsConfigConfigInitials.Architecture
		awshaDeployConfigConfigInitials.BackupConfig = awsConfigConfigInitials.BackupConfig
		//Backup config
		awshaDeployConfigConfigInitials.S3BucketName = awsConfigConfigInitials.S3BucketName
		awshaDeployConfigConfigInitials.BackupMount = awsConfigConfigInitials.BackupMount
		awshaDeployConfigConfigInitials.HabitatUIDGid = awsConfigConfigInitials.HabitatUIDGid
		awshaDeployConfigConfigInitials.LoggingMonitoringManagement = awsConfigConfigInitials.LoggingMonitoringManagement
		awshaDeployConfigConfigInitials.SSHGroupName = awsConfigConfigInitials.SSHGroupName
		awshaDeployConfigConfigInitials.SSHKeyFile = awsConfigConfigInitials.SSHKeyFile
		awshaDeployConfigConfigInitials.SSHPort = awsConfigConfigInitials.SSHPort
		awshaDeployConfigConfigInitials.SSHUser = awsConfigConfigInitials.SSHUser
		awshaDeployConfigConfigInitials.SecretsKeyFile = awsConfigConfigInitials.SecretsKeyFile
		awshaDeployConfigConfigInitials.SecretsStoreFile = awsConfigConfigInitials.SecretsStoreFile
		awshaDeployConfigConfigInitials.WorkspacePath = awsConfigConfigInitials.WorkspacePath
		awshaDeployConfigConfigInitials.SudoPassword = "" // not fetched
		return
	}

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
	CopyConfigInitials(haConfig.Architecture.ExistingInfra, existingInfraConfig, nil)

	// ConfigObjectStorage
	CopyConfigObjectStorage(haConfig.ObjectStorage.Config, existingInfraConfig)

	// ConfigAutomateSettings
	CopyAutomateSettings(haConfig.Automate.Config, existingInfraConfig, nil)

	// ChefServerSettings
	CopyChefServerSettings(haConfig.ChefServer.Config, existingInfraConfig, nil)

	// PostgresqlSettings
	CopyPostgresqlSettings(haConfig.Postgresql.Config, existingInfraConfig, nil)

	// OpensearchSettings
	CopyOpensearchSettings(haConfig.Opensearch.Config, existingInfraConfig, nil)

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

func CopyAws(haConfig *config.HaDeployConfig, awsConfig *AwsConfigToml) *config.HaDeployConfig {
	// ConfigInitials
	CopyConfigInitials(haConfig.Architecture.ExistingInfra, nil, awsConfig)

	// ConfigAutomateSettings
	CopyAutomateSettings(haConfig.Automate.Config, nil, awsConfig)

	// ChefServerSettings
	CopyChefServerSettings(haConfig.ChefServer.Config, nil, awsConfig)

	// PostgresqlSettings
	CopyPostgresqlSettings(haConfig.Postgresql.Config, nil, awsConfig)

	// OpensearchSettings
	CopyOpensearchSettings(haConfig.Opensearch.Config, nil, awsConfig)

	CopyAwsConfig(haConfig.Aws.Config, awsConfig)

	return haConfig
}

func CopyAwsConfig(haConfigAws *config.ConfigAwsSettings, awsConfig *AwsConfigToml) {
	awsConfigSetting := awsConfig.Aws.Config

	// AWS Network Config
	CopyAwsNetworkConfig(haConfigAws, awsConfig)

	// Managed Services
	haConfigAws.SetupManagedServices = awsConfigSetting.SetupManagedServices
	if awsConfigSetting.SetupManagedServices {
		CopyManagedServices(haConfigAws, awsConfig)
	}

	// EC2 Instance Config
	CopyEc2InstanceConfig(haConfigAws, awsConfig)
}

func CopyAwsNetworkConfig(haConfigAws *config.ConfigAwsSettings, awsConfig *AwsConfigToml) {
	awsConfigSetting := awsConfig.Aws.Config

	haConfigAws.Profile = awsConfigSetting.Profile
	haConfigAws.Region = awsConfigSetting.Region
	haConfigAws.AwsVpcID = awsConfigSetting.AwsVpcId
	haConfigAws.AwsCidrBlockAddr = awsConfigSetting.AwsCidrBlockAddr
	haConfigAws.PrivateCustomSubnets = awsConfigSetting.PrivateCustomSubnets
	haConfigAws.PublicCustomSubnets = awsConfigSetting.PublicCustomSubnets
	haConfigAws.SSHKeyPairName = awsConfigSetting.SSHKeyPairName
}

func CopyManagedServices(haConfigAws *config.ConfigAwsSettings, awsConfig *AwsConfigToml) {
	awsConfigSetting := awsConfig.Aws.Config

	haConfigAws.ManagedOpensearchDomainName = awsConfigSetting.OpensearchDomainName
	haConfigAws.ManagedOpensearchDomainURL = awsConfigSetting.OpensearchDomainUrl
	haConfigAws.ManagedOpensearchUserPassword = awsConfigSetting.OpensearchUserPassword
	haConfigAws.ManagedOpensearchUsername = awsConfigSetting.OpensearchUsername
	haConfigAws.ManagedOpensearchCertificate = awsConfigSetting.OpensearchCertificate

	haConfigAws.OsSnapshotUserAccessKeyID = awsConfigSetting.OsUserAccessKeyId
	haConfigAws.OsSnapshotUserAccessKeySecret = awsConfigSetting.OsUserAccessKeySecret
	haConfigAws.AwsOsSnapshotRoleArn = awsConfigSetting.AwsOsSnapshotRoleArn

	haConfigAws.ManagedRdsCertificate = awsConfigSetting.RDSCertificate
	haConfigAws.ManagedRdsDbuserPassword = awsConfigSetting.RDSDBUserPassword
	haConfigAws.ManagedRdsDbuserUsername = awsConfigSetting.RDSDBUserName
	haConfigAws.ManagedRdsInstanceURL = awsConfigSetting.RDSInstanceUrl
	haConfigAws.ManagedRdsSuperuserPassword = awsConfigSetting.RDSSuperUserPassword
	haConfigAws.ManagedRdsSuperuserUsername = awsConfigSetting.RDSSuperUserName
}

func CopyEc2InstanceConfig(haConfigAws *config.ConfigAwsSettings, awsConfig *AwsConfigToml) {
	awsConfigSetting := awsConfig.Aws.Config

	haConfigAws.AmiID = awsConfigSetting.AmiID
	haConfigAws.DeleteOnTermination = awsConfigSetting.DeleteOnTermination

	haConfigAws.AutomateServerInstanceType = awsConfigSetting.AutomateServerInstanceType
	haConfigAws.ChefServerInstanceType = awsConfigSetting.ChefServerInstanceType
	haConfigAws.PostgresqlServerInstanceType = awsConfigSetting.PostgresqlServerInstanceType
	haConfigAws.OpensearchServerInstanceType = awsConfigSetting.OpensearchServerInstanceType

	haConfigAws.AutomateLbCertificateArn = awsConfigSetting.AutomateLbCertificateArn
	haConfigAws.ChefServerLbCertificateArn = awsConfigSetting.ChefServerLbCertificateArn

	haConfigAws.AutomateEbsVolumeIops = awsConfigSetting.AutomateEbsVolumeIops
	haConfigAws.AutomateEbsVolumeSize = awsConfigSetting.AutomateEbsVolumeSize
	haConfigAws.AutomateEbsVolumeType = awsConfigSetting.AutomateEbsVolumeType
	haConfigAws.ChefEbsVolumeIops = awsConfigSetting.ChefEbsVolumeIops
	haConfigAws.ChefEbsVolumeSize = awsConfigSetting.ChefEbsVolumeSize
	haConfigAws.ChefEbsVolumeType = awsConfigSetting.ChefEbsVolumeType
	haConfigAws.OpensearchEbsVolumeIops = awsConfigSetting.OpensearchEbsVolumeIops
	haConfigAws.OpensearchEbsVolumeSize = awsConfigSetting.OpensearchEbsVolumeSize
	haConfigAws.OpensearchEbsVolumeType = awsConfigSetting.OpensearchEbsVolumeType
	haConfigAws.PostgresqlEbsVolumeIops = awsConfigSetting.PostgresqlEbsVolumeIops
	haConfigAws.PostgresqlEbsVolumeSize = awsConfigSetting.PostgresqlEbsVolumeSize
	haConfigAws.PostgresqlEbsVolumeType = awsConfigSetting.PostgresqlEbsVolumeType

	haConfigAws.LbAccessLogs = awsConfigSetting.LBAccessLogs
}
