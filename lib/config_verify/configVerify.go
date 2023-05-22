package config_verify

import (
	"container/list"
	"os"
	"strings"
	"time"

	sc "github.com/chef/automate/api/config/deployment"
	c "github.com/chef/automate/api/config/shared"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/local-user-service/password"
	"github.com/chef/automate/lib/config_parser"
)

type keydetails struct {
	key      string
	certtype string
	svc      string
}

type CertByIP struct {
	IP         string `toml:"ip"`
	PrivateKey string `toml:"private_key"`
	PublicKey  string `toml:"public_key"`
	NodesDn    string `toml:"nodes_dn,omitempty"`
}

type ConfigVerify interface {
	ConfigValidateAWS(config *config_parser.HAAwsConfigToml) error
	ConfigValidateOnPrem(config *config_parser.HAOnPremConfigToml) error
	ConfigValidateStandalone(config *sc.AutomateConfig) error
}

type ConfigVerifyImpl struct{}

func (cv *ConfigVerifyImpl) ConfigValidateAWS(config *config_parser.HAAwsConfigToml) error {
	return ConfigValidateAWS(config)
}

func (cv *ConfigVerifyImpl) ConfigValidateOnPrem(config *config_parser.HAOnPremConfigToml) error {
	return ConfigValidateOnPrem(config)
}

func (cv *ConfigVerifyImpl) ConfigValidateStandalone(config *sc.AutomateConfig) error {
	return ConfigValidateStandalone(config)
}

func ConfigValidateAWS(config *config_parser.HAAwsConfigToml) error {
	errorList := list.New()
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SecretsKeyFile, "secrets_key_file", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SecretsStoreFile, "secrets_store_file", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.Architecture, "Architecture", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.WorkspacePath, "workspace_path", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SSHUser, "ssh_user", errorList)
	validateRequiredPathField(config.Architecture.ConfigInitials.SSHKeyFile, "ssh_key_file", errorList)
	validateBackupMount(config.Architecture.ConfigInitials.BackupMount, errorList)

	if config.Architecture.ConfigInitials.BackupConfig == "s3" && len(strings.TrimSpace(config.Architecture.ConfigInitials.S3BucketName)) < 1 {
		errorList.PushBack("Invalid or empty s3_bucketName")
	}
	validateFQDN(config.Automate.Config.Fqdn, errorList)

	if len(config.Automate.Config.AdminPassword) > 0 {
		val, err := password.NewValidator()
		if err != nil {
			errorList.PushBack(err.Error())
		}
		passvalErr := val.Validate(config.Automate.Config.AdminPassword)
		if passvalErr != nil {
			errorList.PushBack(passvalErr.Error())
		}
	}
	validateRequiredNumberField(config.Automate.Config.InstanceCount, "automate instance_count", errorList)
	validateRequiredNumberField(config.ChefServer.Config.InstanceCount, "chef-server instance_count", errorList)
	validateRequiredNumberField(config.Opensearch.Config.InstanceCount, "open-search instance_count", errorList)
	validateRequiredNumberField(config.Postgresql.Config.InstanceCount, "postgres-sql instance_count", errorList)
	errorList.PushBackList(validateEnvFields(config))
	errorList.PushBackList(validateAWSConfigCerts(config))
	return getSingleErrorFromList(errorList)
}

func validateAWSConfigCerts(config *config_parser.HAAwsConfigToml) *list.List {
	errorList := list.New()
	if config.Automate.Config.EnableCustomCerts {
		if len(strings.TrimSpace(config.Automate.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.Automate.Config.PublicKey)) < 1 {
			errorList.PushBack("Automate public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		// If root_ca is provided, check that it is valid
		if len(strings.TrimSpace(config.Automate.Config.RootCA)) > 0 {
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
			}))
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.Automate.Config.PrivateKey, certtype: "private_key", svc: "automate"},
			{key: config.Automate.Config.PublicKey, certtype: "public_key", svc: "automate"},
		}))
	}
	if config.ChefServer.Config.EnableCustomCerts {
		if len(strings.TrimSpace(config.ChefServer.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.ChefServer.Config.PublicKey)) < 1 {
			errorList.PushBack("ChefServer root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.ChefServer.Config.PrivateKey, certtype: "private_key", svc: "chef-server"},
			{key: config.ChefServer.Config.PublicKey, certtype: "public_key", svc: "chef-server"},
		}))
	}
	if config.Postgresql.Config.EnableCustomCerts {
		if len(strings.TrimSpace(config.Postgresql.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(config.Postgresql.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.Postgresql.Config.PublicKey)) < 1 {
			errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
			{key: config.Postgresql.Config.PrivateKey, certtype: "private_key", svc: "postgresql"},
			{key: config.Postgresql.Config.PublicKey, certtype: "public_key", svc: "postgresql"},
		}))
	}
	if config.Opensearch.Config.EnableCustomCerts {
		if len(strings.TrimSpace(config.Opensearch.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(config.Opensearch.Config.AdminKey)) < 1 ||
			len(strings.TrimSpace(config.Opensearch.Config.AdminCert)) < 1 ||
			len(strings.TrimSpace(config.Opensearch.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.Opensearch.Config.PublicKey)) < 1 {
			errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
			{key: config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
			{key: config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
			{key: config.Opensearch.Config.PrivateKey, certtype: "private_key", svc: "opensearch"},
			{key: config.Opensearch.Config.PublicKey, certtype: "public_key", svc: "opensearch"},
		}))
	}
	return errorList
}
func validateEnvFields(config *config_parser.HAAwsConfigToml) *list.List {
	errorList := list.New()

	validateRequiredStringTypeField(config.Aws.Config.Profile, "aws profile name", errorList)
	validateRequiredStringTypeField(config.Aws.Config.Region, "aws region", errorList)
	validateRequiredStringTypeField(config.Aws.Config.SSHKeyPairName, "aws ssh_key_pair_name", errorList)
	validateRequiredStringTypeField(config.Aws.Config.LBAccessLogs, "aws lb_access_logs", errorList)
	validateRequiredStringTypeField(config.Aws.Config.AutomateServerInstanceType, "aws automate_server_instance_type", errorList)
	validateRequiredStringTypeField(config.Aws.Config.ChefServerInstanceType, "aws chef_server_instance_type", errorList)
	validateRequiredStringTypeField(config.Aws.Config.OpensearchServerInstanceType, "aws opensearch_server_instance_type", errorList)
	validateRequiredStringTypeField(config.Aws.Config.PostgresqlServerInstanceType, "aws postgresql_server_instance_type", errorList)
	validateRequiredStringTypeField(config.Aws.Config.AutomateLbCertificateArn, "aws automate_lb_certificate_arn", errorList)
	validateRequiredStringTypeField(config.Aws.Config.ChefServerLbCertificateArn, "aws chef_server_lb_certificate_arn", errorList)
	validateRequiredNumberField(config.Aws.Config.AutomateEbsVolumeIops, "aws automate_ebs_volume_iops", errorList)
	validateRequiredNumberField(config.Aws.Config.AutomateEbsVolumeSize, "aws automate_ebs_volume_size", errorList)
	validateRequiredStringTypeField(config.Aws.Config.AutomateEbsVolumeType, "aws automate_ebs_volume_type", errorList)
	validateRequiredNumberField(config.Aws.Config.ChefEbsVolumeIops, "aws chef_ebs_volume_iops", errorList)
	validateRequiredNumberField(config.Aws.Config.ChefEbsVolumeSize, "aws chef_ebs_volume_size", errorList)
	validateRequiredStringTypeField(config.Aws.Config.ChefEbsVolumeType, "aws chef_ebs_volume_type", errorList)
	validateRequiredBooleanField(config.Aws.Config.SetupManagedServices, "aws setup_managed_services", errorList)

	if !config.Aws.Config.SetupManagedServices {
		validateRequiredNumberField(config.Aws.Config.OpensearchEbsVolumeIops, "aws opensearch_ebs_volume_iops", errorList)
		validateRequiredNumberField(config.Aws.Config.OpensearchEbsVolumeSize, "aws opensearch_ebs_volume_size", errorList)
		validateRequiredStringTypeField(config.Aws.Config.OpensearchEbsVolumeType, "aws opensearch_ebs_volume_type", errorList)
		validateRequiredNumberField(config.Aws.Config.PostgresqlEbsVolumeIops, "aws postgresql_ebs_volume_iops", errorList)
		validateRequiredNumberField(config.Aws.Config.PostgresqlEbsVolumeSize, "aws postgresql_ebs_volume_size", errorList)
		validateRequiredStringTypeField(config.Aws.Config.PostgresqlEbsVolumeType, "aws postgresql_ebs_volume_type", errorList)
	}

	return errorList
}

func ConfigValidateOnPrem(config *config_parser.HAOnPremConfigToml) error {
	errorList := list.New()

	// Validate required fields
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SecretsKeyFile, "secrets_key_file", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SecretsStoreFile, "secrets_store_file", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.Architecture, "Architecture", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.WorkspacePath, "workspace_path", errorList)
	validateRequiredStringTypeField(config.Architecture.ConfigInitials.SSHUser, "ssh_user", errorList)
	validateRequiredPathField(config.Architecture.ConfigInitials.SSHKeyFile, "ssh_key_file", errorList)
	validateRequiredNumberField(config.Automate.Config.InstanceCount, "automate instance_count", errorList)
	validateFQDN(config.Automate.Config.Fqdn, errorList)
	validateRequiredNumberField(config.ChefServer.Config.InstanceCount, "chef-server instance_count", errorList)
	validateRequiredNumberField(config.Opensearch.Config.InstanceCount, "open-search instance_count", errorList)
	validateRequiredNumberField(config.Postgresql.Config.InstanceCount, "postgres-sql instance_count", errorList)
	validateBackupMount(config.Architecture.ConfigInitials.BackupMount, errorList)

	validateRequiredStringListField(config.ExistingInfra.Config.AutomatePrivateIps, "automate_private_ips", errorList)
	validateRequiredStringListField(config.ExistingInfra.Config.ChefServerPrivateIps, "chef_server_private_ips", errorList)

	if (config.ExternalDB.Database.Type != "aws") && (config.ExternalDB.Database.Type != "self-managed") {
		if len(config.ExistingInfra.Config.OpensearchPrivateIps) < 1 {
			errorList.PushBack("Invalid or empty opensearch_private_ips")
		}

		if len(config.ExistingInfra.Config.PostgresqlPrivateIps) < 1 {
			errorList.PushBack("Invalid or empty postgresql_private_ips")
		}
	}

	if len(config.Architecture.ConfigInitials.BackupConfig) > 0 {
		if config.Architecture.ConfigInitials.BackupConfig == "object_storage" {
			validateRequiredStringTypeField(config.ObjectStorage.Config.AccessKey, "access_key", errorList)
			validateRequiredStringTypeField(config.ObjectStorage.Config.SecretKey, "secret_key", errorList)
			validateRequiredStringTypeField(config.ObjectStorage.Config.BucketName, "bucket_name", errorList)
			validateRequiredStringTypeField(config.ObjectStorage.Config.Endpoint, "endpoint", errorList)
		} else if config.Architecture.ConfigInitials.BackupConfig == "file_system" {
			// no check needed
		} else {
			errorList.PushBack("Invalid or empty backup_config")
		}
	}

	errorList.PushBackList(validateIPs(config))
	errorList.PushBackList(validateCerts(config))
	errorList.PushBackList(validateExternalDbFields(config))

	return getSingleErrorFromList(errorList)
}

func validateExternalDbFields(config *config_parser.HAOnPremConfigToml) *list.List {
	errorList := list.New()
	if config.ExternalDB.Database.Type == "aws" {
		if len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchDomainName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchInstanceURL)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchSuperUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.AWS.AwsOsSnapshotRoleArn)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeyId)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeySecret)) < 1 {
			errorList.PushBack("Opensearch Domain Name and/or Instance URL and/or SuperUser Name and/or SuperUser Password and/or Snapshot Role Arn and/or OsUser AccessKey Id and/or OsUser AccessKey Secret are missing.")
		}
		if len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword)) < 1 {
			errorList.PushBack("PostgreQL Instance URL and/or SuperUser Name and/or SuperUser Password and/or DBUserName and/or DBUserPassword are missing ")
		}
	}
	if config.ExternalDB.Database.Type == "self-managed" {
		if len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchDomainName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchInstanceURL)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchSuperUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.Opensearch.OpensearchRootCert)) < 1 {
			errorList.PushBack("Opensearch Domain Name and/or Instance URL and/or SuperUser Name and/or SuperUser Password and/or Root Cert  are missing.")
		}
		if len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword)) < 1 ||
			len(strings.TrimSpace(config.ExternalDB.Database.PostgreSQL.PostgreSQLRootCert)) < 1 {
			errorList.PushBack("PostgreQL Instance URL and/or SuperUser Name and/or SuperUser Password and/or DBUserName and/or DBUserPassword and/or Root Cert are missing ")
		}
	}
	return errorList
}

func ConfigValidateStandalone(config *sc.AutomateConfig) error {
	e := c.NewInvalidConfigError()

	fqdn := config.Global.V1.Fqdn
	if fqdn == nil {
		e.AddMissingKey("global.v1.fqdn")
	} else {
		// Validate Fqdn
		if err := validateAutomateFQDN(fqdn.Value); err != nil {
			e.AddInvalidValue("global.v1.fqdn", err.Error())
		}
	}

	// Parse frontend TLS configuration
	frontendTLS := config.Global.V1.FrontendTls
	if len(frontendTLS) < 1 {
		e.AddMissingKey("global.v1.frontend_tls")
	} else {
		for _, tls := range config.Global.V1.GetFrontendTls() {
			if tls.CertPath != "" || tls.KeyPath != "" {
				if tls.CertPath != "" {
					certData, err := os.ReadFile(tls.CertPath)
					if err != nil {
						e.AddInvalidValue("global.v1.frontend_tls.cert_path ", err.Error())
					} else {
						tls.Cert = string(certData)
					}
				}
				if tls.KeyPath != "" {
					keyData, err := os.ReadFile(tls.KeyPath)
					if err != nil {
						e.AddInvalidValue("global.v1.frontend_tls.key_path ", err.Error())
					} else {
						tls.Key = string(keyData)
					}
				}

				if tls.Cert != "" && tls.Key != "" {
					err := parseCertAndKey([]byte(tls.Cert), []byte(tls.Key))
					if err != nil {
						e.AddInvalidValue("global.v1.frontend_tls", err.Error())
					}
				} else {
					if tls.Cert == "" {
						e.AddMissingKey("global.v1.frontend_tls.cert")
					}
					if tls.Key == "" {
						e.AddMissingKey("global.v1.frontend_tls.key")
					}
				}
			}

			if tls.ServerName != "" {
				if err := validateAutomateFQDN(tls.ServerName); err != nil {
					e.AddInvalidValue("global.v1.frontend_tls.server_name", err.Error())
				}
			}
		}
	}

	deploymentType := config.Deployment.V1.Svc.DeploymentType
	if deploymentType == nil {
		e.AddMissingKey("deployment.v1.svc.deployment_type")
	} else {
		if deploymentType.Value != "local" {
			e.AddInvalidValue("deployment.v1.svc.deployment_type", "The only supported deployment type is 'local'")
		}
	}

	channel := config.Deployment.V1.Svc.Channel
	if channel == nil {
		e.AddMissingKey("deployment.v1.svc.channel")
	} else {
		if valid, msg := validateChannel(channel.Value); !valid {
			e.AddInvalidValue("deployment.v1.svc.channel", msg)
		}
	}

	upgradeStrategy := config.Deployment.V1.Svc.UpgradeStrategy
	if upgradeStrategy == nil {
		e.AddMissingKey("deployment.v1.svc.upgrade_strategy")
	} else {
		if valid, msg := validateUpgradeStrategy(upgradeStrategy.Value); !valid {
			e.AddInvalidValue("deployment.v1.svc.upgrade_strategy", msg)
		}
	}

	if v := config.Deployment.V1.Svc.GetManifestCacheExpiry().GetValue(); v != "" {
		_, parseErr := time.ParseDuration(v)
		if parseErr != nil {
			e.AddInvalidValue("deployment.v1.svc.manifest_cache_expiry", parseErr.Error())
		}
	}

	packageCleanupMode := config.Deployment.V1.Svc.PackageCleanupMode
	if packageCleanupMode != nil {
		if valid, msg := validatePackageCleanupMode(packageCleanupMode.Value); !valid {
			e.AddInvalidValue("deployment.v1.svc.package_cleanup_mode", msg)
		}
	}

	if desiredProducts := config.Deployment.V1.Svc.GetProducts(); len(desiredProducts) > 0 {
		validationErr := services.ValidateProductDeployment(desiredProducts)
		if validationErr != nil {
			e.AddInvalidValue("deployment.v1.svc.products", validationErr.Error())
		}
	}

	if e.IsEmpty() {
		return nil
	}
	return e
}
