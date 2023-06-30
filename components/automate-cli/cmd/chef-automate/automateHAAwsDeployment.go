package main

import (
	"container/list"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/components/local-user-service/password"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

type awsDeployment struct {
	config     AwsConfigToml
	configPath string
}

func newAwsDeployemnt(configPath string) *awsDeployment {
	return &awsDeployment{
		configPath: configPath,
	}
}

func (a *awsDeployment) doDeployWork(args []string) error {
	if isA2HARBFileExist() {
		err := executeDeployment(args)
		if err != nil {
			return err
		}
		sharedConfigToml, err := getAwsHAConfig()
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to fetch HA config")
		}
		archBytes, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) // nosemgrep
		if err != nil {
			writer.Errorf("%s", err.Error())
			return err
		}
		var arch = strings.Trim(string(archBytes), "\n")
		sharedConfigToml.Architecture.ConfigInitials.Architecture = arch
		shardConfig, err := toml.Marshal(sharedConfigToml)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to marshal config to file")
		}
		err = ioutil.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"), shardConfig, 0644) // nosemgrep
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to write config toml to file")
		}
		return nil
	} else {
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
}

func (a *awsDeployment) doProvisionJob(args []string) error {
	writer.Print("AWS Provision")
	err := bootstrapEnv(a, deployCmdFlags.airgap, deployCmdFlags.saas)
	if err != nil {
		return err
	}
	err = executeSecretsInitCommand(a.config.Architecture.ConfigInitials.SecretsKeyFile)
	if err != nil {
		return err
	}
	writer.Printf("provisioning infra for automate HA \n\n\n\n")
	args = args[1:]
	args = append(args, "-y")
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommandAsync("provision", args, provisionInfraHelpDocs, true)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}

func (a *awsDeployment) generateConfig() error {
	templateBytes, err := ioutil.ReadFile(a.getConfigPath())
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	a.config = AwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &a.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	errList := a.validateConfigFields()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid")
	}
	if a.config.Opensearch.Config.EnableCustomCerts {
		admin_dn, err := a.getDistinguishedNameFromKey(a.config.Opensearch.Config.AdminCert)
		if err != nil {
			return err
		}
		a.config.Opensearch.Config.AdminDn = admin_dn
		nodes_dn, err := a.getDistinguishedNameFromKey(a.config.Opensearch.Config.PublicKey)
		if err != nil {
			return err
		}
		a.config.Opensearch.Config.NodesDn = nodes_dn
	}
	return writeHAConfigFiles(awsA2harbTemplate, a.config)
}

func (a *awsDeployment) getDistinguishedNameFromKey(publicKey string) (string, error) {
	block, _ := pem.Decode([]byte(publicKey))
	if block == nil {
		return "", status.New(status.ConfigError, "failed to decode certificate PEM")
	}
	cert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return "", status.Wrap(err, status.ConfigError, "failed to parse certificate PEM")
	}
	return fmt.Sprintf("%v", cert.Subject), nil
}

func (a *awsDeployment) getConfigPath() string {
	return a.configPath
}

func (a *awsDeployment) validateConfigFields() *list.List {
	errorList := list.New()
	if len(a.config.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(a.config.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(a.config.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(a.config.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(a.config.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(a.config.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if len(a.config.Architecture.ConfigInitials.BackupMount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	}
	if a.config.Architecture.ConfigInitials.BackupConfig == "s3" && len(strings.TrimSpace(a.config.Architecture.ConfigInitials.S3BucketName)) < 1 {
		errorList.PushBack("Invalid or empty s3_bucketName")
	}
	if len(a.config.Automate.Config.AdminPassword) > 0 {
		val, err := password.NewValidator()
		if err != nil {
			errorList.PushBack(err.Error())
		}
		passvalErr := val.Validate(a.config.Automate.Config.AdminPassword)
		if passvalErr != nil {
			errorList.PushBack(passvalErr.Error())
		}
	}
	if len(a.config.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(a.config.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}
	if !a.config.Aws.Config.SetupManagedServices {
		if len(a.config.Opensearch.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty open-search instance_count")
		}
		if len(a.config.Postgresql.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty postgres-sql instance_count")
		}
	}
	errorList.PushBackList(a.validateEnvFields())
	errorList.PushBackList(a.validateCerts())
	return errorList
}

func (a *awsDeployment) validateEnvFields() *list.List {
	errorList := list.New()
	if len(a.config.Aws.Config.Profile) < 1 {
		errorList.PushBack("Invalid or empty aws profile name")
	}
	if len(a.config.Aws.Config.Region) < 1 {
		errorList.PushBack("Invalid or empty aws region")
	}
	if len(a.config.Aws.Config.SSHKeyPairName) < 1 {
		errorList.PushBack("Invalid or empty aws ssh_key_pair_name")
	}
	if len(a.config.Aws.Config.LBAccessLogs) < 1 {
		errorList.PushBack("Invalid or empty aws lb_access_logs")
	}
	if len(a.config.Aws.Config.AutomateServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_server_instance_type")
	}
	if len(a.config.Aws.Config.ChefServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_instance_type")
	}
	if len(a.config.Aws.Config.AutomateLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws automate_lb_certificate_arn")
	}
	if len(a.config.Aws.Config.ChefServerLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_lb_certificate_arn")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_iops")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_size")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_type")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_iops")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_size")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_type")
	}
	if !a.config.Aws.Config.SetupManagedServices {
		if len(a.config.Aws.Config.OpensearchServerInstanceType) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_server_instance_type")
		}
		if len(a.config.Aws.Config.PostgresqlServerInstanceType) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_server_instance_type")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeIops) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_iops")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeSize) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_size")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeType) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_type")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeIops) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_iops")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeSize) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_size")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeType) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_type")
		}
	}
	return errorList
}

func (a *awsDeployment) validateCerts() *list.List {
	errorList := list.New()

	// If automate root_ca is provided, check that it is valid
	if len(strings.TrimSpace(a.config.Automate.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
		}))
	}

	// If chefserver root_ca is provided, check that it is valid
	if len(strings.TrimSpace(a.config.ChefServer.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.ChefServer.Config.RootCA, certtype: "root_ca", svc: "chefserver"},
		}))
	}
	if a.config.Automate.Config.EnableCustomCerts {
		if len(strings.TrimSpace(a.config.Automate.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(a.config.Automate.Config.PublicKey)) < 1 {
			errorList.PushBack("Automate public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}

		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.Automate.Config.PrivateKey, certtype: "private_key", svc: "automate"},
			{key: a.config.Automate.Config.PublicKey, certtype: "public_key", svc: "automate"},
		}))
	}
	if a.config.ChefServer.Config.EnableCustomCerts {
		if len(strings.TrimSpace(a.config.ChefServer.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(a.config.ChefServer.Config.PublicKey)) < 1 {
			errorList.PushBack("ChefServer root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.ChefServer.Config.PrivateKey, certtype: "private_key", svc: "chef-server"},
			{key: a.config.ChefServer.Config.PublicKey, certtype: "public_key", svc: "chef-server"},
		}))
	}
	if a.config.Postgresql.Config.EnableCustomCerts {
		if len(strings.TrimSpace(a.config.Postgresql.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(a.config.Postgresql.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(a.config.Postgresql.Config.PublicKey)) < 1 {
			errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
			{key: a.config.Postgresql.Config.PrivateKey, certtype: "private_key", svc: "postgresql"},
			{key: a.config.Postgresql.Config.PublicKey, certtype: "public_key", svc: "postgresql"},
		}))
	}
	if a.config.Opensearch.Config.EnableCustomCerts {
		if len(strings.TrimSpace(a.config.Opensearch.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(a.config.Opensearch.Config.AdminKey)) < 1 ||
			len(strings.TrimSpace(a.config.Opensearch.Config.AdminCert)) < 1 ||
			len(strings.TrimSpace(a.config.Opensearch.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(a.config.Opensearch.Config.PublicKey)) < 1 {
			errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
			{key: a.config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
			{key: a.config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
			{key: a.config.Opensearch.Config.PrivateKey, certtype: "private_key", svc: "opensearch"},
			{key: a.config.Opensearch.Config.PublicKey, certtype: "public_key", svc: "opensearch"},
		}))
	}
	return errorList
}
