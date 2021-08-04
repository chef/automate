package main

import (
	"io/ioutil"
	"os"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
)

func runInitConfigAwsHACmd() error {
	initConfigHAPath := initConfigHAFlags.path
	if _, err := os.Stat(initConfigHAPath); err == nil {
		writer.Printf("Skipping config initialization. Config already exists at %s\n", initConfigHAPath)
		return nil
	}

	cfg, err := deployment.GenerateInitHAConfig(
		deployment.InitialSecretsKeyFile(initConfigHAFlags.SecretsKeyFile),
		deployment.InitialSecretsStoreFile(initConfigHAFlags.SecretsStoreFile),
		deployment.InitialArchitecture(initConfigHAFlags.Architecture),
		deployment.InitialWorkspacePath(initConfigHAFlags.WorkspacePath),
		deployment.InitialSshUser(initConfigHAFlags.SshUser),
		deployment.InitialSshKeyFile(initConfigHAFlags.SshKeyFile),
		deployment.InitialBackupMount(initConfigHAFlags.BackupMount),
		deployment.InitialAutomateInstanceCount(initConfigHAFlags.AutomateInstanceCount),
		deployment.InitialAutomateConfigFile(initConfigHAFlags.AutomateConfigFile),
		deployment.InitialChefServerInstanceCount(initConfigHAFlags.ChefServerInstanceCount),
		deployment.InitialElasticSearchInstanceCount(initConfigHAFlags.ElasticSearchInstanceCount),
		deployment.InitialPostgresqlInstanceCount(initConfigHAFlags.PostgresqlInstanceCount),
		deployment.InitialAwsProfile(initConfigHAFlags.AwsProfile),
		deployment.InitialAwsRegion(initConfigHAFlags.AwsRegion),
		deployment.InitialAwsSshKeyPairName(initConfigHAFlags.AwsSshKeyPairName),
		deployment.InitialAwsAutomateServerInstaceType(initConfigHAFlags.AwsAutomateServerInstaceType),
		deployment.InitialAwsChefServerInstanceType(initConfigHAFlags.AwsChefServerInstanceType),
		deployment.InitialAwsElasticSearchServerInstaceType(initConfigHAFlags.AwsElasticSearchServerInstaceType),
		deployment.InitialAwsPostgresqlServerInstanceType(initConfigHAFlags.AwsPostgresqlServerInstanceType),
		deployment.InitialAwsAutomateLBCertificateARN(initConfigHAFlags.AwsAutomateLBCertificateARN),
		deployment.InitialAwsChefServerLBCertificateARN(initConfigHAFlags.AwsChefServerLBCertificateARN),
		deployment.InitialAwsAutomateEbsVolumeIops(initConfigHAFlags.AwsAutomateEbsVolumeIops),
		deployment.InitialAwsAutomateEbsVolumeSize(initConfigHAFlags.AwsAutomateEbsVolumeSize),
		deployment.InitialAwsAutomateEbsVolumeType(initConfigHAFlags.AwsAutomateEbsVolumeType),
		deployment.InitialAwsChefEbsVolumeIops(initConfigHAFlags.AwsChefEbsVolumeIops),
		deployment.InitialAwsChefEbsVolumeSize(initConfigHAFlags.AwsChefEbsVolumeSize),
		deployment.InitialAwsChefEbsVolumeType(initConfigHAFlags.AwsEsEbsVolumeIops),
		deployment.InitialAwsEsEbsVolumeSize(initConfigHAFlags.AwsEsEbsVolumeSize),
		deployment.InitialAwsEsEbsVolumeType(initConfigHAFlags.AwsEsEbsVolumeType),
		deployment.InitialAwsPgsEbsVolumeIops(initConfigHAFlags.AwsPgsEbsVolumeIops),
		deployment.InitialAwsPgsEbsVolumeSize(initConfigHAFlags.AwsPgsEbsVolumeSize),
		deployment.InitialAwsPgsEbsVolumeType(initConfigHAFlags.AwsPgsEbsVolumeType),
	)

	if err != nil {
		return status.Wrap(err, status.ConfigError, "Generating initial configuration failed")
	}

	t, err := cfg.RenderHaSettings()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Rendering initial configuration failed")
	}

	// Make sure our user facing config is a valid AutomateConfig
	ac := deployment.NewAutomateConfig()
	err = toml.Unmarshal([]byte(t), ac)
	if err != nil {
		return status.Wrap(err, status.MarshalError, "Marshaling initial configuration failed")
	}

	err = ioutil.WriteFile(initConfigHAPath, []byte(t), 0600)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}
	writer.Println("file generated " + initConfigHAPath)
	return nil
}
