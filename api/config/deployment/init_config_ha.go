package deployment

import (
	"bytes"
	"strings"
	"text/template"
)

// InitConfigHA is a struct that contains fields that correspond to user facing
// configuration values that we'll use when rendering the config template.
type InitConfigHA struct {
	SecretsKeyFile                    string
	SecretsStoreFile                  string
	Architecture                      string
	WorkspacePath                     string
	SshUser                           string
	SshKeyFile                        string
	SudoPassword                      string
	LoggingMonitoringManagement       string
	NewElk                            string
	ExistingElk                       string
	ExistingElkInstanceIp             string
	ExistingElkPort                   string
	ExistingElkCert                   string
	ExistingElkUsername               string
	ExistingElkPassword               string
	BackupMount                       string
	HabitatUidGid                     string
	AutomateInstanceCount             string
	AutomateConfigFile                string
	AutomateAdminPassword             string
	AutomateFQDN                      string
	AutomateTeamsPort                 string
	ChefServerInstanceCount           string
	ElasticSearchInstanceCount        string
	PostgresqlInstanceCount           string
	AwsProfile                        string
	AwsRegion                         string
	AwsSshKeyPairName                 string
	AwsAmiFilterName                  string
	AwsAmiFilterVirtType              string
	AwsFilterOwner                    string
	AwsAmiId                          string
	AwsAutomateServerInstaceType      string
	AwsChefServerInstanceType         string
	AwsElasticSearchServerInstaceType string
	AwsPostgresqlServerInstanceType   string
	AwsAutomateLBCertificateARN       string
	AwsChefServerLBCertificateARN     string
	AwsAutomateEbsVolumeIops          string
	AwsAutomateEbsVolumeSize          string
	AwsAutomateEbsVolumeType          string
	AwsChefEbsVolumeIops              string
	AwsChefEbsVolumeSize              string
	AwsChefEbsVolumeType              string
	AwsEsEbsVolumeIops                string
	AwsEsEbsVolumeSize                string
	AwsEsEbsVolumeType                string
	AwsPgsEbsVolumeIops               string
	AwsPgsEbsVolumeSize               string
	AwsPgsEbsVolumeType               string
	AwsTagContact                     string
	AwsTagDept                        string
	AwsTagProject                     string
}

// NewInitConfigHA for Ha mode returns a new instance of InitConfig with default values
func NewInitConfigHA() *InitConfigHA {
	return &InitConfigHA{}
}

// InitConfigHAOpt is an option that can be passed to the
// GenerateInitConfig
type InitConfigHAOpt func(*InitConfigHA) error

// Initial Secrets key file
func InitialSecretsKeyFile(secretsKeyFile string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.SecretsKeyFile = secretsKeyFile
		return nil
	}
}

// Initial sets the SecretsStoreFile for the generated configuration
func InitialSecretsStoreFile(secretsStoreFile string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.SecretsStoreFile = secretsStoreFile
		return nil
	}
}

// Initial sets the Architecture for the generated configuration
func InitialArchitecture(architecture string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.Architecture = architecture
		return nil
	}
}

// Initial sets the WorkspacePath for the generated configuration
func InitialWorkspacePath(workspacePath string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.WorkspacePath = workspacePath
		return nil
	}
}

// Initial sets the SshUser for the generated configuration
func InitialSshUser(sshUser string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.SshUser = sshUser
		return nil
	}
}

// Initial sets the SshKeyFile for the generated configuration
func InitialSshKeyFile(sshKeyFile string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.SshKeyFile = sshKeyFile
		return nil
	}
}

// Initial sets the BackupMount for the generated configuration
func InitialBackupMount(backupMount string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.BackupMount = backupMount
		return nil
	}
}

// Initial sets the AutomateInstanceCount for the generated configuration
func InitialAutomateInstanceCount(automateInstanceCount string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AutomateInstanceCount = automateInstanceCount
		return nil
	}
}

// Initial sets the AutomateConfigFile for the generated configuration
func InitialAutomateConfigFile(automateConfigFile string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AutomateConfigFile = automateConfigFile
		return nil
	}
}

// Initial sets the ChefServerInstanceCount for the generated configuration
func InitialChefServerInstanceCount(chefServerInstanceCount string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.ChefServerInstanceCount = chefServerInstanceCount
		return nil
	}
}

// Initial sets the ElasticSearchInstanceCount for the generated configuration
func InitialElasticSearchInstanceCount(elasticSearchInstanceCount string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.ElasticSearchInstanceCount = elasticSearchInstanceCount
		return nil
	}
}

// Initial sets the AutomateConfigFile for the generated configuration
func InitialPostgresqlInstanceCount(postgresqlInstanceCount string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.PostgresqlInstanceCount = postgresqlInstanceCount
		return nil
	}
}

// Initial sets the AwsProfile for the generated configuration
func InitialAwsProfile(awsProfile string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsProfile = awsProfile
		return nil
	}
}

// Initial sets the AwsRegion for the generated configuration
func InitialAwsRegion(awsRegion string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsRegion = awsRegion
		return nil
	}
}

// Initial sets the AwsSshKeyPairName for the generated configuration
func InitialAwsSshKeyPairName(awsSshKeyPairName string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsSshKeyPairName = awsSshKeyPairName
		return nil
	}
}

// Initial sets the AwsAutomateServerInstaceType for the generated configuration
func InitialAwsAutomateServerInstaceType(awsAutomateServerInstaceType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsAutomateServerInstaceType = awsAutomateServerInstaceType
		return nil
	}
}

// Initial sets the AwsChefServerInstanceType for the generated configuration
func InitialAwsChefServerInstanceType(awsChefServerInstanceType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsChefServerInstanceType = awsChefServerInstanceType
		return nil
	}
}

// Initial sets the AwsElasticSearchServerInstaceType for the generated configuration
func InitialAwsElasticSearchServerInstaceType(awsElasticSearchServerInstaceType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsElasticSearchServerInstaceType = awsElasticSearchServerInstaceType
		return nil
	}
}

// Initial sets the AwsPostgresqlServerInstanceType for the generated configuration
func InitialAwsPostgresqlServerInstanceType(awsPostgresqlServerInstanceType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsPostgresqlServerInstanceType = awsPostgresqlServerInstanceType
		return nil
	}
}

// Initial sets the AwsAutomateLBCertificateARN for the generated configuration
func InitialAwsAutomateLBCertificateARN(awsAutomateLBCertificateARN string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsAutomateLBCertificateARN = awsAutomateLBCertificateARN
		return nil
	}
}

// Initial sets the AwsChefServerLBCertificateARN for the generated configuration
func InitialAwsChefServerLBCertificateARN(awsChefServerLBCertificateARN string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsChefServerLBCertificateARN = awsChefServerLBCertificateARN
		return nil
	}
}

// Initial sets the AwsAutomateEbsVolumeIops for the generated configuration
func InitialAwsAutomateEbsVolumeIops(awsAutomateEbsVolumeIops string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsAutomateEbsVolumeIops = awsAutomateEbsVolumeIops
		return nil
	}
}

// Initial sets the AwsAutomateEbsVolumeSize for the generated configuration
func InitialAwsAutomateEbsVolumeSize(awsAutomateEbsVolumeSize string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsAutomateEbsVolumeSize = awsAutomateEbsVolumeSize
		return nil
	}
}

// Initial sets the AwsAutomateEbsVolumeType for the generated configuration
func InitialAwsAutomateEbsVolumeType(awsAutomateEbsVolumeType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsAutomateEbsVolumeType = awsAutomateEbsVolumeType
		return nil
	}
}

// Initial sets the AwsChefEbsVolumeIops for the generated configuration
func InitialAwsChefEbsVolumeIops(awsChefEbsVolumeIops string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsChefEbsVolumeIops = awsChefEbsVolumeIops
		return nil
	}
}

// Initial sets the AwsChefEbsVolumeSize for the generated configuration
func InitialAwsChefEbsVolumeSize(awsChefEbsVolumeSize string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsChefEbsVolumeSize = awsChefEbsVolumeSize
		return nil
	}
}

// Initial sets the AwsChefEbsVolumeType for the generated configuration
func InitialAwsChefEbsVolumeType(awsChefEbsVolumeType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsChefEbsVolumeType = awsChefEbsVolumeType
		return nil
	}
}

// Initial sets the AwsEsEbsVolumeSize for the generated configuration
func InitialAwsEsEbsVolumeSize(awsEsEbsVolumeSize string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsEsEbsVolumeSize = awsEsEbsVolumeSize
		return nil
	}
}

// Initial sets the AwsEsEbsVolumeType for the generated configuration
func InitialAwsEsEbsVolumeType(awsEsEbsVolumeType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsEsEbsVolumeType = awsEsEbsVolumeType
		return nil
	}
}

// Initial sets the AwsPgsEbsVolumeIops for the generated configuration
func InitialAwsPgsEbsVolumeIops(awsPgsEbsVolumeIops string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsPgsEbsVolumeIops = awsPgsEbsVolumeIops
		return nil
	}
}

// Initial sets the AwsPgsEbsVolumeSize for the generated configuration
func InitialAwsPgsEbsVolumeSize(awsPgsEbsVolumeSize string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsPgsEbsVolumeSize = awsPgsEbsVolumeSize
		return nil
	}
}

// Initial sets the AwsPgsEbsVolumeIops for the generated configuration
func InitialAwsPgsEbsVolumeType(awsPgsEbsVolumeType string) InitConfigHAOpt {
	return func(c *InitConfigHA) error {
		c.AwsPgsEbsVolumeType = awsPgsEbsVolumeType
		return nil
	}
}

// GenerateInitHAConfig constructions an InitConfig for HA mode, generating values
// not passed by the caller if possible.
func GenerateInitHAConfig(opts ...InitConfigHAOpt) (*InitConfigHA, error) {
	var err error

	cfg := NewInitConfigHA()
	for _, o := range opts {
		err = o(cfg)
		if err != nil {
			return nil, err
		}
	}

	return cfg, nil
}

// Render returns a user facing subset of the AutomateConfig as a TOML string.
func (c InitConfigHA) RenderHaSettings() (string, error) {
	temp := template.Must(template.New("init").
		Funcs(template.FuncMap{"StringsJoin": strings.Join}).
		Parse(haAwsConfigTemplate))

	var buf bytes.Buffer
	err := temp.Execute(&buf, c)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}

// Render returns a user facing subset of the AutomateConfig as a TOML string.
func (c InitConfigHA) RenderExistingNodesHaSettings() (string, error) {
	temp := template.Must(template.New("init").
		Funcs(template.FuncMap{"StringsJoin": strings.Join}).
		Parse(haExistingNodesConfigTemplate))

	var buf bytes.Buffer
	err := temp.Execute(&buf, c)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}
