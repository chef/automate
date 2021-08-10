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
