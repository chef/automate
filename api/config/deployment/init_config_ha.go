package deployment

import (
	"bytes"
	"strings"
	"text/template"
)

var initConfigHAPathFlags = struct {
	path string
}{}

var initConfigHAFlags = struct {
	SecretsKeyFile                    string `toml:"architecture.aws.secrets_key_file"`
	SecretsStoreFile                  string `toml:"architecture.aws.secrets_store_file"`
	Architecture                      string `toml:"architecture.aws.architecture"`
	WorkspacePath                     string `toml:"architecture.aws.workspace_path"`
	SshUser                           string `toml:"architecture.aws.ssh_user"`
	SshKeyFile                        string `toml:"architecture.aws.ssh_key_file"`
	SudoPassword                      string `toml:"architecture.aws.sudo_password"`
	BackupMount                       string `toml:"architecture.aws.backup_mount"`
	AutomateAdminPassword             string `toml:"automate.config.admin_password"`
	AutomateFQDN                      string `toml:"automate.config.fqdn"`
	AutomateTeamsPort                 string `toml:"automate.config.AutomateTeamsPort"`
	AutomateInstanceCount             string `toml:"automate.config.instance_count"`
	AutomateConfigFile                string `toml:"automate.config.config_file"`
	ChefServerInstanceCount           string `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount        string `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount           string `toml:"postgresql.config.instance_count"`
	AwsProfile                        string `toml:"aws.config.profile"`
	AwsRegion                         string `toml:"aws.config.region"`
	AwsSshKeyPairName                 string `toml:"aws.config.ssh_key_pair_name"`
	AwsAutomateServerInstaceType      string `toml:"aws.config.automate_server_instance_type"`
	AwsChefServerInstanceType         string `toml:"aws.config.chef_server_instance_type"`
	AwsElasticSearchServerInstaceType string `toml:"aws.config.elasticsearch_server_instance_type"`
	AwsPostgresqlServerInstanceType   string `toml:"aws.config.postgresql_server_instance_type"`
	AwsAutomateLBCertificateARN       string `toml:"aws.config.automate_lb_certificate_arn"`
	AwsChefServerLBCertificateARN     string `toml:"aws.config.chef_server_lb_certificate_arn"`
	AwsAutomateEbsVolumeIops          string `toml:"aws.config.automate_ebs_volume_iops"`
	AwsAutomateEbsVolumeSize          string `toml:"aws.config.automate_ebs_volume_size"`
	AwsAutomateEbsVolumeType          string `toml:"aws.config.automate_ebs_volume_type"`
	AwsChefEbsVolumeIops              string `toml:"aws.config.chef_ebs_volume_iops"`
	AwsChefEbsVolumeSize              string `toml:"aws.config.chef_ebs_volume_size"`
	AwsChefEbsVolumeType              string `toml:"aws.config.chef_ebs_volume_type"`
	AwsEsEbsVolumeIops                string `toml:"aws.config.elasticsearch_ebs_volume_iops"`
	AwsEsEbsVolumeSize                string `toml:"aws.config.elasticsearch_ebs_volume_size"`
	AwsEsEbsVolumeType                string `toml:"aws.config.elasticsearch_ebs_volume_type"`
	AwsPgsEbsVolumeIops               string `toml:"aws.config.postgresql_ebs_volume_iops"`
	AwsPgsEbsVolumeSize               string `toml:"aws.config.postgresql_ebs_volume_size"`
	AwsPgsEbsVolumeType               string `toml:"aws.config.postgresql_ebs_volume_type"`
	AwsTagContact                     string `toml:"aws.config.X-Contact"`
	AwsTagDept                        string `toml:"aws.config.X-Dept"`
	AwsTagProject                     string `toml:"aws.config.X-Project"`
}{}

var initConfigHAExistingNodesFlags = struct {
	SecretsKeyFile                       string `toml:"architecture.existing_nodes.secrets_key_file"`
	SecretsStoreFile                     string `toml:"architecture.existing_nodes.secrets_store_file"`
	Architecture                         string `toml:"architecture.existing_nodes.architecture"`
	WorkspacePath                        string `toml:"architecture.existing_nodes.workspace_path"`
	SshUser                              string `toml:"architecture.existing_nodes.ssh_user"`
	SshKeyFile                           string `toml:"architecture.existing_nodes.ssh_key_file"`
	SudoPassword                         string `toml:"architecture.existing_nodes.sudo_password"`
	BackupMount                          string `toml:"architecture.existing_nodes.backup_mount"`
	AutomateAdminPassword                string `toml:"automate.config.admin_password"`
	AutomateFQDN                         string `toml:"automate.config.fqdn"`
	AutomateTeamsPort                    string `toml:"automate.config.AutomateTeamsPort"`
	AutomateInstanceCount                string `toml:"automate.config.instance_count"`
	AutomateConfigFile                   string `toml:"automate.config.config_file"`
	ChefServerInstanceCount              string `toml:"chef_server.config.instance_count"`
	ElasticSearchInstanceCount           string `toml:"elasticsearch.config.instance_count"`
	PostgresqlInstanceCount              string `toml:"postgresql.config.instance_count"`
	ExistingNodesAutomateIPs             string `toml:"existing_nodes.config.automate_ips"`
	ExistingNodesAutomatePrivateIPs      string `toml:"existing_nodes.config.chef_server_ips"`
	ExistingNodesChefServerPrivateIPs    string `toml:"existing_nodes.config.chef_server_private_ips"`
	ExistingNodesElasticsearchIPs        string `toml:"existing_nodes.config.elasticsearch_ips"`
	ExistingNodesElasticsearchPrivateIPs string `toml:"existing_nodes.config.elasticsearch_private_ips"`
	ExistingNodesPostgresqlIPs           string `toml:"existing_nodes.config.postgresql_ips"`
	ExistingNodesPostgresqlPrivateIps    string `toml:"existing_nodes.config.postgresql_private_ips"`
}{}

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
