package config_parser

import (
	"errors"
	"reflect"
	"testing"

	sc "github.com/chef/automate/api/config/deployment"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/stretchr/testify/assert"
)

func TestConfigParserImpl_ParseAWSAutomateConfig(t *testing.T) {
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		want    *HAAwsConfigToml
		wantErr bool
		err     error
	}{
		{
			name: "Parse AWS Config successful",
			args: args{configFile: "./testdata/AWSConfig.toml"},
			want: &HAAwsConfigToml{Architecture: struct {
				ConfigInitials struct {
					SecretsKeyFile              string "toml:\"secrets_key_file\""
					SecretsStoreFile            string "toml:\"secrets_store_file\""
					Architecture                string "toml:\"architecture\""
					WorkspacePath               string "toml:\"workspace_path\""
					SSHUser                     string "toml:\"ssh_user\""
					SSHKeyFile                  string "toml:\"ssh_key_file\""
					SSHPort                     string "toml:\"ssh_port\""
					SudoPassword                string "toml:\"sudo_password\""
					LoggingMonitoringManagement string "toml:\"logging_monitoring_management\""
					NewElk                      string "toml:\"new_elk\""
					ExistingElkInstanceIP       string "toml:\"existing_elk_instance_ip\""
					ExistingElkPort             string "toml:\"existing_elk_port\""
					ExistingElkCert             string "toml:\"existing_elk_cert\""
					ExistingElkUsername         string "toml:\"existing_elk_username\""
					ExistingElkPassword         string "toml:\"existing_elk_password\""
					BackupMount                 string "toml:\"backup_mount\""
					BackupConfig                string "toml:\"backup_config\""
					S3BucketName                string "toml:\"s3_bucketName\""
					HabitatUIDGid               string "toml:\"habitat_uid_gid\""
				} "toml:\"aws\""
			}{
				ConfigInitials: struct {
					SecretsKeyFile              string "toml:\"secrets_key_file\""
					SecretsStoreFile            string "toml:\"secrets_store_file\""
					Architecture                string "toml:\"architecture\""
					WorkspacePath               string "toml:\"workspace_path\""
					SSHUser                     string "toml:\"ssh_user\""
					SSHKeyFile                  string "toml:\"ssh_key_file\""
					SSHPort                     string "toml:\"ssh_port\""
					SudoPassword                string "toml:\"sudo_password\""
					LoggingMonitoringManagement string "toml:\"logging_monitoring_management\""
					NewElk                      string "toml:\"new_elk\""
					ExistingElkInstanceIP       string "toml:\"existing_elk_instance_ip\""
					ExistingElkPort             string "toml:\"existing_elk_port\""
					ExistingElkCert             string "toml:\"existing_elk_cert\""
					ExistingElkUsername         string "toml:\"existing_elk_username\""
					ExistingElkPassword         string "toml:\"existing_elk_password\""
					BackupMount                 string "toml:\"backup_mount\""
					BackupConfig                string "toml:\"backup_config\""
					S3BucketName                string "toml:\"s3_bucketName\""
					HabitatUIDGid               string "toml:\"habitat_uid_gid\""
				}{
					SecretsKeyFile:   "/hab/a2_deploy_workspace/secrets.key",
					SecretsStoreFile: "/hab/a2_deploy_workspace/secrets.json",
					Architecture:     "aws",
					WorkspacePath:    "/hab/a2_deploy_workspace",
					BackupMount:      "/mnt/automate_backups",
				},
			}},
			wantErr: false,
			err:     nil,
		},
		{
			name:    "Parse AWs Config file not found",
			args:    args{configFile: "AWSConfig.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error in reading config toml file: open AWSConfig.toml: no such file or directory"),
		},
		{
			name:    "Parse AWs Config failed",
			args:    args{configFile: "./testdata/AWSConfigFailure.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error in unmarshalling config toml file: (30, 21): unescaped control character U+000A"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cp := &ConfigParserImpl{}
			got, err := cp.ParseAWSAutomateConfig(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ConfigParserImpl.ParseAWSAutomateConfig() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParseOnPremConfig(t *testing.T) {
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		want    *HAOnPremConfigToml
		wantErr bool
		err     error
	}{
		{
			name: "Parse OnPrem Config successful",
			args: args{configFile: "./testdata/OnPremConfig.toml"},
			want: &HAOnPremConfigToml{Architecture: struct {
				ConfigInitials struct {
					SecretsKeyFile              string "toml:\"secrets_key_file,omitempty\""
					SecretsStoreFile            string "toml:\"secrets_store_file,omitempty\""
					Architecture                string "toml:\"architecture,omitempty\""
					WorkspacePath               string "toml:\"workspace_path,omitempty\""
					SSHUser                     string "toml:\"ssh_user,omitempty\""
					SSHKeyFile                  string "toml:\"ssh_key_file,omitempty\""
					SSHPort                     string "toml:\"ssh_port,omitempty\""
					SudoPassword                string "toml:\"sudo_password,omitempty\""
					LoggingMonitoringManagement string "toml:\"logging_monitoring_management,omitempty\""
					NewElk                      string "toml:\"new_elk,omitempty\""
					ExistingElkInstanceIP       string "toml:\"existing_elk_instance_ip,omitempty\""
					ExistingElkPort             string "toml:\"existing_elk_port,omitempty\""
					ExistingElkCert             string "toml:\"existing_elk_cert,omitempty\""
					ExistingElkUsername         string "toml:\"existing_elk_username,omitempty\""
					ExistingElkPassword         string "toml:\"existing_elk_password,omitempty\""
					BackupMount                 string "toml:\"backup_mount,omitempty\""
					HabitatUIDGid               string "toml:\"habitat_uid_gid,omitempty\""
					BackupConfig                string "toml:\"backup_config,omitempty\""
				} "toml:\"existing_infra\""
			}{
				ConfigInitials: struct {
					SecretsKeyFile              string "toml:\"secrets_key_file,omitempty\""
					SecretsStoreFile            string "toml:\"secrets_store_file,omitempty\""
					Architecture                string "toml:\"architecture,omitempty\""
					WorkspacePath               string "toml:\"workspace_path,omitempty\""
					SSHUser                     string "toml:\"ssh_user,omitempty\""
					SSHKeyFile                  string "toml:\"ssh_key_file,omitempty\""
					SSHPort                     string "toml:\"ssh_port,omitempty\""
					SudoPassword                string "toml:\"sudo_password,omitempty\""
					LoggingMonitoringManagement string "toml:\"logging_monitoring_management,omitempty\""
					NewElk                      string "toml:\"new_elk,omitempty\""
					ExistingElkInstanceIP       string "toml:\"existing_elk_instance_ip,omitempty\""
					ExistingElkPort             string "toml:\"existing_elk_port,omitempty\""
					ExistingElkCert             string "toml:\"existing_elk_cert,omitempty\""
					ExistingElkUsername         string "toml:\"existing_elk_username,omitempty\""
					ExistingElkPassword         string "toml:\"existing_elk_password,omitempty\""
					BackupMount                 string "toml:\"backup_mount,omitempty\""
					HabitatUIDGid               string "toml:\"habitat_uid_gid,omitempty\""
					BackupConfig                string "toml:\"backup_config,omitempty\""
				}{
					SecretsKeyFile:   "/hab/a2_deploy_workspace/secrets.key",
					SecretsStoreFile: "/hab/a2_deploy_workspace/secrets.json",
					Architecture:     "existing_nodes",
					WorkspacePath:    "/hab/a2_deploy_workspace",
					SSHPort:          "22",
					BackupMount:      "/mnt/automate_backups",
				},
			}},
			wantErr: false,
			err:     nil,
		},
		{
			name:    "Parse OnPrem Config file not found",
			args:    args{configFile: "OnPremConfig.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error in reading config toml file: open OnPremConfig.toml: no such file or directory"),
		},
		{
			name:    "Parse OnPrem Config failed",
			args:    args{configFile: "./testdata/OnPremConfigFailure.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error in unmarshalling config toml file: (10, 21): unescaped control character U+000A"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cp := &ConfigParserImpl{}
			got, err := cp.ParseOnPremConfig(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ConfigParserImpl.ParseOnPremConfig() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestConfigParserImpl_ParseStandaloneConfig(t *testing.T) {
	Cfg := sc.NewAutomateConfig()
	Cfg.Global.V1.Fqdn = w.String("Public_DNS_name")
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		want    *sc.AutomateConfig
		wantErr bool
		err     error
	}{
		{
			name: "Parse Standalone Config successful",
			args: args{
				configFile: "./testdata/StandaloneConfig.toml",
			},
			want:    Cfg,
			wantErr: false,
		},
		{
			name: "Parse Standalone Config failed",
			args: args{
				configFile: "StandaloneConfig.toml",
			},
			want:    &sc.AutomateConfig{},
			wantErr: true,
			err:     errors.New("Failed to read config file located at StandaloneConfig.toml: open StandaloneConfig.toml: no such file or directory"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cp := &ConfigParserImpl{}
			got, err := cp.ParseStandaloneConfig(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			} else {
				assert.Equal(t, tt.want.Global.V1.Fqdn.GetValue(), got.Global.V1.Fqdn.GetValue())
			}

		})
	}
}
