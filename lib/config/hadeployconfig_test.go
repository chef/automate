package config

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/io/fileutils"
	ptoml "github.com/pelletier/go-toml"
	"github.com/stretchr/testify/assert"
)

func TestParseHaDeployConfig(t *testing.T) {
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		want    *HaDeployConfig
		wantErr bool
		err     error
	}{
		{
			name: "Parse AWS Config",
			args: args{configFile: "./testdata/HaAws.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					Aws: &ConfigInitials{
						SSHUser:                     "",
						SSHGroupName:                "",
						SSHKeyFile:                  "",
						SSHPort:                     "",
						SecretsKeyFile:              "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile:            "/hab/a2_deploy_workspace/secrets.json",
						SudoPassword:                "",
						LoggingMonitoringManagement: "",
						Architecture:                "aws",
						WorkspacePath:               "/hab/a2_deploy_workspace",
						BackupMount:                 "/mnt/automate_backups",
						BackupConfig:                "",
						S3BucketName:                "",
						HabitatUIDGid:               "",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword: "",
						Fqdn:          "",
						ConfigFile:    "configs/automate.toml",
						TeamsPort:     "",
						ConfigSettings: ConfigSettings{
							InstanceCount:     "",
							EnableCustomCerts: false,
						},
					},
				},
				ChefServer: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Opensearch: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Postgresql: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Aws: &AwsSettings{
					Config: &ConfigAwsSettings{
						Profile:                      "",
						Region:                       "",
						AwsVpcID:                     "",
						AwsCidrBlockAddr:             "",
						PrivateCustomSubnets:         []string{},
						PublicCustomSubnets:          []string{},
						SSHKeyPairName:               "",
						SetupManagedServices:         false,
						AmiID:                        "",
						DeleteOnTermination:          true,
						AutomateServerInstanceType:   "",
						ChefServerInstanceType:       "",
						OpensearchServerInstanceType: "",
						PostgresqlServerInstanceType: "",
						AutomateLbCertificateArn:     "",
						ChefServerLbCertificateArn:   "",
						ChefEbsVolumeIops:            "",
						ChefEbsVolumeSize:            "",
						ChefEbsVolumeType:            "",
						OpensearchEbsVolumeIops:      "",
						OpensearchEbsVolumeSize:      "",
						OpensearchEbsVolumeType:      "",
						PostgresqlEbsVolumeIops:      "",
						PostgresqlEbsVolumeSize:      "",
						PostgresqlEbsVolumeType:      "",
						AutomateEbsVolumeIops:        "",
						AutomateEbsVolumeSize:        "",
						AutomateEbsVolumeType:        "",
						AmiFilterName:                "",
						AmiFilterVirtType:            "",
						AmiFilterOwner:               "",
						LbAccessLogs:                 "false",
						XContact:                     "",
						XDept:                        "",
						XProject:                     "",
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
		{
			name: "Parse AWS Managed Config",
			args: args{configFile: "./testdata/HaAwsManaged.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					Aws: &ConfigInitials{
						SSHUser:                     "",
						SSHGroupName:                "",
						SSHKeyFile:                  "",
						SSHPort:                     "",
						SecretsKeyFile:              "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile:            "/hab/a2_deploy_workspace/secrets.json",
						SudoPassword:                "",
						LoggingMonitoringManagement: "",
						Architecture:                "aws",
						WorkspacePath:               "/hab/a2_deploy_workspace",
						BackupMount:                 "/mnt/automate_backups",
						BackupConfig:                "",
						S3BucketName:                "",
						HabitatUIDGid:               "",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword: "",
						Fqdn:          "",
						ConfigFile:    "configs/automate.toml",
						TeamsPort:     "",
						ConfigSettings: ConfigSettings{
							InstanceCount:     "",
							EnableCustomCerts: false,
						},
					},
				},
				ChefServer: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Opensearch: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Postgresql: &ServerConfigSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Aws: &AwsSettings{
					Config: &ConfigAwsSettings{
						Profile:                      "",
						Region:                       "",
						AwsVpcID:                     "",
						AwsCidrBlockAddr:             "",
						PrivateCustomSubnets:         []string{},
						PublicCustomSubnets:          []string{},
						SSHKeyPairName:               "",
						SetupManagedServices:         true,
						AmiID:                        "",
						DeleteOnTermination:          true,
						AutomateServerInstanceType:   "",
						ChefServerInstanceType:       "",
						OpensearchServerInstanceType: "",
						PostgresqlServerInstanceType: "",
						AutomateLbCertificateArn:     "",
						ChefServerLbCertificateArn:   "",
						ChefEbsVolumeIops:            "",
						ChefEbsVolumeSize:            "",
						ChefEbsVolumeType:            "",
						OpensearchEbsVolumeIops:      "",
						OpensearchEbsVolumeSize:      "",
						OpensearchEbsVolumeType:      "",
						PostgresqlEbsVolumeIops:      "",
						PostgresqlEbsVolumeSize:      "",
						PostgresqlEbsVolumeType:      "",
						AutomateEbsVolumeIops:        "",
						AutomateEbsVolumeSize:        "",
						AutomateEbsVolumeType:        "",
						AmiFilterName:                "",
						AmiFilterVirtType:            "",
						AmiFilterOwner:               "",
						LbAccessLogs:                 "false",
						XContact:                     "",
						XDept:                        "",
						XProject:                     "",
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseHaDeployConfig(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			}
			// Compare the actual and expected configuration structs
			assert.Equal(t, tt.want, got)

		})
	}
}

func ParseHaDeployConfig(configFile string) (*HaDeployConfig, error) {
	fileUtils := &fileutils.FileSystemUtils{}
	templateBytes, err := fileUtils.ReadFile(configFile)
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := HaDeployConfig{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}
