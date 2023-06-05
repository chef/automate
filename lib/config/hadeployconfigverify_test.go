package config

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseAndVerify(t *testing.T) {
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
		err     error
	}{
		{
			name:    "Verify AWS Config",
			args:    args{configFile: "./testdata/HaAws.toml"},
			wantErr: true,
			err:     errors.New("Invalid or empty ssh_user\nInvalid or empty ssh_key_file\nInvalid or empty s3_bucketName.\nInvalid or empty aws profile name\nInvalid or empty aws region\nInvalid or empty aws aws_vpc_id\nInvalid or empty aws private_custom_subnets\nInvalid or empty aws public_custom_subnets\nInvalid or empty aws ssh_key_pair_name\nInvalid or empty aws automate_server_instance_type\nInvalid or empty aws chef_server_instance_type\nInvalid or empty aws opensearch_server_instance_type\nInvalid or empty aws postgresql_server_instance_type\nInvalid or empty aws automate_lb_certificate_arn\nInvalid or empty aws chef_server_lb_certificate_arn\nInvalid or empty aws automate_ebs_volume_iops\nInvalid or empty aws automate_ebs_volume_size\nInvalid or empty aws automate_ebs_volume_type\nInvalid or empty aws chef_ebs_volume_iops\nInvalid or empty aws chef_ebs_volume_size\nInvalid or empty aws chef_ebs_volume_type\nInvalid or empty aws opensearch_ebs_volume_iops\nInvalid or empty aws opensearch_ebs_volume_size\nInvalid or empty aws opensearch_ebs_volume_type\nInvalid or empty aws postgresql_ebs_volume_iops\nInvalid or empty aws postgresql_ebs_volume_size\nInvalid or empty aws postgresql_ebs_volume_type\nInvalid or empty URL: automate fqdn\nInvalid or empty automate instance_count\nInvalid or empty automate instance_count\nInvalid or empty opensearch instance_count\nInvalid or empty postgresql instance_count"),
		},
		{
			name:    "Verify AWS Managed Config",
			args:    args{configFile: "./testdata/HaAwsManaged.toml"},
			wantErr: true,
			err:     errors.New("Invalid ssh_key_file: ~/.ssh/central.pem (stat ~/.ssh/central.pem: no such file or directory)\nInvalid or empty URL: automate fqdn"),
		},
		{
			name:    "Verify OnPrem Config",
			args:    args{configFile: "./testdata/HaOnPrem.toml"},
			wantErr: true,
			err:     errors.New("Invalid or empty ssh_user\nInvalid or empty ssh_key_file\nInvalid value 'automate_backups' for field 'backup_mount'. Expected values are: /mnt/automate_backups\nautomate private ip 1324.2534.1is not valid\nInvalid or empty chef_server_private_ips\nURL should not include the protocol (http:// or https://): automate fqdn\nInvalid or empty automate instance_count\nInvalid value 'automate.toml' for field 'config_file'. Expected values are: configs/automate.toml\nInvalid format. Failed to decode root_ca for automate\nInvalid format. Failed to decode private_key for automate\nInvalid format. Failed to decode public_key for automate\nInvalid format. Failed to decode private_key for automate ip\nInvalid format. Failed to decode public_key for automate ip\nInvalid automate instance_count: two\nInvalid format. Failed to decode private_key for chef-server\nInvalid format. Failed to decode public_key for chef-server\nInvalid format. Failed to decode private_key for chef server ip\nInvalid format. Failed to decode public_key for chef server ip\nInvalid or empty opensearch instance_count\nOpensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.\nInvalid format. Failed to decode root_ca for opensearch\nInvalid format. Failed to decode admin_key for opensearch\nInvalid format. Failed to decode admin_cert for opensearch\nInvalid format. Failed to decode private_key for opensearch\nInvalid format. Failed to decode public_key for opensearch\nopensearch iproot_ca and/or public_key and/or private_key are missing in certs_by_ip. Otherwise set enable_custom_certs to false.\nInvalid or empty postgresql instance_count\nInvalid format. Failed to decode root_ca for postgresql\nInvalid format. Failed to decode private_key for postgresql\nInvalid format. Failed to decode public_key for postgresql\npostgresql ip 0.0.1 for certs is not valid\nInvalid format. Failed to decode private_key for postgresql ip\nInvalid format. Failed to decode public_key for postgresql ip"),
		},
		{
			name:    "Verify OnPrem Db Self-Managed Config",
			args:    args{configFile: "./testdata/HaOnPremDbSelfManaged.toml"},
			wantErr: true,
			err:     errors.New("Invalid ssh_key_file: ~/.ssh/A2HA.pem (stat ~/.ssh/A2HA.pem: no such file or directory)\nInvalid or empty dbuser_username\nInvalid or empty dbuser_password\nInvalid or empty instance_url\nInvalid or empty postgresql_root_cert\nInvalid or empty superuser_password\nInvalid or empty superuser_username\nInvalid or empty opensearch_domain_name\nInvalid or empty URL: opensearch_domain_url\nInvalid or empty opensearch_root_cert\nInvalid or empty opensearch_user_password\nInvalid or empty opensearch_username"),
		},
		{
			name:    "Verify OnPrem Db Aws Managed Config",
			args:    args{configFile: "./testdata/HaOnPremDbAwsManaged.toml"},
			wantErr: false,
			err:     nil,
		},
		{
			name:    "Invalid Architecture toml file",
			args:    args{configFile: "./testdata/InvalidArchitecture.toml"},
			wantErr: true,
			err:     errors.New("invalid ha deploy config"),
		},
		{
			name:    "Invalid toml file path",
			args:    args{configFile: ""},
			wantErr: true,
			err:     errors.New("config file path is empty"),
		},
		{
			name:    "error parsing toml file",
			args:    args{configFile: "./testdata/UnmarshalErr.toml"},
			wantErr: true,
			err:     errors.New("error unmarshalling config TOML file: (5, 2): unexpected token table key cannot contain ']', was expecting a table key"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			config := HaDeployConfig{}
			err := config.ParseAndVerify(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}
