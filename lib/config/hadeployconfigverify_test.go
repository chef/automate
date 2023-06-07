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
			err:     errors.New("invalid or empty: ssh_user\ninvalid or empty: ssh_key_file\ninvalid or empty s3_bucketName\ninvalid or empty: aws profile name\ninvalid or empty: aws region\ninvalid or empty: aws aws_vpc_id\ninvalid or empty: aws private_custom_subnets\ninvalid or empty: aws public_custom_subnets\ninvalid or empty: aws ssh_key_pair_name\ninvalid or empty: aws ami_id\ninvalid or empty: aws automate_server_instance_type\ninvalid or empty: aws automate_lb_certificate_arn\nempty value: aws automate_ebs_volume_iops\nempty value: aws automate_ebs_volume_size\ninvalid or empty: aws automate_ebs_volume_type\nempty value: aws chef_ebs_volume_iops\nempty value: aws chef_ebs_volume_size\ninvalid or empty: aws chef_ebs_volume_type\ninvalid or empty: aws chef_server_instance_type\ninvalid or empty: aws chef_server_lb_certificate_arn\ninvalid or empty: aws opensearch_server_instance_type\ninvalid or empty: aws postgresql_server_instance_type\nempty value: aws opensearch_ebs_volume_iops\nempty value: aws opensearch_ebs_volume_size\ninvalid or empty: aws opensearch_ebs_volume_type\nempty value: aws postgresql_ebs_volume_iops\nempty value: aws postgresql_ebs_volume_size\ninvalid or empty: aws postgresql_ebs_volume_type\ninvalid or empty URL: automate fqdn\nempty value: automate instance_count\nempty value: chef server instance_count\nempty value: opensearch instance_count\nempty value: postgresql instance_count"),
		},
		{
			name:    "Verify AWS Managed Config",
			args:    args{configFile: "./testdata/HaAwsManaged.toml"},
			wantErr: true,
			err:     errors.New("invalid ssh_key_file: ~/.ssh/central.pem (stat ~/.ssh/central.pem: no such file or directory)\ninvalid or empty URL: automate fqdn"),
		},
		{
			name:    "Verify OnPrem Config",
			args:    args{configFile: "./testdata/HaOnPrem.toml"},
			wantErr: true,
			err:     errors.New("invalid or empty: ssh_user\ninvalid or empty: ssh_key_file\nautomate private ip 1324.2534.1is not valid\ninvalid or empty: chef_server_private_ips\ninvalid or empty: opensearch_private_ips\ninvalid or empty: postgresql_private_ips\nurl should not include the protocol (http:// or https://): automate fqdn\nempty value: automate instance_count\ninvalid value 'automate.toml' for field 'config_file'. Expected values are: configs/automate.toml\ninvalid format. Failed to decode root_ca for automate\ninvalid format. Failed to decode private_key for automate\ninvalid format. Failed to decode public_key for automate\ninvalid format. Failed to decode private_key for automate ip\ninvalid format. Failed to decode public_key for automate ip\ninvalid value 'chef server instance_count' for field 'two'\ninvalid format. Failed to decode private_key for chef-server\ninvalid format. Failed to decode public_key for chef-server\ninvalid format. Failed to decode private_key for chef server ip\ninvalid format. Failed to decode public_key for chef server ip\nempty value: opensearch instance_count\nopensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false\nopensearch iproot_ca and/or public_key and/or private_key are missing in certs_by_ip. Otherwise set enable_custom_certs to false\nempty value: postgresql instance_count\ninvalid format. Failed to decode root_ca for postgresql\ninvalid format. Failed to decode private_key for postgresql\ninvalid format. Failed to decode public_key for postgresql\npostgresql ip 0.0.1 for certs is not valid\ninvalid format. Failed to decode private_key for postgresql ip\ninvalid format. Failed to decode public_key for postgresql ip"),
		},
		{
			name:    "Verify OnPrem Db Self-Managed Config",
			args:    args{configFile: "./testdata/HaOnPremDbSelfManaged.toml"},
			wantErr: true,
			err:     errors.New("invalid ssh_key_file: ~/.ssh/A2HA.pem (stat ~/.ssh/A2HA.pem: no such file or directory)\ninvalid or empty: dbuser_username\ninvalid or empty: dbuser_password\ninvalid or empty URL: instance_url\ninvalid or empty: postgresql_root_cert\ninvalid or empty: superuser_password\ninvalid or empty: superuser_username\ninvalid or empty: opensearch_domain_name\ninvalid or empty URL: opensearch_domain_url\ninvalid or empty: opensearch_root_cert\ninvalid or empty: opensearch_user_password\ninvalid or empty: opensearch_username"),
		},
		{
			name:    "Verify OnPrem Db Aws Managed Config",
			args:    args{configFile: "./testdata/HaOnPremDbAwsManaged.toml"},
			wantErr: false,
			err:     nil,
		},
		{
			name:    "Verify Aws Chef Managed Config",
			args:    args{configFile: "./testdata/HaAwsChefManaged.toml"},
			wantErr: true,
			err:     errors.New("invalid value '1234567' for field 'ssh_port' port number must be between 1 and 65535\ninvalid or empty: ssh_key_file\ninvalid or empty: aws profile name\ninvalid or empty: aws region\ninvalid or empty: aws aws_vpc_id\nminimum number of aws private_custom_subnets required is 3\ninvalid or empty: aws public_custom_subnets\ninvalid or empty: aws ssh_key_pair_name\ninvalid or empty: aws ami_id\ninvalid or empty: aws automate_server_instance_type\ninvalid or empty: aws automate_lb_certificate_arn\nempty value: aws automate_ebs_volume_iops\nempty value: aws automate_ebs_volume_size\ninvalid or empty: aws automate_ebs_volume_type\nempty value: aws chef_ebs_volume_iops\nempty value: aws chef_ebs_volume_size\ninvalid or empty: aws chef_ebs_volume_type\ninvalid or empty: aws chef_server_instance_type\ninvalid or empty: aws chef_server_lb_certificate_arn\ninvalid or empty: aws opensearch_server_instance_type\ninvalid or empty: aws postgresql_server_instance_type\nempty value: aws opensearch_ebs_volume_iops\nempty value: aws opensearch_ebs_volume_size\ninvalid or empty: aws opensearch_ebs_volume_type\nempty value: aws postgresql_ebs_volume_iops\nempty value: aws postgresql_ebs_volume_size\ninvalid or empty: aws postgresql_ebs_volume_type\ninvalid or empty URL: automate fqdn\npassword is too short (must be at least 8 characters)\nempty value: automate instance_count\nautomate root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false\nempty value: chef server instance_count\nchefServer public_key and/or private_key are missing. Otherwise set enable_custom_certs to false\nempty value: opensearch instance_count\nopensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false\nempty value: postgresql instance_count\npostgresql root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false"),
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
			config := &HaDeployConfig{}
			err := config.ParseAndVerify(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}
