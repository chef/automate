package config

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVerify(t *testing.T) {
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
		// {
		// 	name:    "Verify AWS Config",
		// 	args:    args{configFile: "./testdata/HaAws.toml"},
		// 	want:    nil,
		// 	wantErr: true,
		// 	err:     errors.New("Invalid or empty ssh_user\nInvalid or empty ssh_key_file\nInvalid or empty s3_bucketName.\nInvalid or empty aws profile name\nInvalid or empty aws region\nInvalid or empty aws aws_vpc_id\nInvalid or empty aws private_custom_subnets\nInvalid or empty aws public_custom_subnets\nInvalid or empty aws ssh_key_pair_name\nInvalid or empty aws automate_server_instance_type\nInvalid or empty aws chef_server_instance_type\nInvalid or empty aws opensearch_server_instance_type\nInvalid or empty aws postgresql_server_instance_type\nInvalid or empty aws automate_lb_certificate_arn\nInvalid or empty aws chef_server_lb_certificate_arn\nInvalid or empty aws automate_ebs_volume_iops\nInvalid or empty aws automate_ebs_volume_size\nInvalid or empty aws automate_ebs_volume_type\nInvalid or empty aws chef_ebs_volume_iops\nInvalid or empty aws chef_ebs_volume_size\nInvalid or empty aws chef_ebs_volume_type\nInvalid or empty aws opensearch_ebs_volume_iops\nInvalid or empty aws opensearch_ebs_volume_size\nInvalid or empty aws opensearch_ebs_volume_type\nInvalid or empty aws postgresql_ebs_volume_iops\nInvalid or empty aws postgresql_ebs_volume_size\nInvalid or empty aws postgresql_ebs_volume_type\nInvalid or empty URL: automate fqdn\nInvalid or empty automate instance_count\nInvalid or empty automate instance_count\nInvalid or empty opensearch instance_count\nInvalid or empty postgresql instance_count"),
		// },
		// {
		// 	name:    "Verify AWS Managed Config",
		// 	args:    args{configFile: "./testdata/HaAwsManaged.toml"},
		// 	want:    nil,
		// 	wantErr: true,
		// 	err:     errors.New("Invalid ssh_key_file: ~/.ssh/central.pem (path does not exist)\nInvalid or empty URL: automate fqdn\nUnknown certificate type: private_key for automate\nUnknown certificate type: public_key for automate\nUnknown certificate type: private_key for chef-server\nUnknown certificate type: public_key for chef-server\nUnknown certificate type: admin_key for opensearch\nUnknown certificate type: admin_cert for opensearch\nUnknown certificate type: private_key for opensearch\nUnknown certificate type: public_key for opensearch\nUnknown certificate type: private_key for postgresql\nUnknown certificate type: public_key for postgresql"),
		// },
		{
			name:    "Verify OnPrem Config",
			args:    args{configFile: "./testdata/HaOnPrem.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("Invalid or empty ssh_user\nInvalid or empty ssh_key_file\nInvalid or empty backup_config\nInvalid or empty automate_private_ips\nInvalid or empty chef_server_private_ips\nInvalid or empty URL: automate fqdn\nInvalid or empty automate instance_count\nInvalid format. Failed to decode root_ca for automate\nInvalid format. Failed to decode private_key for automate\nInvalid format. Failed to decode public_key for automate\nInvalid format. Failed to decode private_key for automate ip\nInvalid format. Failed to decode public_key for automate ip\nInvalid or empty automate instance_count\nInvalid format. Failed to decode private_key for chef-server\nInvalid format. Failed to decode public_key for chef-server\nInvalid format. Failed to decode private_key for chef server ip\nInvalid format. Failed to decode public_key for chef server ip\nInvalid or empty opensearch instance_count\nInvalid format. Failed to decode root_ca for opensearch\nInvalid format. Failed to decode admin_key for opensearch\nInvalid format. Failed to decode admin_cert for opensearch\nInvalid format. Failed to decode private_key for opensearch\nInvalid format. Failed to decode public_key for opensearch\nInvalid format. Failed to decode private_key for opensearch ip\nInvalid format. Failed to decode public_key for opensearch ip\nInvalid or empty postgresql instance_count\nInvalid format. Failed to decode root_ca for postgresql\nInvalid format. Failed to decode private_key for postgresql\nInvalid format. Failed to decode public_key for postgresql\nInvalid format. Failed to decode private_key for postgresql ip\nInvalid format. Failed to decode public_key for postgresql ip"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			config := HaDeployConfig{}
			err := config.Verify(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			} else {
				assert.Equal(t, tt.want, nil)
			}
		})
	}
}
