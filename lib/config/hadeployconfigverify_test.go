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
		{
			name:    "Parse AWS Managed Config",
			args:    args{configFile: "./testdata/HaAwsManaged.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New(""),
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
