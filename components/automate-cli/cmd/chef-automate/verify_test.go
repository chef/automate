package main

import (
	"testing"

	verification "github.com/chef/automate/lib/verification"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	configFileName       = "config.toml"
	parseConfigFileError = "Unable to parse config file"
)

func TestVerifyHaOnpremDeploy(t *testing.T) {
	type fields struct {
		Verification      verification.Verification
		A2HARBFileExist   bool
		ManagedServicesOn bool
	}
	type args struct {
		configPath string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
		err     error
	}{
		{
			name: "Test VerifyHaOnpremDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
		{
			name: "Test VerifyHaOnpremDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremDeploymentFunc: func(configFile string) error {
						return errors.New(parseConfigFileError)
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New(parseConfigFileError),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyHaOnpremDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}
