package main

import (
	"testing"

	verification "github.com/chef/automate/lib/verification"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

const (
	configFileName       = "config.toml"
	parseConfigFileError = "Unable to parse config file"
)

func TestVerifyCmdFunc(t *testing.T) {
	type args struct {
		flagsObj *verifyCmdFlags
	}
	tests := []struct {
		name string
		args args
	}{
		{
			name: "Test VerifyCmdFunc",
			args: args{
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := verifyCmdFunc(tt.args.flagsObj)
			err := got(&cobra.Command{}, []string{})
			assert.Equal(t, nil, err)
		})
	}
}

func TestRunVerifyCmd(t *testing.T) {
	type args struct {
		cmd      *cobra.Command
		args     []string
		flagsObj *verifyCmdFlags
	}
	tests := []struct {
		name              string
		args              args
		A2HARBFileExist   bool
		ManagedServicesOn bool
		mockFunc          verification.Verification
		wantErr           bool
		err               error
	}{
		{
			name: "Test VerifyHaAWSProvision",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: true,
			mockFunc: &verification.VerificationMock{
				VerifyHAAWSProvisionFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaAWSManagedProvision",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyHAAWSManagedProvisionFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaOnpremDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyOnPremDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaOnPremAWSManagedDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyOnPremAWSManagedDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaOnPremCustManagedDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{configFileName},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyOnPremCustManagedDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaAWSDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: true,
			mockFunc: &verification.VerificationMock{
				VerifyHAAWSDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyHaAWSManagedDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: true,
			mockFunc: &verification.VerificationMock{
				VerifyHAAWSManagedDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: true,
			wantErr:           false,
		},
		{
			name: "Test VerifyStandaloneDeploy",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: false,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyStandaloneDeploymentFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
		{
			name: "Test VerifyCertificates",
			args: args{
				cmd:  &cobra.Command{},
				args: []string{},
				flagsObj: &verifyCmdFlags{
					certificates: true,
				},
			},
			A2HARBFileExist: false,
			mockFunc: &verification.VerificationMock{
				VerifyCertificatesFunc: func(configFile string) error {
					return nil
				},
			},
			ManagedServicesOn: false,
			wantErr:           false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := verifyCmdFlow{
				Verification:      tt.mockFunc,
				A2HARBFileExist:   tt.A2HARBFileExist,
				ManagedServicesOn: tt.ManagedServicesOn,
			}

			err := v.runVerifyCmd(tt.args.cmd, tt.args.args, tt.args.flagsObj)

			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyHaAWSProvision(t *testing.T) {
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
			name: "Test VerifyHaAWSProvision: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSProvisionFunc: func(configFile string) error {
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
		{
			name: "Test VerifyHaAWSProvision: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSProvisionFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyHaAWSProvision(tt.args.configPath)

			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyHaAWSManagedProvision(t *testing.T) {
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
			name: "Test VerifyHaAWSManagedProvision: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedProvisionFunc: func(configFile string) error {
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
		{
			name: "Test VerifyHaAWSManagedProvision: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedProvisionFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyHaAWSManagedProvision(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyHaAWSDeploy(t *testing.T) {
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
			name: "Test VerifyHaAWSDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   true,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
		{
			name: "Test VerifyHaAWSDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSDeploymentFunc: func(configFile string) error {
						return errors.New(parseConfigFileError)
					},
				},
				A2HARBFileExist:   true,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New(parseConfigFileError),
		},
		{
			name: "Test VerifyHaAWSDeploy: a2ha.rb file not present",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New(errProvisonInfra),
		},
		{
			name: "Test VerifyHaAWSDeploy: Managed services present",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: true,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New("This flag will not verify the Managed Services Setup. Please use the --ha-aws-managed-deploy flag."),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyHaAWSDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyHaAWSManagedDeploy(t *testing.T) {
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
			name: "Test VerifyHaAWSManagedDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   true,
				ManagedServicesOn: true,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
		{
			name: "Test VerifyHaAWSManagedDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedDeploymentFunc: func(configFile string) error {
						return errors.New(parseConfigFileError)
					},
				},
				A2HARBFileExist:   true,
				ManagedServicesOn: true,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New(parseConfigFileError),
		},
		{
			name: "Test VerifyHaAWSManagedDeploy: a2ha.rb file not present",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: true,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New(errProvisonInfra),
		},
		{
			name: "Test VerifyHaAWSManagedDeploy: Managed services not present",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyHAAWSManagedDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New("Managed Services flag is not set. Cannot verify the config."),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyHaAWSManagedDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyStandaloneDeploy(t *testing.T) {
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
			name: "Test VerifyStandaloneDeploy: check a2ha.rb file",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyStandaloneDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   true,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: true,
			err:     errors.New("Deployment type does not match with the requested flag."),
		},
		{
			name: "Test VerifyStandaloneDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyStandaloneDeploymentFunc: func(configFile string) error {
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
		{
			name: "Test VerifyStandaloneDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyStandaloneDeploymentFunc: func(configFile string) error {
						return nil
					},
				},
				A2HARBFileExist:   false,
				ManagedServicesOn: false,
			},
			args:    args{configPath: configFileName},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := &verifyCmdFlow{
				Verification:      tt.fields.Verification,
				A2HARBFileExist:   tt.fields.A2HARBFileExist,
				ManagedServicesOn: tt.fields.ManagedServicesOn,
			}
			err := v.verifyStandaloneDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

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

func TestVerifyHaOnPremAWSManagedDeploy(t *testing.T) {
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
			name: "Test VerifyHaOnPremAWSManagedDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremAWSManagedDeploymentFunc: func(configFile string) error {
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
			name: "Test VerifyHaOnPremAWSManagedDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremAWSManagedDeploymentFunc: func(configFile string) error {
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
			err := v.verifyHaOnPremAWSManagedDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}

func TestVerifyHaOnPremCustManagedDeploy(t *testing.T) {
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
			name: "Test VerifyHaOnPremCustManagedDeploy: verification successful",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremCustManagedDeploymentFunc: func(configFile string) error {
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
			name: "Test VerifyHaOnPremCustManagedDeploy: verification failed",
			fields: fields{
				Verification: &verification.VerificationMock{
					VerifyOnPremCustManagedDeploymentFunc: func(configFile string) error {
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
			err := v.verifyHaOnPremCustManagedDeploy(tt.args.configPath)
			if tt.wantErr {
				assert.EqualError(t, err, tt.err.Error())
			} else {
				assert.Equal(t, nil, err)
			}
		})
	}
}
