package main

import (
	"context"
	"os"
	"testing"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"
)

var nodeDelCmd = &cobra.Command{
	Use:   "node-delete [uuid]",
	Short: "Delete node by node uuid",
	Long:  "",
	RunE:  runDeleteNodeCmd,
	Args:  cobra.ExactArgs(1),
}

var infrastructureCmdTest = &cobra.Command{
	Use:               "infrastructure COMMAND",
	Short:             "Chef Automate infrastructure",
	Long:              "Test for infra cmd",
	PersistentPreRunE: preInfrastructureCmd,
}

func Test_runpreInfrastructureCmd(t *testing.T) {
	tests := []struct {
		testName string
		cmd      *cobra.Command
		args     []string
	}{
		{"Test node delete", infrastructureCmdTest, []string{"uuid of node"}},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			err := preInfrastructureCmd(tt.cmd, tt.args)
			if err == nil {
				assert.NoError(t, err)
			}
			assert.Error(t, err)
		})
	}
}

func Test_runDeleteNodeCmd(t *testing.T) {
	tests := []struct {
		testName string
		cmd      *cobra.Command
		args     []string
	}{
		{"Test node delete", nodeDelCmd, []string{"uuid of node"}},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			err := runDeleteNodeCmd(tt.cmd, tt.args)
			if err == nil {
				assert.NoError(t, err)
			}
			assert.Error(t, err)
		})
	}
}

type MockDSClient struct {
	InfrastructureNodeDeleteFunc func(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error)
	CloseFunc                    func() error
}

func (mds *MockDSClient) InfrastructureNodeDelete(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error) {
	return mds.InfrastructureNodeDeleteFunc(ctx, in, opts...)
}

func (mds *MockDSClient) Close() error {
	return mds.CloseFunc()
}

func TestRunDeleteNode(t *testing.T) {
	customWriter := majorupgrade_utils.NewCustomWriter()
	i := &InfraFlow{
		DsClient: &MockDSClient{InfrastructureNodeDeleteFunc: func(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error) {
			return &api.InfrastructureNodeDeleteResponse{}, nil
		}, CloseFunc: func() error {
			return nil
		}},
		Writer: customWriter.CliWriter,
	}
	nodeId := "3d8ffe06-6281-494a-9957-34c6f3f50154"
	err := i.RunDeleteNode(nodeId)
	assert.Equal(t, err, nil)

}

func TestRunDeleteNodeFailed(t *testing.T) {
	customWriter := majorupgrade_utils.NewCustomWriter()
	i := &InfraFlow{
		DsClient: &MockDSClient{InfrastructureNodeDeleteFunc: func(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error) {
			return nil, errors.New("DeploymentServiceCallError")
		}, CloseFunc: func() error {
			return nil
		}},
		Writer: customWriter.CliWriter,
	}
	nodeId := "3d8ffe06-6281-494a-9957-34c6f3f50154"
	err := i.RunDeleteNode(nodeId)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Request to delete node failed: DeploymentServiceCallError")
}

func TestRunDeleteNodeFailedForInvaliUUID(t *testing.T) {
	customWriter := majorupgrade_utils.NewCustomWriter()
	i := &InfraFlow{
		DsClient: &MockDSClient{InfrastructureNodeDeleteFunc: func(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error) {
			return &api.InfrastructureNodeDeleteResponse{}, nil
		}, CloseFunc: func() error {
			return nil
		}},
		Writer: customWriter.CliWriter,
	}
	nodeId := "not-a-valid-uuid"
	err := i.RunDeleteNode(nodeId)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "argument in not a valid node UUID")
}

func Test_readFileAndMarshal(t *testing.T) {

	tests := []struct {
		name       string
		fileData   string
		want       *LicenseResult
		wantErr    bool
		fileCreate bool
	}{
		{
			name: "Success File Parsing",
			fileData: `
			{
				"command": "chef-automate license status --result-json output.jso",
				"status": "OK",
				"error_code": 0,
				"error_description": "",
				"error_cause": "",
				"error_stack_trace": "",
				"error_recovery": "",
				"error_type": "",
				"result":
				{
					"set": true,
					"license_id": "test-license-Id",
					"customer_name": "test-customer",
					"expiration_date":
					{
						"seconds": 1738281599
					},
					"deployment_id": "test-deployment-id",
					"deployment_type": "Standalone",
					"license_type": "internal"
				}	
			}`,
			want: &LicenseResult{
				Result: LicenseStatus{
					CustomerName: "test-customer",
					LicenseType:  "internal",
					ExpirationDate: ExpirationDate{
						Seconds: int64(1738281599),
					},
					LicenseId: "test-license-Id",
				},
				ErrorType:        "",
				ErrorDescription: "",
			},
			wantErr:    false,
			fileCreate: true,
		}, {
			name:       "Success File Parsing",
			fileData:   "",
			want:       nil,
			wantErr:    true,
			fileCreate: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.fileCreate {
				f, _ := os.Create("test.txt")
				f.WriteString(tt.fileData)
				defer f.Close()
			}

			got, err := readFileAndMarshal("test.txt")
			if tt.wantErr {
				assert.NotNil(t, err)
			}

			assert.Equal(t, tt.want, got)

		})
	}
}

func Test_checkLicenseExpiry(t *testing.T) {

	tests := []struct {
		name          string
		licenseResult *LicenseResult
		wantErr       error
	}{
		{
			name: "Valid license",
			licenseResult: &LicenseResult{
				Result: LicenseStatus{
					LicenseType: "internal",
					LicenseId:   "test-id",
					ExpirationDate: ExpirationDate{
						Seconds: int64(time.Now().AddDate(0, 0, 1).Unix()),
					},
				},
			},
		}, {
			name: "Invalid license",
			licenseResult: &LicenseResult{
				Result: LicenseStatus{
					LicenseType: "internal",
					LicenseId:   "test-id",
					ExpirationDate: ExpirationDate{
						Seconds: int64(time.Now().AddDate(0, 0, -1).Unix()),
					},
				},
				ErrorType:        "license error",
				ErrorDescription: "license is invalid",
			},
			wantErr: errors.New("This license has expired"),
		}, {
			name: "Grace Period for commercial license",
			licenseResult: &LicenseResult{
				Result: LicenseStatus{
					LicenseType: "commercial",
					LicenseId:   "test-id",
					ExpirationDate: ExpirationDate{
						Seconds: int64(time.Now().AddDate(0, 0, -2).Unix()),
					},
				},
			},
		}, {
			name: "No License is applied",
			licenseResult: &LicenseResult{
				Result:           LicenseStatus{},
				ErrorType:        "",
				ErrorDescription: "",
			},
			wantErr: errors.New("Please apply a license"),
		}, {
			name: "Received Error from deployment service",
			licenseResult: &LicenseResult{
				Result:           LicenseStatus{},
				ErrorType:        "DeploymentServiceError",
				ErrorDescription: "Unable to connect to license service",
			},
			wantErr: errors.New("Unable to connect to license service"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := checkLicenseExpiry(tt.licenseResult)
			if tt.wantErr != nil {
				assert.Contains(t, err.Error(), tt.wantErr.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}
