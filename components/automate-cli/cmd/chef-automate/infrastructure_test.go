package main

import (
	"context"
	"testing"

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

func Test_runpreInfrastructureCmd(t *testing.T) {
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
			if err == nil{
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
