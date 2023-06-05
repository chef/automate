package main

import (
	"context"
	"errors"
	"testing"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/infrastructure"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"
)

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
	i := &infrastructure.InfraFlow{
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
	i := &infrastructure.InfraFlow{
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
	i := &infrastructure.InfraFlow{
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
