package main

import (
	"context"
	"fmt"
	"testing"

	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/mock"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/golang/mock/gomock"
	"google.golang.org/grpc"

	// components/automate-cli/cmd/chef-automate/mock/mock_deployclientstreamer.go

	"github.com/stretchr/testify/require"
)

func NewClientWithPara(dcs deployment.DeployClientStreamer, ca deployment.CertificateAuthorityServiceClient) *client.DSClient {
	// Create a new DSClient that embeds the mock implementations
	client := &client.DSClient{
		DeployClientStreamer:              dcs,
		CertificateAuthorityServiceClient: ca,
	}
	return client
}

func Test_runLicenseStatusCmd(t *testing.T) {
	fmt.Println("Test 1")
	// Create mock implementations of the embedded interfaces
	deployMock := &mock.MockDeployClientStreamer{}
	caMock := &mock.MockCertificateAuthorityServiceClient{}

	client := NewClientWithPara(deployMock, caMock)

	require.NotNil(t, client)
	require.NotEmpty(t, client)
	err := runLicenseStatusCmdImp(nil, nil, client)
	require.Error(t, err)
	fmt.Println("Errors: ", err)
	require.Equal(t, err.Error(), "This license has expired. Please contact sales@chef.io to renew your Chef Automate license.")

	fmt.Println("Test 2")
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)

	// First call to LicenseStatus should return an error.
	mockDeployClientStreamer.EXPECT().
		LicenseStatus(gomock.Any(), gomock.Any(), gomock.Any()).
		Times(1).
		Do(func(ctx context.Context, req *deployment.LicenseStatusRequest, opts ...grpc.CallOption) {
			mockDeployClientStreamer.SetReturnError(true)
		})

	// Second call to LicenseStatus should not return an error.
	mockDeployClientStreamer.EXPECT().
		LicenseStatus(gomock.Any(), gomock.Any(), gomock.Any()).
		Times(1).
		Do(func(ctx context.Context, req *deployment.LicenseStatusRequest, opts ...grpc.CallOption) {
			mockDeployClientStreamer.SetReturnError(false)
		})

	caaMock := &mock.MockCertificateAuthorityServiceClient{}
	connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

	err = runLicenseStatusCmdImp(nil, nil, connection)
	if err == nil {
		t.Errorf("Expected an error, but got nil")
	}

	err = runLicenseStatusCmdImp(nil, nil, connection)
	if err == nil {
		t.Errorf("Expected no error, but got %v", err)
	}
}
