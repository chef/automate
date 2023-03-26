package main

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/mock"
	"github.com/chef/automate/components/automate-deployment/pkg/client"

	// components/automate-cli/cmd/chef-automate/mock/mock_deployclientstreamer.go

	"github.com/stretchr/testify/require"
)

func NewClient(timeout time.Duration) *client.DSClient {
	// Create mock implementations of the embedded interfaces
	deployMock := &mock.MockDeployClientStreamer{}
	caMock := &mock.MockCertificateAuthorityServiceClient{}

	// Create a new DSClient that embeds the mock implementations
	client := &client.DSClient{
		DeployClientStreamer:              deployMock,
		CertificateAuthorityServiceClient: caMock,
	}
	return client
}

func TestrunLicenseStatusCmd(t *testing.T) {
	fmt.Println("Test 1")
	client := NewClient(client.DefaultClientTimeout)

	require.NotNil(t, client)
	require.NotEmpty(t, client)
	err := runLicenseStatusCmd(nil, nil, client)
	require.NoError(t, err)

	// fmt.Println("Test 2")
	// client, err = NewClient(time.Second * 0)
	// require.Error(t, err)
	// err = runLicenseStatusCmd(nil, nil, client)
	// require.NoError(t, err)

}
