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

func Test_runLicenseStatusCmd(t *testing.T) {
	fmt.Println("Test 1")
	client := NewClient(client.DefaultClientTimeout)

	require.NotNil(t, client)
	require.NotEmpty(t, client)
	err := runLicenseStatusCmd(nil, nil, client)
	require.Error(t, err)
	fmt.Printf("** ERROR: %+v\n", err)
	// require.NoError(t, err)

}
