package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"testing"

	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/mock"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/golang/mock/gomock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
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
	require.Error(t, err)

	err = runLicenseStatusCmdImp(nil, nil, connection)
	require.Error(t, err)
}

func TestMaybeFromFile(t *testing.T) {
	t.Run("Reading token data from file", func(t *testing.T) {
		// Create a temporary file and write some data to it
		content := "this is a test file"
		tmpfile, err := ioutil.TempFile("", "example")
		if err != nil {
			t.Fatal(err)
		}
		defer os.Remove(tmpfile.Name()) // clean up

		if _, err := tmpfile.Write([]byte(content)); err != nil {
			t.Fatal(err)
		}

		if err := tmpfile.Close(); err != nil {
			t.Fatal(err)
		}

		// Call the function with the file path
		result, err := maybeFromFile(tmpfile.Name())
		if err != nil {
			t.Errorf("maybeFromFile(%q) failed with error %q", tmpfile.Name(), err)
		}

		// Check if the returned result matches the expected output
		if result != content {
			t.Errorf("maybeFromFile(%q) = %q, want %q", tmpfile.Name(), result, content)
		}
	})

	t.Run("No such file appears on disk", func(t *testing.T) {
		// Call the function with a non-existent file path
		result, err := maybeFromFile("non-existent-file.txt")
		if err != nil {
			t.Errorf("maybeFromFile(%q) failed with error %q", "non-existent-file.txt", err)
		}

		// Check if the returned result matches the expected output
		if result != "non-existent-file.txt" {
			t.Errorf("maybeFromFile(%q) = %q, want %q", "non-existent-file.txt", result, "non-existent-file.txt")
		}
	})

	t.Run("Reading token data from file failed", func(t *testing.T) {
		// Call the function with a file path that exists but cannot be read
		result, err := maybeFromFile("/dev/null")

		fmt.Printf("*******: %v**", err)
		expectedErr := errors.New("Reading token data from file failed")
		if err.Error() != expectedErr.Error() {
			t.Errorf("maybeFromFile(%q) failed with error %q, want %q", "/dev/null", err, expectedErr)
		}

		// Check if the returned result matches the expected output
		if result != "" {
			t.Errorf("maybeFromFile(%q) = %q, want %q", "/dev/null", result, "")
		}
	})
}
