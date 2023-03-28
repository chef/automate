package main

import (
	"io/ioutil"
	"os"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/mock"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/golang/mock/gomock"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
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
	t.Run("Test 1. PASSED", func(t *testing.T) {
		deployMock := &mock.MockDeployClientStreamer{}
		caMock := &mock.MockCertificateAuthorityServiceClient{}

		client := NewClientWithPara(deployMock, caMock)

		require.NotNil(t, client)
		require.NotEmpty(t, client)
		err := runLicenseStatusCmdImp(nil, nil, client)
		require.NoError(t, err)
	})

	t.Run("Test 2, failed", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()
		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)

		mockDeployClientStreamer.EXPECT().
			LicenseStatus(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseStatusCmdImp(nil, nil, connection)
		require.Error(t, err)
		require.Equal(t, err.Error(), "Request to get license status failed: error occurred!")
	})

	t.Run("Test 2, expired license", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()
		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)

		mockDeployClientStreamer.EXPECT().
			LicenseStatus(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		mockDeployClientStreamer.SetReturnError(true, errors.New("expired license"))

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseStatusCmdImp(nil, nil, connection)
		require.Error(t, err)
		require.Equal(t, err.Error(), "This license has expired. Please contact sales@chef.io to renew your Chef Automate license.")
	})

}

func TestMaybeFromFile(t *testing.T) {
	t.Run("Reading token data from file", func(t *testing.T) {
		// Create a temporary file and write some data to it
		content := "this is a test file"
		tmpfile, err := ioutil.TempFile("", "example")
		require.NoError(t, err)

		defer os.Remove(tmpfile.Name()) // clean up

		n, err := tmpfile.Write([]byte(content))
		require.NoError(t, err)
		require.NotZero(t, n)

		err = tmpfile.Close()
		require.NoError(t, err)

		// Call the function with the file path
		result, err := maybeFromFile(tmpfile.Name())
		require.NoError(t, err)
		require.Equal(t, content, result)
	})

	t.Run("No such file appears on disk", func(t *testing.T) {
		result, err := maybeFromFile("non-existent-file.txt")
		if err != nil {
			t.Errorf("maybeFromFile(%q) failed with error %q", "non-existent-file.txt", err)
		}

		// Check if the returned result matches the expected output
		if result != "non-existent-file.txt" {
			t.Errorf("maybeFromFile(%q) = %q, want %q", "non-existent-file.txt", result, "non-existent-file.txt")
		}
	})

	// t.Run("Reading token data from file failed", func(t *testing.T) {
	// 	// Call the function with a file path that exists but cannot be read
	// 	result, err := maybeFromFile("/dev/null")

	// 	fmt.Printf("*******: %v**", err)
	// 	expectedErr := errors.New("Reading token data from file failed")
	// 	if err.Error() != expectedErr.Error() {
	// 		t.Errorf("maybeFromFile(%q) failed with error %q, want %q", "/dev/null", err, expectedErr)
	// 	}

	// 	// Check if the returned result matches the expected output
	// 	if result != "" {
	// 		t.Errorf("maybeFromFile(%q) = %q, want %q", "/dev/null", result, "")
	// 	}
	// })
}

func Test_getConfigMgmtUsageNodesImp(t *testing.T) {
	t.Run("Test 2, pass scenario", func(t *testing.T) {
		// Create mock implementations of the embedded interfaces
		deployMock := &mock.MockDeployClientStreamer{}
		caMock := &mock.MockCertificateAuthorityServiceClient{}

		client := NewClientWithPara(deployMock, caMock)
		require.NotNil(t, client)
		require.NotEmpty(t, client)

		hourAgo := &tspb.Timestamp{Seconds: time.Now().Unix() - 3600}

		depNU, err := getConfigMgmtUsageNodesImp(hourAgo, client)
		require.NoError(t, err)
		require.NotEmpty(t, depNU)
	})

	t.Run("Test 2, failed scenario", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()
		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)
		mockDeployClientStreamer.SetReturnError(true, nil)
		mockDeployClientStreamer.EXPECT().
			Usage(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		depNU, err := getConfigMgmtUsageNodesImp(&tspb.Timestamp{Seconds: time.Now().Unix() - 3600}, connection)
		require.Error(t, err)
		require.Nil(t, depNU)
	})

}

func Test_runLicenseApplyCmdImp(t *testing.T) {
	t.Run("Test 1. pass scenario", func(t *testing.T) {
		mockDeployClientStreamer := &mock.MockDeployClientStreamer{}

		caaMock := &mock.MockCertificateAuthorityServiceClient{}

		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseApplyCmdImp(nil, []string{"file1.txt"}, connection)
		require.NoError(t, err)
	})

	t.Run("Test 2. Request to apply license failed", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()

		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)
		mockDeployClientStreamer.SetReturnError(true, nil)

		mockDeployClientStreamer.EXPECT().
			LicenseApply(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseApplyCmdImp(nil, []string{"file1.txt"}, connection)
		require.Error(t, err)
		require.Equal(t, err.Error(), "Request to apply license failed: error occurred!")
	})

	t.Run("Test 3. The license does not appear to be complete", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()

		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)
		mockDeployClientStreamer.SetReturnError(true, errors.New("data_loss"))

		mockDeployClientStreamer.EXPECT().
			LicenseApply(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseApplyCmdImp(nil, []string{"file1.txt"}, connection)
		require.Error(t, err)
		require.Equal(t, err.Error(), "The license does not appear to be complete. Please check and try again.: rpc error: code = DataLoss desc = Data loss error occurred")
	})

	t.Run("Test 3. The license cannot be verified by Chef Automate", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()

		mockDeployClientStreamer := mock.NewMockDeployClientStreamer(ctrl)
		mockDeployClientStreamer.SetReturnError(true, errors.New("Unauthenticated"))

		mockDeployClientStreamer.EXPECT().
			LicenseApply(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("new error for a failed scenario")).AnyTimes()

		caaMock := &mock.MockCertificateAuthorityServiceClient{}
		connection := NewClientWithPara(mockDeployClientStreamer, caaMock)

		err := runLicenseApplyCmdImp(nil, []string{"file1.txt"}, connection)
		require.Error(t, err)
		require.Equal(t, err.Error(), "The license cannot be verified by Chef Automate. Please contact support@chef.io for assistance.: rpc error: code = Unauthenticated desc = Unauthenticated error occurred")

	})
}
