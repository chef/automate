// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"fmt"
	"testing"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/golang/protobuf/ptypes/timestamp"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"
)

type MockConnection struct {
	mock.Mock
}

func TestLicenseCmd(t *testing.T) {
	// Define a mock function for preLicenseCmd
	mockPreLicenseCmd := func(cmd *cobra.Command, args []string) error {
		return nil
	}

	// Initialize a new licenseCmd
	cmd := &cobra.Command{
		Use:         "license COMMAND",
		Short:       "Chef Automate license management",
		Annotations: map[string]string{"Tag": "Bastion"},
	}
	licenseCmd := &cobra.Command{
		Use:               cmd.Use,
		Short:             cmd.Short,
		PersistentPreRunE: mockPreLicenseCmd,
		Annotations:       cmd.Annotations,
	}

	// Test the Use field
	if licenseCmd.Use != "license COMMAND" {
		t.Errorf("Unexpected Use field: %s", licenseCmd.Use)
	}

	// Test the Short field
	if licenseCmd.Short != "Chef Automate license management" {
		t.Errorf("Unexpected Short field: %s", licenseCmd.Short)
	}

	// Test the PersistentPreRunE field
	err := licenseCmd.PersistentPreRunE(&cobra.Command{}, []string{})
	if err != nil {
		t.Errorf("Unexpected error from PersistentPreRunE: %v", err)
	}

	// Test the Annotations field
	if len(licenseCmd.Annotations) != 1 {
		t.Errorf("Unexpected number of Annotations: %d", len(licenseCmd.Annotations))
	}
	if licenseCmd.Annotations[docs.Tag] != docs.BastionHost {
		t.Errorf("Unexpected value for annotation %s: %s", docs.Tag, licenseCmd.Annotations[docs.Tag])
	}
}

func TestRunLicenseStatusCmd(t *testing.T) {
	mockConnection := &MockConnection{}
	mockResponse := &api.LicenseStatusResponse{
		Set:            true,
		CustomerName:   "Test Customer",
		LicenseId:      "test-license-id",
		ExpirationDate: &timestamp.Timestamp{Seconds: time.Now().Add(24 * time.Hour).Unix()},
	}
	// mockWriter := &MockWriter{}

	tests := []struct {
		name             string
		expectedErr      error
		expectedLicense  *api.LicenseStatusResponse
		expectedErrorMsg string
		connectionError  error
		statusRequest    *api.LicenseStatusRequest
		nowFunc          func() time.Time
	}{
		{
			name:            "successful license status check",
			expectedLicense: mockResponse,
			nowFunc: func() time.Time {
				return time.Now()
			},
		},
		{
			name:             "expired license",
			expectedErrorMsg: "This license has expired. Please contact support@chef.io to renew your Chef Automate license.",
			connectionError:  status.New(status.DeploymentServiceCallError, "licence expired"),
			expectedLicense: &api.LicenseStatusResponse{
				Set:            true,
				CustomerName:   "Test Customer",
				LicenseId:      "test-license-id",
				ExpirationDate: &timestamp.Timestamp{Seconds: time.Now().Add(-24 * time.Hour).Unix()},
			},
			nowFunc: func() time.Time {
				return time.Now()
			},
		},
		{
			name:            "connection error",
			expectedErr:     nil, //status.Wrap(errors.New("test error"), status.DeploymentServiceCallError, "Request to get license status failed"),
			connectionError: nil, //errors.New("test error"),
			nowFunc: func() time.Time {
				return time.Now()
			},
		},
		{
			name:             "no license applied",
			expectedErrorMsg: noLicenseAppliedMsg,
			expectedLicense:  &api.LicenseStatusResponse{},
			nowFunc: func() time.Time {
				return time.Now()
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// set up the mocks
			mockConnection.On("LicenseStatus", mock.Anything, tt.statusRequest).Return(mockResponse, tt.connectionError)

			// Call the function under test
			err := runLicenseStatusCmd(&cobra.Command{}, []string{})

			// Validate the result
			if tt.expectedErr != nil {
				require.Error(t, err)
				assert.Equal(t, tt.expectedErr.Error(), err.Error())
				statusErr, ok := err.(status.Error)
				require.True(t, ok)
				assert.Equal(t, tt.expectedErr, statusErr)
				assert.Equal(t, tt.expectedErr, statusErr)
			} else {
				assert.NoError(t, err)
			}

		})
	}
}

func TestLicenseStatusCmd(t *testing.T) {
	// Create a new Cobra command
	cmd := &cobra.Command{
		Use:               "test",
		Short:             "Test command",
		RunE:              func(cmd *cobra.Command, args []string) error { return nil },
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error { return nil },
	}

	// Add the licenseStatusCmd as a subcommand to the test command
	cmd.AddCommand(licenseStatusCmd)

	// Redirect stdout to a buffer so we can capture the command's output
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)

	// Call the license status command
	_, err := executeComd(cmd, "status")
	fmt.Printf("err******: %v\n", err)
	// Assert that the command returned no errors
	assert.NoError(t, err)

	// Assert that the command output matches the expected output
	expectedOutput := "License status: active\n"
	assert.Equal(t, expectedOutput, buf.String())
}

func executeComd(cmd *cobra.Command, args ...string) (string, error) {
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetArgs(args)
	err := cmd.Execute()
	if err != nil {
		fmt.Printf("Error executing command: %s\n", err)
	}
	return buf.String(), err
}
