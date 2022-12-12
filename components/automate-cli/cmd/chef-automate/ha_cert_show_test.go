package main

import (
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestIsCommonCerts(t *testing.T) {
	cs := NewCertShowImpl(certShowFlags{}, getMockNodeUtilsImpl(), getMockSSHUtilsImpl(), getMockWriterImpl())

	type testCaseInfo struct {
		testCaseDescription string
		input               []CertByIP
		expected            bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Common certs",
			input: []CertByIP{
				{
					IP:         ValidIP,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
				{
					IP:         ValidIP1,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
			},
			expected: true,
		},
		{
			testCaseDescription: "Uncommon certs",
			input: []CertByIP{
				{
					IP:         ValidIP,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
				{
					IP:         ValidIP1,
					PrivateKey: "private_key_2",
					PublicKey:  "public_key_2",
					NodesDn:    "",
				},
			},
			expected: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			err := cs.certShow(nil, nil)
			assert.NoError(t, err)

			actual := cs.isCommonCerts(tc.input)
			assert.Equal(t, tc.expected, actual)
		})
	}
}

func TestValidateNode(t *testing.T) {
	cs := NewCertShowImpl(certShowFlags{}, getMockNodeUtilsImpl(), getMockSSHUtilsImpl(), getMockWriterImpl())

	type testCaseInfo struct {
		testCaseDescription string
		inputIPs            []CertByIP
		inputNode           string
		inputRemoteService  string
		expectedError       bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Node is part of cluster",
			inputIPs: []CertByIP{
				{
					IP:         ValidIP,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
				{
					IP:         ValidIP1,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
			},
			inputNode:          ValidIP,
			inputRemoteService: "automate",
			expectedError:      false,
		},
		{
			testCaseDescription: "Node is not part of cluster",
			inputIPs: []CertByIP{
				{
					IP:         ValidIP,
					PrivateKey: "private_key_1",
					PublicKey:  "public_key_1",
					NodesDn:    "",
				},
				{
					IP:         ValidIP1,
					PrivateKey: "private_key_2",
					PublicKey:  "public_key_2",
					NodesDn:    "",
				},
			},
			inputNode:          ValidIP2,
			inputRemoteService: "opensearch",
			expectedError:      true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			cs.flags.node = tc.inputNode
			actualError := cs.validateNode(tc.inputIPs, tc.inputRemoteService)
			if tc.expectedError {
				assert.Error(t, actualError)
				assert.Contains(t, actualError.Error(), "does not exist in the")
			} else {
				assert.NoError(t, actualError)
			}
		})
	}
}

func getMockNodeUtilsImpl() *MockNodeUtilsImpl {
	return &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomteHAInfraDetails, *SSHConfig, error) {
			return nil, &SSHConfig{}, nil
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return AWS_MODE, nil
		},
		isA2HARBFileExistFunc: func() bool {
			return true
		},
		isManagedServicesOnFunc: func() bool {
			return true
		},
		pullAndUpdateConfigFunc: PullConfFunc,
	}
}

func getMockSSHUtilsImpl() *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return "", nil
		},
	}
}

func getMockWriterImpl() *cli.Writer {
	return majorupgrade_utils.NewCustomWriterWithInputs("x").CliWriter
}
