package main

import (
	"os"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	RemoteFilePath = "198.51.100.0:/home/ec2-user/certs/public.pem"
	LocalFilePath  = "/home/ec2-user/certs/public.pem"
	ValidIP        = "198.51.100.0"
	FileContent    = "File exist and readed successfully"
	ValidCertPath  = "./certRotate.go"
)

func TestIsRemotePath(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription string
		input               string
		expected            bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Valid remote path",
			input:               RemoteFilePath,
			expected:            true,
		},
		{
			testCaseDescription: "Local path instead of remote path",
			input:               LocalFilePath,
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 1",
			input:               "/home/ec2-user/certs/public.pem198.51.100.0",
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 2",
			input:               "198.51.100.0/home/ec2-user/certs/public.pem",
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 3",
			input:               "\n   198.51.100.0:/home/ec2-user/certs/public.pem",
			expected:            false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			actual := c.IsRemotePath(tc.input)
			assert.Equal(t, tc.expected, actual)
		})
	}
}

func TestGetIPV4(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription string
		input               string
		expected            string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Valid IP V4",
			input:               RemoteFilePath,
			expected:            ValidIP,
		},
		{
			testCaseDescription: "Valid IP V4 but invalid remote path 1",
			input:               "/home/ec2-user/certs/public.pem:127.0.0.1",
			expected:            "127.0.0.1",
		},
		{
			testCaseDescription: "Valid IP V4 but invalid remote path 2",
			input:               "/home/ec2-user/:0.0.0.0:/certs/public.pem",
			expected:            "0.0.0.0",
		},
		{
			testCaseDescription: "Invalid IP v4 and valid path",
			input:               "256.256.256.256:/home/ec2-user/certs/public.pem",
			expected:            "",
		},
		{
			testCaseDescription: "Invalid IP v4 and invalid path",
			input:               "/home/ec2-user/certs/public.pem:1.2.3",
			expected:            "",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			actual := c.GetIPV4(tc.input)
			assert.Equal(t, tc.expected, actual)
		})
	}
}

func TestGetRemoteFileDetails(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription    string
		input                  string
		isError                bool
		expectedErrorMessage   string
		expectedRemoteFilePath string
		expectedFileName       string
		expectedHostIP         string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription:    "Valid remote path",
			input:                  RemoteFilePath,
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: LocalFilePath,
			expectedFileName:       "public.pem",
			expectedHostIP:         ValidIP,
		},
		{
			testCaseDescription:    "Invalid Remote Path - Local path",
			input:                  LocalFilePath,
			isError:                true,
			expectedErrorMessage:   " is not a valid IPv4 address",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
		},
		{
			testCaseDescription:    "Invalid Remote Path - Colon missing",
			input:                  "198.51.100.0/home/ec2-user/certs/public.pem",
			isError:                true,
			expectedErrorMessage:   "Invalid remote path: 198.51.100.0/home/ec2-user/certs/public.pem",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
		},
		{
			testCaseDescription:    "Invalid Remote Path - No filename",
			input:                  "198.51.100.0:/home/ec2-user/certs/public/",
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: "/home/ec2-user/certs/public",
			expectedFileName:       "public",
			expectedHostIP:         ValidIP,
		},
		{
			testCaseDescription:    "Invalid Remote Path - Reverse",
			input:                  "/home/ec2-user/certs/public/:198.51.100.0",
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: ValidIP,
			expectedFileName:       ValidIP,
			expectedHostIP:         ValidIP,
		},
		{
			testCaseDescription:    "Invalid Remote Path - Empty Path",
			input:                  ValidIP + ":",
			isError:                true,
			expectedErrorMessage:   "Invalid remote path: " + ValidIP + ":",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(tc.input)
			if tc.isError {
				assert.Error(t, err)
				assert.Equal(t, tc.expectedErrorMessage, err.Error())
			} else {
				assert.NoError(t, err)
			}
			assert.Equal(t, tc.expectedRemoteFilePath, remoteFilePathRes)
			assert.Equal(t, tc.expectedFileName, fileNameRes)
			assert.Equal(t, tc.expectedHostIP, hostIPRes)
		})
	}
}

func TestGetCerts(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	var infra *AutomteHAInfraDetails = &AutomteHAInfraDetails{}

	type testCaseInfo struct {
		testCaseDescription string
		flagsObj            flags
		rootCaWant          string
		publicCertWant      string
		privateCertWant     string
		adminCertWant       string
		adminKeyWant        string
		isError             bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "All paths given and flag is automate service",
			flagsObj: flags{
				automate:    true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      ValidCertPath,
			},
			rootCaWant:      FileContent,
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         false,
		},
		{
			testCaseDescription: "All paths given except root-ca flag is automate service and node flag given",
			flagsObj: flags{
				automate:    true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				node:        "ip-given",
			},
			rootCaWant:      "",
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         false,
		},
		{
			testCaseDescription: "All paths given and flag is opensearch service",
			flagsObj: flags{
				opensearch:  true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      ValidCertPath,
				adminCert:   ValidCertPath,
				adminKey:    ValidCertPath,
			},
			rootCaWant:      FileContent,
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   FileContent,
			adminKeyWant:    FileContent,
			isError:         false,
		},
		{
			testCaseDescription: "All paths empty and flag for automate service",
			flagsObj:            flags{},
			rootCaWant:          "",
			publicCertWant:      "",
			privateCertWant:     "",
			adminCertWant:       "",
			adminKeyWant:        "",
			isError:             true,
		},
		{
			testCaseDescription: "some invalid paths given and flag is automate service",
			flagsObj: flags{
				automate:    true,
				privateCert: ValidCertPath,
				publicCert:  "./xyx-cert.go",
				rootCA:      ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "All paths given but invalid (file not exist in (f.s)and flag is automate service",
			flagsObj: flags{
				automate:    true,
				privateCert: "./xyz.go",
				publicCert:  "./xyz.go",
				rootCA:      "./xyx.go",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "All paths given except root-ca and flag is automate service",
			flagsObj: flags{
				automate:    true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "All paths given but root-ca path is invalid(file not exist) flag is automate service",
			flagsObj: flags{
				automate:    true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      "./xyx-cert.go",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "Some mandatory path not given and flag is opensearch service",
			flagsObj: flags{
				opensearch:  true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      ValidCertPath,
				adminCert:   ValidCertPath,
				adminKey:    "",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "Invalid adminCert path and flag is opensearch service",
			flagsObj: flags{
				opensearch:  true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      ValidCertPath,
				adminCert:   "./xyz-cert.go",
				adminKey:    ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "Invalid adminKey path and flag is opensearch service",
			flagsObj: flags{
				opensearch:  true,
				privateCert: ValidCertPath,
				publicCert:  ValidCertPath,
				rootCA:      ValidCertPath,
				adminCert:   ValidCertPath,
				adminKey:    "./xyz-cert.go",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			rootCaGot, publicCertGot, privateCertGot, adminCertGot, adminKeyGot, err := c.getCerts(infra, &tc.flagsObj)
			if tc.isError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			assert.Equal(t, tc.rootCaWant, rootCaGot)
			assert.Equal(t, tc.publicCertWant, publicCertGot)
			assert.Equal(t, tc.privateCertWant, privateCertGot)
			assert.Equal(t, tc.adminCertWant, adminCertGot)
			assert.Equal(t, tc.adminKeyWant, adminKeyGot)
		})
	}
}

func mockFS() *fileutils.MockFileSystemUtils {
	return &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filename string) ([]byte, error) {
			if _, err := os.Stat(filename); err == nil {
				// path/to/whatever exists
				return []byte(FileContent), nil
			} else if errors.Is(err, os.ErrNotExist) {
				// path/to/whatever does *not* exist
				return []byte{}, err
			} else {
				return []byte{}, err
			}
		},
	}
}
