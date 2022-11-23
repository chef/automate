package main

import (
	"errors"
	"os"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
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

	t.Run("Valid remote path", func(t *testing.T) {
		input := RemoteFilePath
		res := c.IsRemotePath(input)
		assert.True(t, res)
	})

	t.Run("Local path instead of remote path", func(t *testing.T) {
		input := LocalFilePath
		res := c.IsRemotePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid remote path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem198.51.100.0"
		res := c.IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 2", func(t *testing.T) {
		input := "198.51.100.0/home/ec2-user/certs/public.pem"
		res := c.IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 3", func(t *testing.T) {
		input := "\n   198.51.100.0:/home/ec2-user/certs/public.pem"
		res := c.IsRemotePath(input)
		assert.False(t, res)
	})
}

func TestGetIPV4(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	t.Run("Valid IP V4", func(t *testing.T) {
		input := RemoteFilePath
		res := c.GetIPV4(input)
		expected := ValidIP
		assert.Equal(t, expected, res)
	})

	t.Run("Valid IP V4 but invalid remote path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem:127.0.0.1"
		res := c.GetIPV4(input)
		expected := "127.0.0.1"
		assert.Equal(t, expected, res)
	})

	t.Run("Valid IP V4 but invalid remote path 2", func(t *testing.T) {
		input := "/home/ec2-user/:0.0.0.0:/certs/public.pem"
		res := c.GetIPV4(input)
		expected := "0.0.0.0"
		assert.Equal(t, expected, res)
	})

	t.Run("Invalid IP v4 and valid path", func(t *testing.T) {
		input := "256.256.256.256:/home/ec2-user/certs/public.pem"
		res := c.GetIPV4(input)
		expected := ""
		assert.Equal(t, expected, res)
	})

	t.Run("Invalid IP v4 and invalid path", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem:1.2.3"
		res := c.GetIPV4(input)
		expected := ""
		assert.Equal(t, expected, res)
	})
}

func TestGetRemoteFileDetails(t *testing.T) {
	c := certRotateFlow{FileUtils: mockFS()}

	t.Run("Valid Remote Path", func(t *testing.T) {
		input := RemoteFilePath
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := LocalFilePath
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := "public.pem"
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Local path", func(t *testing.T) {
		input := LocalFilePath
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, " is not a valid IPv4 address", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Colon missing", func(t *testing.T) {
		input := "198.51.100.0/home/ec2-user/certs/public.pem"
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote path: 198.51.100.0/home/ec2-user/certs/public.pem", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - No filename", func(t *testing.T) {
		input := "198.51.100.0:/home/ec2-user/certs/public/"
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := "/home/ec2-user/certs/public"
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := "public"
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Reverse", func(t *testing.T) {
		input := "/home/ec2-user/certs/public/:198.51.100.0"
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := ValidIP
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ValidIP
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path (empty path)", func(t *testing.T) {
		input := ValidIP + ":"
		remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote path: 198.51.100.0:", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.NotEqual(t, hostIPExp, hostIPRes)
	})

}

func TestGetCerts(t *testing.T) {

	c := certRotateFlow{FileUtils: mockFS()}

	oldCertFlags := certFlagsObj
	oldSshFlag := sshFlagObj
	oldNodeFlag := nodeFlagObj

	defer func() {
		certFlagsObj = oldCertFlags
		sshFlagObj = oldSshFlag
		nodeFlagObj = oldNodeFlag
	}()

	var infra *AutomteHAInfraDetails = &AutomteHAInfraDetails{}

	t.Run("All paths empty and flag for automate service", func(t *testing.T) {
		certFlagsObj = certFlags{}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("All paths given and flag is automate service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      ValidCertPath,
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.NoError(t, err)
		assert.Equal(t, FileContent, rootCARes)
		assert.Equal(t, FileContent, publicCertRes)
		assert.Equal(t, FileContent, privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("some invalid paths given and flag is automate service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  "./xyx-cert.go",
			rootCA:      ValidCertPath,
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("All paths given but invalid (file not exist in (f.s)and flag is automate service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: "./xyz.go",
			publicCert:  "./xyz.go",
			rootCA:      "./xyx.go",
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("All paths given except root-ca and flag is automate service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, _, _, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
	})

	t.Run("All paths given but root-ca path is invalid(file not exist) flag is automate service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      "./xyx-cert.go",
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, _, _, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
	})

	t.Run("All paths given except root-ca flag is automate service and node flag given", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
		}
		sshFlagObj = sshFlag{
			automate: true,
		}
		nodeFlagObj = nodeFlag{
			node: "ip-given",
		}
		//root-ca ignored
		rootCARes, publicCertRes, privateCertRes, _, _, err := c.getCerts(infra)
		assert.NoError(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, FileContent, publicCertRes)
		assert.Equal(t, FileContent, privateCertRes)
	})

	t.Run("All paths given and flag is opensearch service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      ValidCertPath,
			adminCert:   ValidCertPath,
			adminKey:    ValidCertPath,
		}
		sshFlagObj = sshFlag{
			opensearch: true,
		}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.NoError(t, err)
		assert.Equal(t, FileContent, rootCARes)
		assert.Equal(t, FileContent, publicCertRes)
		assert.Equal(t, FileContent, privateCertRes)
		assert.Equal(t, FileContent, adminCertRes)
		assert.Equal(t, FileContent, adminKeyRes)
	})

	t.Run("Some mandatory path not and flag is opensearch service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      ValidCertPath,
			adminCert:   ValidCertPath,
			adminKey:    "",
		}
		sshFlagObj = sshFlag{
			//services
			opensearch: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("invalid adminCert path and flag is opensearch service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      ValidCertPath,
			adminCert:   "./xyz-cert.go",
			adminKey:    ValidCertPath,
		}
		sshFlagObj = sshFlag{
			//services
			opensearch: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})

	t.Run("invalid adminKey path and flag is opensearch service", func(t *testing.T) {
		certFlagsObj = certFlags{
			privateCert: ValidCertPath,
			publicCert:  ValidCertPath,
			rootCA:      ValidCertPath,
			adminCert:   ValidCertPath,
			adminKey:    "./xyz-cert.go",
		}
		sshFlagObj = sshFlag{
			//services
			opensearch: true,
		}
		nodeFlagObj = nodeFlag{}
		rootCARes, publicCertRes, privateCertRes, adminCertRes, adminKeyRes, err := c.getCerts(infra)
		assert.Error(t, err)
		assert.Equal(t, "", rootCARes)
		assert.Equal(t, "", publicCertRes)
		assert.Equal(t, "", privateCertRes)
		assert.Equal(t, "", adminCertRes)
		assert.Equal(t, "", adminKeyRes)
	})
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
