package genconfig

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/ioutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/stretchr/testify/assert"
)

func TestPromptsOnPremManaged(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	sshUser := "ec2-user"
	input(b, sshUser+"\r")

	sshGroup := "ec2-user"
	input(b, "\r")

	sshPort := 22
	input(b, "\r")

	sshKeyFilePath := "test.pem"
	sshKeyFileContent := "ssh user private key"
	input(b, sshKeyFilePath+"\r")

	enableCustomCerts := false
	input(b, "\r")

	automateFqdn := "A2-automate.amazonaws.com"
	input(b, automateFqdn+"\r")

	automateFqdnRootCaFilePath := "automateFqdnRootCa.pem"
	automateFqdnRootCaFileContent := "Automate Root CA"
	input(b, automateFqdnRootCaFilePath+"\r")

	automateAdminPass := "admin12345"
	input(b, automateAdminPass+"\r")

	automateNodeCount := 1
	input(b, fmt.Sprint(automateNodeCount)+"\r")

	automateIp1 := "172.168.192.1"
	input(b, automateIp1+"\r")

	chefInfraServerFqdn := "chef-infra-server.amazonaws.com"
	input(b, chefInfraServerFqdn+"\r")

	chefInfraServerFqdnRootCaFilePath := "chefInfraServerFqdnRootCa.pem"
	chefInfraServerFqdnRootCaFileContent := "Chef Infra Server Root CA"
	input(b, chefInfraServerFqdnRootCaFilePath+"\r")

	chefInfraServerNodeCount := 1
	input(b, fmt.Sprint(chefInfraServerNodeCount)+"\r")

	chefInfraServerIp1 := "172.168.192.4"
	input(b, chefInfraServerIp1+"\r")

	useExternalServices := true
	input(b, "\r")

	useAwsDatabases := true
	input(b, "\r")

	osDomainName := "opensearch-automate"
	input(b, osDomainName+"\r")

	osDomainUrl := "opensearch.automate.com"
	input(b, osDomainUrl+"\r")

	osUser := "admin"
	input(b, osUser+"\r")

	osPass := "admin@1234"
	input(b, osPass+"\r")

	osDefaultCerts := true
	input(b, "\r")

	osSnapshotRoleArn := "arn-os-snapshot-role"
	input(b, osSnapshotRoleArn+"\r")

	osSnapshotUserAccessKeyId := "ASKEIDSKFLDKDFJERKDS"
	input(b, osSnapshotUserAccessKeyId+"\r")

	osSnapshotUserAccessKeySecret := "asdfelkjsldfklsdjflsdfjdklsfjsdfsldkfjsd"
	input(b, osSnapshotUserAccessKeySecret+"\r")

	pgUrl := "pg.automate.com:5432"
	input(b, pgUrl+"\r")

	pgSuperUserName := "root"
	input(b, pgSuperUserName+"\r")

	pgSuperUserPass := "root1234"
	input(b, pgSuperUserPass+"\r")

	pgDbUserName := "dbUser"
	input(b, pgDbUserName+"\r")

	pgDbUserPass := "dbUser1234"
	input(b, pgDbUserPass+"\r")

	pgDefaultCerts := true
	input(b, "\r")

	enableBackup := true
	input(b, "\r")

	useAwsS3 := true
	input(b, "\r")

	awsBackupBucketName := "automate-backup"
	input(b, awsBackupBucketName+"\r")

	awsBackupAcessKeyId := "AKEKFKEIDKETODDFEDFE"
	input(b, awsBackupAcessKeyId+"\r")

	awsBackupAcessKeySecret := "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"
	input(b, awsBackupAcessKeySecret+"\r")

	awsRegion := "ap-south-2"
	input(b, "asia"+moveDown+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == sshKeyFilePath {
				data = []byte(sshKeyFileContent)
			} else if filepath == automateFqdnRootCaFilePath {
				data = []byte(automateFqdnRootCaFileContent)
			} else if filepath == chefInfraServerFqdnRootCaFilePath {
				data = []byte(chefInfraServerFqdnRootCaFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == sshKeyFilePath ||
				name == automateFqdnRootCaFilePath ||
				name == chefInfraServerFqdnRootCaFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.NoError(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "existing_nodes"
	workspacePath := "/hab/a2_deploy_workspace"
	automateConfigFile := "configs/automate.toml"
	pgInstanceCount := 0
	osInstanceCount := 0
	s3Endpoint := "https://s3.amazonaws.com"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.ExistingInfra.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.ExistingInfra.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.ExistingInfra.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.ExistingInfra.WorkspacePath)

	assert.Equal(t, sshUser, c.Config.Architecture.ExistingInfra.SSHUser)
	assert.Equal(t, sshGroup, c.Config.Architecture.ExistingInfra.SSHGroupName)
	assert.Equal(t, fmt.Sprint(sshPort), c.Config.Architecture.ExistingInfra.SSHPort)
	assert.Equal(t, sshKeyFilePath, c.Config.Architecture.ExistingInfra.SSHKeyFile)

	assert.Equal(t, enableCustomCerts, c.HasCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Automate.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Opensearch.Config.EnableCustomCerts)

	assert.Equal(t, automateFqdn, c.Config.Automate.Config.Fqdn)
	assert.Equal(t, automateFqdnRootCaFileContent, c.Config.Automate.Config.FqdnRootCA)
	assert.Equal(t, automateConfigFile, c.Config.Automate.Config.ConfigFile)
	assert.Equal(t, automateAdminPass, c.Config.Automate.Config.AdminPassword)
	assert.Equal(t, fmt.Sprint(automateNodeCount), c.Config.Automate.Config.InstanceCount)
	assert.Equal(t, automateNodeCount, len(c.Config.ExistingInfra.Config.AutomatePrivateIps))
	assert.Equal(t, automateIp1, c.Config.ExistingInfra.Config.AutomatePrivateIps[0])

	assert.Equal(t, chefInfraServerFqdn, c.Config.ChefServer.Config.ChefServerFqdn)
	assert.Equal(t, chefInfraServerFqdnRootCaFileContent, c.Config.ChefServer.Config.FqdnRootCA)
	assert.Equal(t, fmt.Sprint(chefInfraServerNodeCount), c.Config.ChefServer.Config.InstanceCount)
	assert.Equal(t, chefInfraServerNodeCount, len(c.Config.ExistingInfra.Config.ChefServerPrivateIps))
	assert.Equal(t, chefInfraServerIp1, c.Config.ExistingInfra.Config.ChefServerPrivateIps[0])

	assert.Equal(t, useExternalServices, c.Config.External.Database != nil)
	assert.Equal(t, useAwsDatabases, c.Config.External.Database.Type == "aws")

	assert.Equal(t, osDomainName, c.Config.External.Database.OpenSearch.OpensearchDomainName)
	assert.Equal(t, osDomainUrl, c.Config.External.Database.OpenSearch.OpensearchDomainURL)
	assert.Equal(t, osUser, c.Config.External.Database.OpenSearch.OpensearchUsername)
	assert.Equal(t, osPass, c.Config.External.Database.OpenSearch.OpensearchUserPassword)
	assert.Equal(t, osDefaultCerts, len(c.Config.External.Database.OpenSearch.OpensearchRootCert) == 0)
	assert.Equal(t, osSnapshotRoleArn, c.Config.External.Database.OpenSearch.Aws.AwsOsSnapshotRoleArn)
	assert.Equal(t, osSnapshotUserAccessKeyId, c.Config.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeyID)
	assert.Equal(t, osSnapshotUserAccessKeySecret, c.Config.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeySecret)

	assert.Equal(t, pgUrl, c.Config.External.Database.PostgreSQL.InstanceURL)
	assert.Equal(t, pgSuperUserName, c.Config.External.Database.PostgreSQL.SuperuserUsername)
	assert.Equal(t, pgSuperUserPass, c.Config.External.Database.PostgreSQL.SuperuserPassword)
	assert.Equal(t, pgDbUserName, c.Config.External.Database.PostgreSQL.DbuserUsername)
	assert.Equal(t, pgDbUserPass, c.Config.External.Database.PostgreSQL.DbuserPassword)
	assert.Equal(t, pgDefaultCerts, len(c.Config.External.Database.PostgreSQL.PostgresqlRootCert) == 0)

	assert.Equal(t, fmt.Sprint(osInstanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, fmt.Sprint(pgInstanceCount), c.Config.Postgresql.Config.InstanceCount)

	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupMount == "/mnt/automate_backups")
	assert.Equal(t, enableBackup, len(c.Config.Architecture.ExistingInfra.BackupConfig) > 0)
	assert.Equal(t, useAwsS3, c.Config.Architecture.ExistingInfra.BackupConfig == "object_storage")
	assert.Equal(t, useAwsS3, len(c.Config.ObjectStorage.Config.BucketName) > 0)
	assert.Equal(t, awsBackupBucketName, c.Config.ObjectStorage.Config.BucketName)
	assert.Equal(t, awsBackupAcessKeyId, c.Config.ObjectStorage.Config.AccessKey)
	assert.Equal(t, awsBackupAcessKeySecret, c.Config.ObjectStorage.Config.SecretKey)
	assert.Equal(t, s3Endpoint, c.Config.ObjectStorage.Config.Endpoint)
	assert.Equal(t, awsRegion, c.Config.ObjectStorage.Config.Region)
}

func TestOnPremPromptOpenSearchNodesCustomCertPerNode(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := true
	input(b, moveDown+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	adminCertFilePath := "admin.pem"
	adminCertFileContent := "ADMIN CERT"
	input(b, adminCertFilePath+"\r")

	adminKeyFilePath := "admin.key"
	adminKeyFileContent := "ADMIN KEY"
	input(b, adminKeyFilePath+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	priKeyFilePath2 := "test2.key"
	priKeyFileContent2 := "PRIVATE KEY2"
	input(b, priKeyFilePath2+"\r")

	pubKeyFilePath2 := "test2.pem"
	pubKeyFileContent2 := "PUBLIC KEY2"
	input(b, pubKeyFilePath2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	priKeyFilePath3 := "test3.key"
	priKeyFileContent3 := "PRIVATE KEY3"
	input(b, priKeyFilePath3+"\r")

	pubKeyFilePath3 := "test3.pem"
	pubKeyFileContent3 := "PUBLIC KEY3"
	input(b, pubKeyFilePath3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == priKeyFilePath2 {
				data = []byte(priKeyFileContent2)
			} else if filepath == pubKeyFilePath2 {
				data = []byte(pubKeyFileContent2)
			} else if filepath == priKeyFilePath3 {
				data = []byte(priKeyFileContent3)
			} else if filepath == pubKeyFilePath3 {
				data = []byte(pubKeyFileContent3)
			} else if filepath == rootCaFilePath {
				data = []byte(rootCaFileContent)
			} else if filepath == adminCertFilePath {
				data = []byte(adminCertFileContent)
			} else if filepath == adminKeyFilePath {
				data = []byte(adminKeyFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == priKeyFilePath2 ||
				name == pubKeyFilePath2 ||
				name == priKeyFilePath3 ||
				name == pubKeyFilePath3 ||
				name == rootCaFilePath ||
				name == adminCertFilePath ||
				name == adminKeyFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptOpenSearchNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Opensearch.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Opensearch.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Opensearch.Config.PrivateKey) == 0)
	assert.Equal(t, rootCaFileContent, c.Config.Opensearch.Config.RootCA)
	assert.Equal(t, adminCertFileContent, c.Config.Opensearch.Config.AdminCert)
	assert.Equal(t, adminKeyFileContent, c.Config.Opensearch.Config.AdminKey)

	assert.Equal(t, enableNodeCustomCerts, len(*c.Config.Opensearch.Config.CertsByIP) > 0)
	assert.Equal(t, instanceCount, len(*c.Config.Opensearch.Config.CertsByIP))
	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.OpensearchPrivateIps))

	assert.Equal(t, ip1, (*c.Config.Opensearch.Config.CertsByIP)[0].IP)
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.OpensearchPrivateIps[0])
	assert.Equal(t, priKeyFileContent1, (*c.Config.Opensearch.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, pubKeyFileContent1, (*c.Config.Opensearch.Config.CertsByIP)[0].PublicKey)

	assert.Equal(t, ip2, (*c.Config.Opensearch.Config.CertsByIP)[1].IP)
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.OpensearchPrivateIps[1])
	assert.Equal(t, priKeyFileContent2, (*c.Config.Opensearch.Config.CertsByIP)[1].PrivateKey)
	assert.Equal(t, pubKeyFileContent2, (*c.Config.Opensearch.Config.CertsByIP)[1].PublicKey)

	assert.Equal(t, ip3, (*c.Config.Opensearch.Config.CertsByIP)[2].IP)
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.OpensearchPrivateIps[2])
	assert.Equal(t, priKeyFileContent3, (*c.Config.Opensearch.Config.CertsByIP)[2].PrivateKey)
	assert.Equal(t, pubKeyFileContent3, (*c.Config.Opensearch.Config.CertsByIP)[2].PublicKey)
}

func TestOnPremPromptOpenSearchNodesCustomCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := false
	input(b, "\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	adminCertFilePath := "admin.pem"
	adminCertFileContent := "ADMIN CERT"
	input(b, adminCertFilePath+"\r")

	adminKeyFilePath := "admin.key"
	adminKeyFileContent := "ADMIN KEY"
	input(b, adminKeyFilePath+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == rootCaFilePath {
				data = []byte(rootCaFileContent)
			} else if filepath == adminCertFilePath {
				data = []byte(adminCertFileContent)
			} else if filepath == adminKeyFilePath {
				data = []byte(adminKeyFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == rootCaFilePath ||
				name == adminCertFilePath ||
				name == adminKeyFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptOpenSearchNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Opensearch.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Opensearch.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Opensearch.Config.PrivateKey) == 0)
	assert.Equal(t, priKeyFileContent1, c.Config.Opensearch.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent1, c.Config.Opensearch.Config.PublicKey)
	assert.Equal(t, rootCaFileContent, c.Config.Opensearch.Config.RootCA)
	assert.Equal(t, adminCertFileContent, c.Config.Opensearch.Config.AdminCert)
	assert.Equal(t, adminKeyFileContent, c.Config.Opensearch.Config.AdminKey)

	assert.Equal(t, true, c.Config.Opensearch.Config.CertsByIP == nil)

	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.OpensearchPrivateIps))
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.OpensearchPrivateIps[0])
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.OpensearchPrivateIps[1])
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.OpensearchPrivateIps[2])
}

func TestOnPremPromptPostgresqlCustomCertPerNode(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := true
	input(b, moveDown+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	priKeyFilePath2 := "test2.key"
	priKeyFileContent2 := "PRIVATE KEY2"
	input(b, priKeyFilePath2+"\r")

	pubKeyFilePath2 := "test2.pem"
	pubKeyFileContent2 := "PUBLIC KEY2"
	input(b, pubKeyFilePath2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	priKeyFilePath3 := "test3.key"
	priKeyFileContent3 := "PRIVATE KEY3"
	input(b, priKeyFilePath3+"\r")

	pubKeyFilePath3 := "test3.pem"
	pubKeyFileContent3 := "PUBLIC KEY3"
	input(b, pubKeyFilePath3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == priKeyFilePath2 {
				data = []byte(priKeyFileContent2)
			} else if filepath == pubKeyFilePath2 {
				data = []byte(pubKeyFileContent2)
			} else if filepath == priKeyFilePath3 {
				data = []byte(priKeyFileContent3)
			} else if filepath == pubKeyFilePath3 {
				data = []byte(pubKeyFileContent3)
			} else if filepath == rootCaFilePath {
				data = []byte(rootCaFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == priKeyFilePath2 ||
				name == pubKeyFilePath2 ||
				name == priKeyFilePath3 ||
				name == pubKeyFilePath3 ||
				name == rootCaFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptPostgresql()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Postgresql.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Postgresql.Config.PrivateKey) == 0)
	assert.Equal(t, rootCaFileContent, c.Config.Postgresql.Config.RootCA)

	assert.Equal(t, enableNodeCustomCerts, len(*c.Config.Postgresql.Config.CertsByIP) > 0)
	assert.Equal(t, instanceCount, len(*c.Config.Postgresql.Config.CertsByIP))
	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.PostgresqlPrivateIps))

	assert.Equal(t, ip1, (*c.Config.Postgresql.Config.CertsByIP)[0].IP)
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[0])
	assert.Equal(t, priKeyFileContent1, (*c.Config.Postgresql.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, pubKeyFileContent1, (*c.Config.Postgresql.Config.CertsByIP)[0].PublicKey)

	assert.Equal(t, ip2, (*c.Config.Postgresql.Config.CertsByIP)[1].IP)
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[1])
	assert.Equal(t, priKeyFileContent2, (*c.Config.Postgresql.Config.CertsByIP)[1].PrivateKey)
	assert.Equal(t, pubKeyFileContent2, (*c.Config.Postgresql.Config.CertsByIP)[1].PublicKey)

	assert.Equal(t, ip3, (*c.Config.Postgresql.Config.CertsByIP)[2].IP)
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[2])
	assert.Equal(t, priKeyFileContent3, (*c.Config.Postgresql.Config.CertsByIP)[2].PrivateKey)
	assert.Equal(t, pubKeyFileContent3, (*c.Config.Postgresql.Config.CertsByIP)[2].PublicKey)
}

func TestOnPremPromptPostgresqlCustomCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := false
	input(b, "\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == rootCaFilePath {
				data = []byte(rootCaFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == rootCaFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptPostgresql()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Postgresql.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Postgresql.Config.PrivateKey) == 0)
	assert.Equal(t, priKeyFileContent1, c.Config.Postgresql.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent1, c.Config.Postgresql.Config.PublicKey)
	assert.Equal(t, rootCaFileContent, c.Config.Postgresql.Config.RootCA)

	assert.Equal(t, true, c.Config.Postgresql.Config.CertsByIP == nil)

	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.PostgresqlPrivateIps))
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[0])
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[1])
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.PostgresqlPrivateIps[2])
}

func TestOnPremPromptAutomateNodesCustomCertPerNode(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := true
	input(b, moveDown+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	priKeyFilePath2 := "test2.key"
	priKeyFileContent2 := "PRIVATE KEY2"
	input(b, priKeyFilePath2+"\r")

	pubKeyFilePath2 := "test2.pem"
	pubKeyFileContent2 := "PUBLIC KEY2"
	input(b, pubKeyFilePath2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	priKeyFilePath3 := "test3.key"
	priKeyFileContent3 := "PRIVATE KEY3"
	input(b, priKeyFilePath3+"\r")

	pubKeyFilePath3 := "test3.pem"
	pubKeyFileContent3 := "PUBLIC KEY3"
	input(b, pubKeyFilePath3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == priKeyFilePath2 {
				data = []byte(priKeyFileContent2)
			} else if filepath == pubKeyFilePath2 {
				data = []byte(pubKeyFileContent2)
			} else if filepath == priKeyFilePath3 {
				data = []byte(priKeyFileContent3)
			} else if filepath == pubKeyFilePath3 {
				data = []byte(pubKeyFileContent3)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == priKeyFilePath2 ||
				name == pubKeyFilePath2 ||
				name == priKeyFilePath3 ||
				name == pubKeyFilePath3 {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptAutomateNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Automate.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Automate.Config.EnableCustomCerts)
	assert.Equal(t, "", c.Config.Automate.Config.PublicKey)
	assert.Equal(t, "", c.Config.Automate.Config.PrivateKey)

	assert.Equal(t, enableNodeCustomCerts, len(*c.Config.Automate.Config.CertsByIP) > 0)
	assert.Equal(t, instanceCount, len(*c.Config.Automate.Config.CertsByIP))
	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.AutomatePrivateIps))

	assert.Equal(t, ip1, (*c.Config.Automate.Config.CertsByIP)[0].IP)
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.AutomatePrivateIps[0])
	assert.Equal(t, priKeyFileContent1, (*c.Config.Automate.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, pubKeyFileContent1, (*c.Config.Automate.Config.CertsByIP)[0].PublicKey)

	assert.Equal(t, ip2, (*c.Config.Automate.Config.CertsByIP)[1].IP)
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.AutomatePrivateIps[1])
	assert.Equal(t, priKeyFileContent2, (*c.Config.Automate.Config.CertsByIP)[1].PrivateKey)
	assert.Equal(t, pubKeyFileContent2, (*c.Config.Automate.Config.CertsByIP)[1].PublicKey)

	assert.Equal(t, ip3, (*c.Config.Automate.Config.CertsByIP)[2].IP)
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.AutomatePrivateIps[2])
	assert.Equal(t, priKeyFileContent3, (*c.Config.Automate.Config.CertsByIP)[2].PrivateKey)
	assert.Equal(t, pubKeyFileContent3, (*c.Config.Automate.Config.CertsByIP)[2].PublicKey)
}

func TestOnPremPromptAutomateNodesCustomCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := false
	input(b, "\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptAutomateNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.Automate.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.Automate.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Automate.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.Automate.Config.PrivateKey) == 0)
	assert.Equal(t, priKeyFileContent1, c.Config.Automate.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent1, c.Config.Automate.Config.PublicKey)

	assert.Equal(t, true, c.Config.Automate.Config.CertsByIP == nil)

	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.AutomatePrivateIps))
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.AutomatePrivateIps[0])
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.AutomatePrivateIps[1])
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.AutomatePrivateIps[2])
}

func TestOnPremPromptChefInfraServerNodesCustomCertPerNode(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := true
	input(b, moveDown+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	priKeyFilePath2 := "test2.key"
	priKeyFileContent2 := "PRIVATE KEY2"
	input(b, priKeyFilePath2+"\r")

	pubKeyFilePath2 := "test2.pem"
	pubKeyFileContent2 := "PUBLIC KEY2"
	input(b, pubKeyFilePath2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	priKeyFilePath3 := "test3.key"
	priKeyFileContent3 := "PRIVATE KEY3"
	input(b, priKeyFilePath3+"\r")

	pubKeyFilePath3 := "test3.pem"
	pubKeyFileContent3 := "PUBLIC KEY3"
	input(b, pubKeyFilePath3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			} else if filepath == priKeyFilePath2 {
				data = []byte(priKeyFileContent2)
			} else if filepath == pubKeyFilePath2 {
				data = []byte(pubKeyFileContent2)
			} else if filepath == priKeyFilePath3 {
				data = []byte(priKeyFileContent3)
			} else if filepath == pubKeyFilePath3 {
				data = []byte(pubKeyFileContent3)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 ||
				name == priKeyFilePath2 ||
				name == pubKeyFilePath2 ||
				name == priKeyFilePath3 ||
				name == pubKeyFilePath3 {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptChefInfraServerNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.ChefServer.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, "", c.Config.ChefServer.Config.PublicKey)
	assert.Equal(t, "", c.Config.ChefServer.Config.PrivateKey)

	assert.Equal(t, enableNodeCustomCerts, len(*c.Config.ChefServer.Config.CertsByIP) > 0)
	assert.Equal(t, instanceCount, len(*c.Config.ChefServer.Config.CertsByIP))
	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.ChefServerPrivateIps))

	assert.Equal(t, ip1, (*c.Config.ChefServer.Config.CertsByIP)[0].IP)
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.ChefServerPrivateIps[0])
	assert.Equal(t, priKeyFileContent1, (*c.Config.ChefServer.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, pubKeyFileContent1, (*c.Config.ChefServer.Config.CertsByIP)[0].PublicKey)

	assert.Equal(t, ip2, (*c.Config.ChefServer.Config.CertsByIP)[1].IP)
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.ChefServerPrivateIps[1])
	assert.Equal(t, priKeyFileContent2, (*c.Config.ChefServer.Config.CertsByIP)[1].PrivateKey)
	assert.Equal(t, pubKeyFileContent2, (*c.Config.ChefServer.Config.CertsByIP)[1].PublicKey)

	assert.Equal(t, ip3, (*c.Config.ChefServer.Config.CertsByIP)[2].IP)
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.ChefServerPrivateIps[2])
	assert.Equal(t, priKeyFileContent3, (*c.Config.ChefServer.Config.CertsByIP)[2].PrivateKey)
	assert.Equal(t, pubKeyFileContent3, (*c.Config.ChefServer.Config.CertsByIP)[2].PublicKey)
}

func TestOnPremPromptChefInfraServerNodesCustomCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	instanceCount := 3
	input(b, fmt.Sprint(instanceCount)+"\r")

	enableCustomCerts := true
	input(b, "\r")

	enableNodeCustomCerts := false
	input(b, "\r")

	priKeyFilePath1 := "test1.key"
	priKeyFileContent1 := "PRIVATE KEY1"
	input(b, priKeyFilePath1+"\r")

	pubKeyFilePath1 := "test1.pem"
	pubKeyFileContent1 := "PUBLIC KEY1"
	input(b, pubKeyFilePath1+"\r")

	ip1 := "172.168.192.1"
	input(b, ip1+"\r")

	ip2 := "172.168.192.2"
	input(b, ip2+"\r")

	ip3 := "172.168.192.3"
	input(b, ip3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath1 {
				data = []byte(priKeyFileContent1)
			} else if filepath == pubKeyFilePath1 {
				data = []byte(pubKeyFileContent1)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath1 ||
				name == pubKeyFilePath1 {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCerts,
	}

	err := c.PromptChefInfraServerNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(instanceCount), c.Config.ChefServer.Config.InstanceCount)
	assert.Equal(t, enableCustomCerts, c.Config.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.ChefServer.Config.PublicKey) == 0)
	assert.Equal(t, enableNodeCustomCerts, len(c.Config.ChefServer.Config.PrivateKey) == 0)
	assert.Equal(t, priKeyFileContent1, c.Config.ChefServer.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent1, c.Config.ChefServer.Config.PublicKey)

	assert.Equal(t, true, c.Config.ChefServer.Config.CertsByIP == nil)

	assert.Equal(t, instanceCount, len(c.Config.ExistingInfra.Config.ChefServerPrivateIps))
	assert.Equal(t, ip1, c.Config.ExistingInfra.Config.ChefServerPrivateIps[0])
	assert.Equal(t, ip2, c.Config.ExistingInfra.Config.ChefServerPrivateIps[1])
	assert.Equal(t, ip3, c.Config.ExistingInfra.Config.ChefServerPrivateIps[2])
}

func TestOnPremPromptBackupObjectStorage(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	useObjectStorage := true
	input(b, moveDown+moveDown+"\r")

	backupBucketName := "automate-backup"
	input(b, backupBucketName+"\r")

	backupAcessKeyId := "AKEKFKEIDKETODDFEDFE"
	input(b, backupAcessKeyId+"\r")

	backupAcessKeySecret := "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"
	input(b, backupAcessKeySecret+"\r")
	backupEndpoint := "https://s3.amazonaws.com"
	input(b, backupEndpoint+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()

	assert.NoError(t, err)
	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupConfig != "")
	assert.Equal(t, useObjectStorage, c.Config.Architecture.ExistingInfra.BackupConfig == "object_storage")
	assert.Equal(t, backupBucketName, c.Config.ObjectStorage.Config.BucketName)
	assert.Equal(t, backupAcessKeyId, c.Config.ObjectStorage.Config.AccessKey)
	assert.Equal(t, backupEndpoint, c.Config.ObjectStorage.Config.Endpoint)
	assert.Equal(t, "/mnt/automate_backups", c.Config.Architecture.ExistingInfra.BackupMount)
}

func TestOnPremPromptBackupMinio(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	useMinio := true
	input(b, moveDown+"\r")

	backupBucketName := "automate-backup"
	input(b, backupBucketName+"\r")

	backupAcessKeyId := "AKEKFKEIDKETODDFEDFE"
	input(b, backupAcessKeyId+"\r")

	backupAcessKeySecret := "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"
	input(b, backupAcessKeySecret+"\r")

	backupEndpoint := "http://35.182.23.44:9000"
	input(b, backupEndpoint+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()

	assert.NoError(t, err)
	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupConfig != "")
	assert.Equal(t, useMinio, c.Config.Architecture.ExistingInfra.BackupConfig == "object_storage")
	assert.Equal(t, backupBucketName, c.Config.ObjectStorage.Config.BucketName)
	assert.Equal(t, backupAcessKeyId, c.Config.ObjectStorage.Config.AccessKey)
	assert.Equal(t, backupEndpoint, c.Config.ObjectStorage.Config.Endpoint)
	assert.Equal(t, "/mnt/automate_backups", c.Config.Architecture.ExistingInfra.BackupMount)
}

func TestOnPremPromptBackupMinioWithDomain(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	useMinio := true
	input(b, moveDown+"\r")

	backupBucketName := "automate-backup"
	input(b, backupBucketName+"\r")

	backupAcessKeyId := "AKEKFKEIDKETODDFEDFE"
	input(b, backupAcessKeyId+"\r")

	backupAcessKeySecret := "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"
	input(b, backupAcessKeySecret+"\r")

	backupEndpoint := "http://s3.amazonaws.com:9000"
	input(b, backupEndpoint+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()

	assert.NoError(t, err)
	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupConfig != "")
	assert.Equal(t, useMinio, c.Config.Architecture.ExistingInfra.BackupConfig == "object_storage")
	assert.Equal(t, backupBucketName, c.Config.ObjectStorage.Config.BucketName)
	assert.Equal(t, backupAcessKeyId, c.Config.ObjectStorage.Config.AccessKey)
	assert.Equal(t, backupEndpoint, c.Config.ObjectStorage.Config.Endpoint)
	assert.Equal(t, "/mnt/automate_backups", c.Config.Architecture.ExistingInfra.BackupMount)
}

func TestOnPremPromptBackupMinioerror(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	useMinio := true
	input(b, moveDown+"\r")

	backupBucketName := "automate-backup"
	input(b, backupBucketName+"\r")

	backupAcessKeyId := "AKEKFKEIDKETODDFEDFE"
	input(b, backupAcessKeyId+"\r")

	backupAcessKeySecret := "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"
	input(b, backupAcessKeySecret+"\r")

	backupEndpoint := "https://192.168.1.1:"
	input(b, backupEndpoint+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()


	assert.Error(t, err)
	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupConfig != "")
	assert.Equal(t, useMinio, c.Config.Architecture.ExistingInfra.BackupConfig == "object_storage")
	assert.Equal(t, backupBucketName, c.Config.ObjectStorage.Config.BucketName)
	assert.Equal(t, backupAcessKeyId, c.Config.ObjectStorage.Config.AccessKey)
	assert.Equal(t, "/mnt/automate_backups", c.Config.Architecture.ExistingInfra.BackupMount)
}

func TestOnPremPromptBackupNFS(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	useNfs := true
	input(b, moveDown+moveDown+moveDown+moveDown+"\r")

	backupMountLoc := "/mnt/custom-backup-mount"
	input(b, backupMountLoc+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()

	assert.NoError(t, err)
	assert.Equal(t, enableBackup, c.Config.Architecture.ExistingInfra.BackupConfig != "")
	assert.Equal(t, useNfs, c.Config.Architecture.ExistingInfra.BackupConfig == "file_system")
	assert.Equal(t, backupMountLoc, c.Config.Architecture.ExistingInfra.BackupMount)
}
func TestOnPremBatchRunErr(t *testing.T) {
	for i := 0; i <= 35; i++ {
		testPromptsOnPremManagedErr(t, i)
	}
}

func testPromptsOnPremManagedErr(t *testing.T, upto int) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	callInputForHaDeploy(upto, b)

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == "test.pem" {
				data = []byte("test.pem")
			} else if filepath == "automateFqdnRootCa.pem" {
				data = []byte("automateFqdnRootCa.pem")
			} else if filepath == "chefInfraServerFqdnRootCa.pem" {
				data = []byte("chefInfraServerFqdnRootCa.pem")
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" ||
				name == "automateFqdnRootCa.pem" ||
				name == "chefInfraServerFqdnRootCa.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "existing_nodes"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.ExistingInfra.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.ExistingInfra.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.ExistingInfra.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.ExistingInfra.WorkspacePath)
}

func callInputForHaDeploy(upto int, b *bytes.Buffer) {
	if upto >= 1 {
		input(b, "ec2-user"+"\r")
	}
	if upto >= 2 {
		input(b, "\r")
	}
	if upto >= 3 {
		input(b, "\r")
	}
	if upto >= 4 {
		input(b, "test.pem"+"\r")
	}
	if upto >= 5 {
		input(b, "\r")
	}
	if upto >= 6 {
		input(b, "A2-automate.amazonaws.com"+"\r")
	}
	if upto >= 7 {
		input(b, "automateFqdnRootCa.pem"+"\r")
	}
	if upto >= 8 {
		input(b, "admin12345"+"\r")
	}
	if upto >= 9 {
		input(b, fmt.Sprint(1)+"\r")
	}
	if upto >= 10 {
		input(b, "172.168.192.1"+"\r")
	}
	if upto >= 11 {
		input(b, "chef-infra-server.amazonaws.com"+"\r")
	}
	if upto >= 12 {
		input(b, "chefInfraServerFqdnRootCa.pem"+"\r")
	}
	if upto >= 13 {
		input(b, fmt.Sprint(1)+"\r")
	}
	if upto >= 14 {
		input(b, "172.168.192.4"+"\r")
	}
	if upto >= 15 {
		input(b, "\r")
	}
	if upto >= 16 {
		input(b, "\r")
	}
	if upto >= 17 {
		input(b, "opensearch-automate"+"\r")
	}
	if upto >= 18 {
		input(b, "opensearch.automate.com"+"\r")
	}
	if upto >= 19 {
		input(b, "admin"+"\r")
	}
	if upto >= 20 {
		input(b, "admin@1234"+"\r")
	}
	if upto >= 21 {
		input(b, "\r")
	}
	if upto >= 22 {
		input(b, "arn-os-snapshot-role"+"\r")
	}
	if upto >= 23 {
		input(b, "ASKEIDSKFLDKDFJERKDS"+"\r")
	}
	if upto >= 24 {
		input(b, "asdfelkjsldfklsdjflsdfjdklsfjsdfsldkfjsd"+"\r")
	}
	if upto >= 25 {
		input(b, "pg.automate.com:5432"+"\r")
	}
	if upto >= 26 {
		input(b, "root"+"\r")
	}
	if upto >= 27 {
		input(b, "root1234"+"\r")
	}
	if upto >= 28 {
		input(b, "dbUser"+"\r")
	}
	if upto >= 29 {
		input(b, "dbUser1234"+"\r")
	}
	if upto >= 30 {
		input(b, "\r")
	}
	if upto >= 31 {
		input(b, "\r")
	}
	if upto >= 32 {
		input(b, "\r")
	}
	if upto >= 33 {
		input(b, "automate-backup"+"\r")
	}
	if upto >= 34 {
		input(b, "AKEKFKEIDKETODDFEDFE"+"\r")
	}
	if upto >= 35 {
		input(b, "sdfsdflsdlfkjsdlkjeflilsfldijflsdkfjddfd"+"\r")
	}
	if upto >= 36 {
		input(b, "asia"+moveDown+"\r")
	}
}

func TestPromptExternalPostgresqlRootCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	input(b, moveDown+"\r")

	certFilePath := "test.pem"
	certFileContent := "ROOT CA"
	input(b, certFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == certFilePath {
				data = []byte(certFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == certFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	c.Config.InitExternal().InitDatabase().Type = "aws"

	err := c.PromptExternalPostgresqlRootCert()

	assert.NoError(t, err)

	assert.Equal(t, certFileContent, c.Config.External.Database.PostgreSQL.PostgresqlRootCert)
}

func TestPromptExternalOpenSearchRootCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	input(b, moveDown+"\r")

	certFilePath := "test.pem"
	certFileContent := "ROOT CA"
	input(b, certFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == certFilePath {
				data = []byte(certFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == certFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := HaDeployConfigGen{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	c.Config.InitExternal().InitDatabase().Type = "aws"

	err := c.PromptExternalOpenSearchRootCert()

	assert.NoError(t, err)

	assert.Equal(t, certFileContent, c.Config.External.Database.OpenSearch.OpensearchRootCert)
}

func TestSetDefaultValuesForDBNodesOnPrem(t *testing.T) {
	defaultInstanceCount := "0"
	c := HaDeployConfigGen{
		Config: &config.HaDeployConfig{},
	}
	c.SetDefaultValuesForDBNodes()

	assert.Equal(t, defaultInstanceCount, c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, defaultInstanceCount, c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, false, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, false, c.Config.Opensearch.Config.EnableCustomCerts)
}
