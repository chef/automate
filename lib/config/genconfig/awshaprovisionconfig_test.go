package genconfig

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/ioutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/stretchr/testify/assert"
)

func pad(siz int, buf *bytes.Buffer) {
	pu := make([]byte, 4096-siz)
	for i := 0; i < 4096-siz; i++ {
		pu[i] = 97
	}
	buf.Write(pu)
}

func input(buf *bytes.Buffer, input string) {
	buf.WriteString(input)
	pad(len(input), buf)
}

func TestPromptsCidrAWSManaged(t *testing.T) {
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

	input(b, moveDown+"\r")

	awsProfile := "default"
	input(b, "\r")

	awsRegion := "ap-south-2"
	input(b, "asia"+moveDown+"\r")

	vpcId := "vpc-123"
	input(b, vpcId+"\r")

	useCidr := true
	input(b, "\r")

	cidr := "172.168.192.0"
	input(b, cidr+"\r")

	awsSshKeyPairName := "my-key-pair"
	input(b, awsSshKeyPairName+"\r")

	awsAmiId := "ami-123"
	input(b, awsAmiId+"\r")

	delOnTerm := false
	input(b, "\r")

	enableAccessLogsLb := true
	input(b, "\r")

	automateFqdn := "A2-automate.amazonaws.com"
	input(b, automateFqdn+"\r")

	automateLbCertArn := "arn-123"
	input(b, automateLbCertArn+"\r")

	automateFqdnRootCaFilePath := "automateFqdnRootCa.pem"
	automateFqdnRootCaFileContent := "Automate Root CA"
	input(b, automateFqdnRootCaFilePath+"\r")

	automateAdminPass := "admin12345"
	input(b, automateAdminPass+"\r")

	automateNodeCount := 1
	input(b, fmt.Sprint(automateNodeCount)+"\r")

	automateInstanceType := "m5.large"
	input(b, "\r")

	automateVolSize := 200
	input(b, "\r")

	automateVolType := "gp3"
	input(b, "\r")

	automateVolIops := 100
	input(b, "\r")

	chefInfraServerFqdn := "chef-infra-server.amazonaws.com"
	input(b, chefInfraServerFqdn+"\r")

	chefInfraServerLbCertArn := "arn-124"
	input(b, chefInfraServerLbCertArn+"\r")

	chefInfraServerFqdnRootCaFilePath := "chefInfraServerFqdnRootCa.pem"
	chefInfraServerFqdnRootCaFileContent := "Chef Infra Server Root CA"
	input(b, chefInfraServerFqdnRootCaFilePath+"\r")

	chefInfraServerNodeCount := 1
	input(b, fmt.Sprint(chefInfraServerNodeCount)+"\r")

	chefInfraServerInstanceType := "m5.large"
	input(b, "\r")

	chefInfraServerVolSize := 200
	input(b, "\r")

	chefInfraServerVolType := "gp3"
	input(b, "\r")

	chefInfraServerVolIops := 100
	input(b, "\r")

	useAwsManagedServices := true
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

	// To not skip role cred
	input(b, moveDown+"\r")

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

	awsBackupBucketName := "automate-backup"
	input(b, awsBackupBucketName+"\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
		httpRequestClient: &httputils.MockHTTPClient{
			MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
				if url == TOKEN_URLS {
					return &http.Response{
						StatusCode: 200,
					}, []byte("dummytoken"), nil
				}
				return &http.Response{StatusCode: 200}, []byte(`{
					"Code" : "Success",
					"LastUpdated" : "2023-08-09T11:23:21Z",
					"InstanceProfileArn" : "arn:aws:iam::1234:instance-profile/bastion-role-user",
					"InstanceProfileId" : "AIPARHU"
				}`), nil
			},
		},
	}

	err := c.Prompts()

	assert.NoError(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"
	automateConfigFile := "configs/automate.toml"
	pgInstanceCount := 0
	osInstanceCount := 0

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)

	assert.Equal(t, sshUser, c.Config.Architecture.Aws.SSHUser)
	assert.Equal(t, sshGroup, c.Config.Architecture.Aws.SSHGroupName)
	assert.Equal(t, fmt.Sprint(sshPort), c.Config.Architecture.Aws.SSHPort)
	assert.Equal(t, sshKeyFilePath, c.Config.Architecture.Aws.SSHKeyFile)

	assert.Equal(t, enableCustomCerts, c.HasCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Automate.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, enableCustomCerts, c.Config.Opensearch.Config.EnableCustomCerts)

	assert.Equal(t, awsProfile, c.Config.Aws.Config.Profile)
	assert.Equal(t, awsRegion, c.Config.Aws.Config.Region)
	assert.Equal(t, vpcId, c.Config.Aws.Config.AwsVpcID)
	assert.Equal(t, useCidr, len(c.Config.Aws.Config.AwsCidrBlockAddr) > 0)
	assert.Equal(t, cidr, c.Config.Aws.Config.AwsCidrBlockAddr)

	assert.Equal(t, awsSshKeyPairName, c.Config.Aws.Config.SSHKeyPairName)
	assert.Equal(t, awsAmiId, c.Config.Aws.Config.AmiID)
	assert.Equal(t, delOnTerm, c.Config.Aws.Config.DeleteOnTermination)
	assert.Equal(t, fmt.Sprint(enableAccessLogsLb), c.Config.Aws.Config.LbAccessLogs)

	assert.Equal(t, automateFqdn, c.Config.Automate.Config.Fqdn)
	assert.Equal(t, automateLbCertArn, c.Config.Aws.Config.AutomateLbCertificateArn)
	assert.Equal(t, automateFqdnRootCaFileContent, c.Config.Automate.Config.FqdnRootCA)
	assert.Equal(t, automateConfigFile, c.Config.Automate.Config.ConfigFile)
	assert.Equal(t, automateAdminPass, c.Config.Automate.Config.AdminPassword)
	assert.Equal(t, fmt.Sprint(automateNodeCount), c.Config.Automate.Config.InstanceCount)
	assert.Equal(t, automateInstanceType, c.Config.Aws.Config.AutomateServerInstanceType)
	assert.Equal(t, fmt.Sprint(automateVolSize), c.Config.Aws.Config.AutomateEbsVolumeSize)
	assert.Equal(t, automateVolType, c.Config.Aws.Config.AutomateEbsVolumeType)
	assert.Equal(t, fmt.Sprint(automateVolIops), c.Config.Aws.Config.AutomateEbsVolumeIops)

	assert.Equal(t, chefInfraServerFqdn, c.Config.ChefServer.Config.ChefServerFqdn)
	assert.Equal(t, chefInfraServerLbCertArn, c.Config.Aws.Config.ChefServerLbCertificateArn)
	assert.Equal(t, chefInfraServerFqdnRootCaFileContent, c.Config.ChefServer.Config.FqdnRootCA)
	assert.Equal(t, fmt.Sprint(chefInfraServerNodeCount), c.Config.ChefServer.Config.InstanceCount)
	assert.Equal(t, chefInfraServerInstanceType, c.Config.Aws.Config.ChefServerInstanceType)
	assert.Equal(t, fmt.Sprint(chefInfraServerVolSize), c.Config.Aws.Config.ChefEbsVolumeSize)
	assert.Equal(t, chefInfraServerVolType, c.Config.Aws.Config.ChefEbsVolumeType)
	assert.Equal(t, fmt.Sprint(chefInfraServerVolIops), c.Config.Aws.Config.ChefEbsVolumeIops)

	assert.Equal(t, useAwsManagedServices, c.Config.Aws.Config.SetupManagedServices)

	assert.Equal(t, osDomainName, c.Config.Aws.Config.ManagedOpensearchDomainName)
	assert.Equal(t, osDomainUrl, c.Config.Aws.Config.ManagedOpensearchDomainURL)
	assert.Equal(t, osUser, c.Config.Aws.Config.ManagedOpensearchUsername)
	assert.Equal(t, osPass, c.Config.Aws.Config.ManagedOpensearchUserPassword)
	assert.Equal(t, osDefaultCerts, len(c.Config.Aws.Config.ManagedOpensearchCertificate) == 0)
	assert.Equal(t, osSnapshotRoleArn, c.Config.Aws.Config.AwsOsSnapshotRoleArn)
	assert.Equal(t, osSnapshotUserAccessKeyId, c.Config.Aws.Config.OsSnapshotUserAccessKeyID)
	assert.Equal(t, osSnapshotUserAccessKeySecret, c.Config.Aws.Config.OsSnapshotUserAccessKeySecret)

	assert.Equal(t, pgUrl, c.Config.Aws.Config.ManagedRdsInstanceURL)
	assert.Equal(t, pgSuperUserName, c.Config.Aws.Config.ManagedRdsSuperuserUsername)
	assert.Equal(t, pgSuperUserPass, c.Config.Aws.Config.ManagedRdsSuperuserPassword)
	assert.Equal(t, pgDbUserName, c.Config.Aws.Config.ManagedRdsDbuserUsername)
	assert.Equal(t, pgDbUserPass, c.Config.Aws.Config.ManagedRdsDbuserPassword)
	assert.Equal(t, pgDefaultCerts, len(c.Config.Aws.Config.ManagedRdsCertificate) == 0)

	assert.Equal(t, fmt.Sprint(osInstanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, fmt.Sprint(pgInstanceCount), c.Config.Postgresql.Config.InstanceCount)

	assert.Equal(t, enableBackup, len(c.Config.Architecture.Aws.BackupConfig) > 0)
	assert.Equal(t, "s3", c.Config.Architecture.Aws.BackupConfig)
	assert.Equal(t, awsBackupBucketName, c.Config.Architecture.Aws.S3BucketName)
}

func TestPromptBackup(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	enableBackup := true
	input(b, "\r")

	backupConfig := "efs"
	input(b, moveDown+"\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptBackup()

	assert.NoError(t, err)
	assert.Equal(t, enableBackup, len(c.Config.Architecture.Aws.BackupConfig) > 0)
	assert.Equal(t, "/mnt/automate_backups", c.Config.Architecture.Aws.BackupMount)
	assert.Equal(t, backupConfig, c.Config.Architecture.Aws.BackupConfig)
}

func TestPromptDatabases(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	isAwsManagedDb := false
	input(b, moveDown+"\r")

	pgInstanceCount := 3
	input(b, fmt.Sprint(pgInstanceCount)+"\r")

	pgInstanceType := "m5.large"
	input(b, "\r")

	pgVolSize := 200
	input(b, "\r")

	pgVolType := "gp3"
	input(b, "\r")

	pgVolIops := 100
	input(b, "\r")

	osInstanceCount := 3
	input(b, fmt.Sprint(osInstanceCount)+"\r")

	osInstanceType := "m5.large"
	input(b, "\r")

	osVolSize := 200
	input(b, "\r")

	osVolType := "gp3"
	input(b, "\r")

	osVolIops := 100
	input(b, "\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptDatabases()

	assert.NoError(t, err)
	assert.Equal(t, isAwsManagedDb, c.Config.Aws.Config.SetupManagedServices)
	assert.Equal(t, fmt.Sprint(pgInstanceCount), c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, pgInstanceType, c.Config.Aws.Config.PostgresqlServerInstanceType)
	assert.Equal(t, fmt.Sprint(pgVolSize), c.Config.Aws.Config.PostgresqlEbsVolumeSize)
	assert.Equal(t, pgVolType, c.Config.Aws.Config.PostgresqlEbsVolumeType)
	assert.Equal(t, fmt.Sprint(pgVolIops), c.Config.Aws.Config.PostgresqlEbsVolumeIops)
	assert.Equal(t, fmt.Sprint(osInstanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, osInstanceType, c.Config.Aws.Config.OpensearchServerInstanceType)
	assert.Equal(t, fmt.Sprint(osVolSize), c.Config.Aws.Config.OpensearchEbsVolumeSize)
	assert.Equal(t, osVolType, c.Config.Aws.Config.OpensearchEbsVolumeType)
	assert.Equal(t, fmt.Sprint(osVolIops), c.Config.Aws.Config.OpensearchEbsVolumeIops)
}

func TestPromptCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	certVal, err := c.PromptCert("", OPENSEARCH_NODETYPE, ROOTCA)

	assert.NoError(t, err)
	assert.Equal(t, certFileContent, certVal)
}

func TestPromptAutomateNodes(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	automateInstanceCount := 1
	input(b, fmt.Sprint(automateInstanceCount)+"\r")

	enableCustomCert := true
	input(b, "\r")

	priKeyFilePath := "test.key"
	priKeyFileContent := "PRIVATE KEY"
	input(b, priKeyFilePath+"\r")

	pubKeyFilePath := "test.pem"
	pubKeyFileContent := "PUBLIC KEY"
	input(b, pubKeyFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath {
				data = []byte(priKeyFileContent)
			} else if filepath == pubKeyFilePath {
				data = []byte(pubKeyFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath ||
				name == pubKeyFilePath {
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

	c := AwsHaProvisionConfig{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCert,
	}

	err := c.PromptAutomateNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(automateInstanceCount), c.Config.Automate.Config.InstanceCount)
	assert.Equal(t, enableCustomCert, c.HasCustomCerts)
	assert.Equal(t, enableCustomCert, c.Config.Automate.Config.EnableCustomCerts)
	assert.Equal(t, priKeyFileContent, c.Config.Automate.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent, c.Config.Automate.Config.PublicKey)
}

func TestPromptChefInfraServerNodes(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	chefInfraServerInstanceCount := 1
	input(b, fmt.Sprint(chefInfraServerInstanceCount)+"\r")

	enableCustomCert := true
	input(b, "\r")

	priKeyFilePath := "test.key"
	priKeyFileContent := "PRIVATE KEY"
	input(b, priKeyFilePath+"\r")

	pubKeyFilePath := "test.pem"
	pubKeyFileContent := "PUBLIC KEY"
	input(b, pubKeyFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath {
				data = []byte(priKeyFileContent)
			} else if filepath == pubKeyFilePath {
				data = []byte(pubKeyFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath ||
				name == pubKeyFilePath {
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

	c := AwsHaProvisionConfig{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCert,
	}

	err := c.PromptChefInfraServerNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(chefInfraServerInstanceCount), c.Config.ChefServer.Config.InstanceCount)
	assert.Equal(t, enableCustomCert, c.HasCustomCerts)
	assert.Equal(t, enableCustomCert, c.Config.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, priKeyFileContent, c.Config.ChefServer.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent, c.Config.ChefServer.Config.PublicKey)
}

func TestPromptPostgresqlNodes(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	pgInstanceCount := 3
	input(b, fmt.Sprint(pgInstanceCount)+"\r")

	enableCustomCert := true
	input(b, "\r")

	priKeyFilePath := "test.key"
	priKeyFileContent := "PRIVATE KEY"
	input(b, priKeyFilePath+"\r")

	pubKeyFilePath := "test.pem"
	pubKeyFileContent := "PUBLIC KEY"
	input(b, pubKeyFilePath+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath {
				data = []byte(priKeyFileContent)
			} else if filepath == pubKeyFilePath {
				data = []byte(pubKeyFileContent)
			} else if filepath == rootCaFilePath {
				data = []byte(rootCaFileContent)
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == priKeyFilePath ||
				name == pubKeyFilePath ||
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

	c := AwsHaProvisionConfig{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCert,
	}

	err := c.PromptPostgresqlNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(pgInstanceCount), c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, enableCustomCert, c.HasCustomCerts)
	assert.Equal(t, enableCustomCert, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, priKeyFileContent, c.Config.Postgresql.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent, c.Config.Postgresql.Config.PublicKey)
	assert.Equal(t, rootCaFileContent, c.Config.Postgresql.Config.RootCA)
}

func TestPromptOpenSearchNodes(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	osInstanceCount := 3
	input(b, fmt.Sprint(osInstanceCount)+"\r")

	enableCustomCert := true
	input(b, "\r")

	priKeyFilePath := "test.key"
	priKeyFileContent := "PRIVATE KEY"
	input(b, priKeyFilePath+"\r")

	pubKeyFilePath := "test.pem"
	pubKeyFileContent := "PUBLIC KEY"
	input(b, pubKeyFilePath+"\r")

	rootCaFilePath := "rootca.pem"
	rootCaFileContent := "ROOT CA"
	input(b, rootCaFilePath+"\r")

	adminCertFilePath := "admin.pem"
	adminCertFileContent := "ADMIN CERT"
	input(b, adminCertFilePath+"\r")

	adminKeyFilePath := "admin.key"
	adminKeyFileContent := "ADMIN KEY"
	input(b, adminKeyFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == priKeyFilePath {
				data = []byte(priKeyFileContent)
			} else if filepath == pubKeyFilePath {
				data = []byte(pubKeyFileContent)
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
			if name == priKeyFilePath ||
				name == pubKeyFilePath ||
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

	c := AwsHaProvisionConfig{
		Prompt:         p,
		FileUtils:      mfsu,
		Config:         &config.HaDeployConfig{},
		HasCustomCerts: enableCustomCert,
	}

	err := c.PromptOpenSearchNodes()

	assert.NoError(t, err)
	assert.Equal(t, fmt.Sprint(osInstanceCount), c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, enableCustomCert, c.HasCustomCerts)
	assert.Equal(t, enableCustomCert, c.Config.Opensearch.Config.EnableCustomCerts)
	assert.Equal(t, priKeyFileContent, c.Config.Opensearch.Config.PrivateKey)
	assert.Equal(t, pubKeyFileContent, c.Config.Opensearch.Config.PublicKey)
	assert.Equal(t, rootCaFileContent, c.Config.Opensearch.Config.RootCA)
	assert.Equal(t, adminCertFileContent, c.Config.Opensearch.Config.AdminCert)
	assert.Equal(t, adminKeyFileContent, c.Config.Opensearch.Config.AdminKey)
}

func TestPromptPrivateSubnet(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	subnet1 := "subnet-1"
	input(b, subnet1+"\r")
	subnet2 := "subnet-2"
	input(b, subnet2+"\r")
	subnet3 := "subnet-3"
	input(b, subnet3+"\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptPrivateSubnet()

	assert.NoError(t, err)
	assert.Equal(t, subnet1, c.Config.Aws.Config.PrivateCustomSubnets[0])
	assert.Equal(t, subnet2, c.Config.Aws.Config.PrivateCustomSubnets[1])
	assert.Equal(t, subnet3, c.Config.Aws.Config.PrivateCustomSubnets[2])
}

func TestPromptPublicSubnet(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	enablePublicSubnets := true
	input(b, "\r")

	subnet1 := "172.168.192.1"
	input(b, subnet1+"\r")
	subnet2 := "172.168.192.2"
	input(b, subnet2+"\r")
	subnet3 := "172.168.192.3"
	input(b, subnet3+"\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptPublicSubnet()

	assert.NoError(t, err)
	assert.Equal(t, enablePublicSubnets, len(c.Config.Aws.Config.PublicCustomSubnets) > 0)
	assert.Equal(t, subnet1, c.Config.Aws.Config.PublicCustomSubnets[0])
	assert.Equal(t, subnet2, c.Config.Aws.Config.PublicCustomSubnets[1])
	assert.Equal(t, subnet3, c.Config.Aws.Config.PublicCustomSubnets[2])
}

func TestPromptPgCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	noDefaultAwsCerts := true
	input(b, moveDown+"\r")

	certFilePath := "test.pem"
	certFileContent := "POSTGRESQL CERT"
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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptPgCert()

	assert.NoError(t, err)
	assert.Equal(t, noDefaultAwsCerts, c.Config.Aws.Config.ManagedRdsCertificate != "")
	assert.Equal(t, certFileContent, c.Config.Aws.Config.ManagedRdsCertificate)
}

func TestPromptOsCert(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	noDefaultAwsCerts := true
	input(b, moveDown+"\r")

	certFilePath := "test.pem"
	certFileContent := "OPENSEARCH CERT"
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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptOsCert()

	assert.NoError(t, err)
	assert.Equal(t, noDefaultAwsCerts, c.Config.Aws.Config.ManagedOpensearchCertificate != "")
	assert.Equal(t, certFileContent, c.Config.Aws.Config.ManagedOpensearchCertificate)
}

func TestPromptsCidrAWSManagedErr(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)
}

func TestPromptsCidrAWSManagedErr1(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	sshUser := "ec2-user"
	input(b, sshUser+"\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)

	assert.Equal(t, sshUser, c.Config.Architecture.Aws.SSHUser)
}

func TestPromptsCidrAWSManagedErr2(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	sshUser := "ec2-user"
	input(b, sshUser+"\r")

	sshGroup := "ec2-user"
	input(b, "\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)

	assert.Equal(t, sshUser, c.Config.Architecture.Aws.SSHUser)
	assert.Equal(t, sshGroup, c.Config.Architecture.Aws.SSHGroupName)
}

func TestPromptsCidrAWSManagedErr3(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	sshUser := "ec2-user"
	input(b, sshUser+"\r")

	sshGroup := "ec2-user"
	input(b, "\r")

	sshPort := 22
	input(b, "\r")

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)

	assert.Equal(t, sshUser, c.Config.Architecture.Aws.SSHUser)
	assert.Equal(t, sshGroup, c.Config.Architecture.Aws.SSHGroupName)
	assert.Equal(t, fmt.Sprint(sshPort), c.Config.Architecture.Aws.SSHPort)
}

func TestBatchRunErr(t *testing.T) {
	for i := 0; i <= 47; i++ {
		testPromptsCidrAWSManagedErrRun(t, i)
	}
}

func testPromptsCidrAWSManagedErrRun(t *testing.T, upto int) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	callInput(upto, b)

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

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
		httpRequestClient: &httputils.MockHTTPClient{
			MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
				if url == TOKEN_URLS {
					return &http.Response{
						StatusCode: 200,
					}, []byte("dummytoken"), nil
				}
				return &http.Response{StatusCode: 200}, []byte(`{
					"Code" : "Success",
					"LastUpdated" : "2023-08-09T11:23:21Z",
					"InstanceProfileArn" : "arn:aws:iam::1234:instance-profile/bastion-role-user",
					"InstanceProfileId" : "AIPARHU"
				}`), nil
			},
		},
	}

	err := c.Prompts()

	assert.Error(t, err)
	secretsKeyFile := "/hab/a2_deploy_workspace/secrets.key"
	secretsStoreFile := "/hab/a2_deploy_workspace/secrets.json"
	architecture := "aws"
	workspacePath := "/hab/a2_deploy_workspace"

	assert.Equal(t, secretsKeyFile, c.Config.Architecture.Aws.SecretsKeyFile)
	assert.Equal(t, secretsStoreFile, c.Config.Architecture.Aws.SecretsStoreFile)
	assert.Equal(t, architecture, c.Config.Architecture.Aws.Architecture)
	assert.Equal(t, workspacePath, c.Config.Architecture.Aws.WorkspacePath)
}

func callInput(upto int, b *bytes.Buffer) {
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
	if upto >= 5 {
		input(b, moveDown+"\r")
	}
	if upto >= 6 {
		input(b, "\r")
	}
	if upto >= 7 {
		input(b, "asia"+moveDown+"\r")
	}
	if upto >= 8 {
		input(b, "vpc-123"+"\r")
	}
	if upto >= 9 {
		input(b, "\r")
	}
	if upto >= 10 {
		input(b, "172.168.192.0"+"\r")
	}
	if upto >= 11 {
		input(b, "my-key-pair"+"\r")
	}
	if upto >= 12 {
		input(b, "ami-123"+"\r")
	}
	if upto >= 13 {
		input(b, "\r")
	}
	if upto >= 14 {
		input(b, "\r")
	}
	if upto >= 15 {
		input(b, "A2-automate.amazonaws.com"+"\r")
	}
	if upto >= 16 {
		input(b, "arn-123"+"\r")
	}
	if upto >= 17 {
		input(b, "automateFqdnRootCa.pem"+"\r")
	}
	if upto >= 18 {
		input(b, "admin12345"+"\r")
	}
	if upto >= 19 {
		input(b, fmt.Sprint(1)+"\r")
	}
	if upto >= 20 {
		input(b, "\r")
	}
	if upto >= 21 {
		input(b, "\r")
	}
	if upto >= 22 {
		input(b, "\r")
	}
	if upto >= 23 {
		input(b, "\r")
	}
	if upto >= 24 {
		input(b, "chef-infra-server.amazonaws.com"+"\r")
	}
	if upto >= 25 {
		input(b, "arn-124"+"\r")
	}
	if upto >= 26 {
		input(b, "chefInfraServerFqdnRootCa.pem"+"\r")
	}
	if upto >= 27 {
		input(b, fmt.Sprint(1)+"\r")
	}
	if upto >= 28 {
		input(b, "\r")
	}
	if upto >= 29 {
		input(b, "\r")
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
		input(b, "opensearch-automate"+"\r")
	}
	if upto >= 34 {
		input(b, "opensearch.automate.com"+"\r")
	}
	if upto >= 35 {
		input(b, "admin"+"\r")
	}
	if upto >= 36 {
		input(b, "admin@1234"+"\r")
	}
	if upto >= 37 {
		input(b, "\r")
	}
	if upto >= 38 {
		input(b, "arn-os-snapshot-role"+"\r")
	}
	if upto >= 39 {
		input(b, "ASKEIDSKFLDKDFJERKDS"+"\r")
	}
	if upto >= 40 {
		input(b, "asdfelkjsldfklsdjflsdfjdklsfjsdfsldkfjsd"+"\r")
	}
	if upto >= 41 {
		input(b, "pg.automate.com:5432"+"\r")
	}
	if upto >= 42 {
		input(b, "root"+"\r")
	}
	if upto >= 43 {
		input(b, "root1234"+"\r")
	}
	if upto >= 44 {
		input(b, "dbUser"+"\r")
	}
	if upto >= 45 {
		input(b, "dbUser1234"+"\r")
	}
	if upto >= 46 {
		input(b, "\r")
	}
	if upto >= 47 {
		input(b, "\r")
	}
	if upto >= 48 {
		input(b, "automate-backup"+"\r")
	}
}

func TestPromptCidr(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")
	priSub3 := "subnet-pri-3"
	input(b, priSub3+"\r")

	input(b, "\r")
	pubSub1 := "subnet-pub-1"
	input(b, pubSub1+"\r")
	pubSub2 := "subnet-pub-2"
	input(b, pubSub2+"\r")
	pubSub3 := "subnet-pub-3"
	input(b, pubSub3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.NoError(t, err)
	assert.Equal(t, 3, len(c.Config.Aws.Config.PrivateCustomSubnets))
	assert.Equal(t, priSub1, c.Config.Aws.Config.PrivateCustomSubnets[0])
	assert.Equal(t, priSub2, c.Config.Aws.Config.PrivateCustomSubnets[1])
	assert.Equal(t, priSub3, c.Config.Aws.Config.PrivateCustomSubnets[2])

	assert.Equal(t, 3, len(c.Config.Aws.Config.PublicCustomSubnets))
	assert.Equal(t, pubSub1, c.Config.Aws.Config.PublicCustomSubnets[0])
	assert.Equal(t, pubSub2, c.Config.Aws.Config.PublicCustomSubnets[1])
	assert.Equal(t, pubSub3, c.Config.Aws.Config.PublicCustomSubnets[2])
}

func TestPromptCidrErr1(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr2(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr3(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr4(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr5(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")
	priSub3 := "subnet-pri-3"
	input(b, priSub3+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr6(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")
	priSub3 := "subnet-pri-3"
	input(b, priSub3+"\r")

	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr7(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")
	priSub3 := "subnet-pri-3"
	input(b, priSub3+"\r")

	input(b, "\r")
	pubSub1 := "subnet-pub-1"
	input(b, pubSub1+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestPromptCidrErr8(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")
	priSub1 := "subnet-pri-1"
	input(b, priSub1+"\r")
	priSub2 := "subnet-pri-2"
	input(b, priSub2+"\r")
	priSub3 := "subnet-pri-3"
	input(b, priSub3+"\r")

	input(b, "\r")
	pubSub1 := "subnet-pub-1"
	input(b, pubSub1+"\r")
	pubSub2 := "subnet-pub-2"
	input(b, pubSub2+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfig{
		Prompt:    p,
		FileUtils: mfsu,
		Config:    &config.HaDeployConfig{},
	}

	err := c.PromptCidr()

	assert.Error(t, err)
}

func TestAwsHaProvisionConfigFactory(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	c := AwsHaProvisionConfigFactory(p)

	assert.NotNil(t, c)
}

func TestSetDefaultValuesForDBNodes(t *testing.T) {
	defaultInstanceCount := "0"
	c := AwsHaProvisionConfig{
		Config: &config.HaDeployConfig{},
	}
	c.SetDefaultValuesForDBNodes()

	assert.Equal(t, defaultInstanceCount, c.Config.Postgresql.Config.InstanceCount)
	assert.Equal(t, defaultInstanceCount, c.Config.Opensearch.Config.InstanceCount)
	assert.Equal(t, false, c.Config.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, false, c.Config.Opensearch.Config.EnableCustomCerts)
}
