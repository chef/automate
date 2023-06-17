package main

import (
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/stretchr/testify/assert"
)

// func TestPopulateHaCommonConfig(t *testing.T) {
// 	// Mocking dependencies
// 	getAutomateHAInfraDetails := func() (*AutomateHAInfraDetails, error) {
// 		// Mocked implementation that returns sample infra details or an error
// 		// Modify this function according to your test case scenario
// 		return &AutomateHAInfraDetails{
// 			// Populate the struct fields with sample data
// 		}, nil
// 	}

// 	fetchInfraConfig := func() (*ExistingInfraConfigToml, error) {
// 		// Mocked implementation that returns sample existing infra config or an error
// 		// Modify this function according to your test case scenario
// 		return &ExistingInfraConfigToml{
// 			// Sample existing infra config fields
// 		}, nil
// 	}

// 	fetchAwsConfig := func() (*AwsConfigToml, error) {
// 		// Mocked implementation that returns sample AWS config or an error
// 		// Modify this function according to your test case scenario
// 		return &AwsConfigToml{
// 			// Sample AWS config fields
// 		}, nil
// 	}

// 	// Replace the original functions with the mocked implementations
// 	originalGetAutomateHAInfraDetails := getAutomateHAInfraDetails
// 	originalFetchInfraConfig := fetchInfraConfig
// 	originalFetchAwsConfig := fetchAwsConfig

// 	sshConfig := &SSHConfig{
// 		sshUser:    infra.Outputs.SSHUser.Value,
// 		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
// 		sshPort:    infra.Outputs.SSHPort.Value,
// 	}
// 	sshUtil := NewSSHUtil(sshConfig)
// 	configPuller := NewPullConfigs(infra, sshUtil)
// 	defer func() {
// 		getAutomateHAInfraDetails = originalGetAutomateHAInfraDetails
// 		configPuller.fetchInfraConfig = originalFetchInfraConfig
// 		configPuller.fetchAwsConfig = originalFetchAwsConfig
// 	}()

// 	// Mocked function to override the implementation of NewSSHUtil
// 	newSSHUtil := func(config *config.SSHConfig) sshUtilInterface {
// 		// Mocked implementation that returns a dummy SSHUtil or a custom mock
// 		// Modify this function according to your test case scenario
// 		return &dummySSHUtil{}
// 	}

// 	// Mocked function to override the implementation of CopyExistingInfra
// 	copyExistingInfra := func(existingInfraConfig *config.ExistingInfraConfig) *config.HaDeployConfig {
// 		// Mocked implementation that returns a dummy HaDeployConfig or a custom mock
// 		// Modify this function according to your test case scenario
// 		return &config.HaDeployConfig{}
// 	}

// 	// Mocked function to override the implementation of CopyAws
// 	copyAws := func(awsConfig *config.AwsConfig) *config.HaDeployConfig {
// 		// Mocked implementation that returns a dummy HaDeployConfig or a custom mock
// 		// Modify this function according to your test case scenario
// 		return &config.HaDeployConfig{}
// 	}

// 	// Replace the original functions with the mocked implementations
// 	originalNewSSHUtil := newSSHUtil
// 	originalCopyExistingInfra := copyExistingInfra
// 	originalCopyAws := copyAws
// 	defer func() {
// 		NewSSHUtil = originalNewSSHUtil
// 		CopyExistingInfra = originalCopyExistingInfra
// 		CopyAws = originalCopyAws
// 	}()

// 	// Assign the mocked functions to the package-level variables
// 	NewSSHUtil = newSSHUtil
// 	CopyExistingInfra = copyExistingInfra
// 	CopyAws = copyAws

// 	// Test case: Existing Infra Config
// 	getAutomateHAInfraDetails = func() (*infraDetails, error) {
// 		// Mocked implementation that returns sample infra details or an error
// 		return &infraDetails{}, nil
// 	}

// 	configPuller.fetchInfraConfig = fetchInfraConfig

// 	haDeployConfig, err := PopulateHaCommonConfig()
// 	assert.Nil(t, err)
// 	assert.NotNil(t, haDeployConfig)
// 	// Add assertions to verify the populated HaDeployConfig for the existing infra config scenario

// 	// Test case: AWS Config
// 	getAutomateHAInfraDetails = func() (*infraDetails, error) {
// 		// Mocked implementation that returns sample infra details or an error
// 		return &infraDetails{}, nil
// 	}

// 	configPuller.fetchAwsConfig = fetchAwsConfig

// 	haDeployConfig, err = PopulateHaCommonConfig()
// 	assert.Nil(t, err)
// 	assert.NotNil(t, haDeployConfig)
// 	// Add assertions to verify the populated HaDeployConfig for the AWS config scenario

// 	// Test case: Deployed config not found
// 	getAutomateHAInfraDetails = func() (*infraDetails, error) {
// 		// Mocked implementation that returns an error indicating infra details not found
// 		return nil, errors.New("infra details not found")
// 	}

// 	haDeployConfig, err = PopulateHaCommonConfig()
// 	assert.NotNil(t, err)
// 	assert.Nil(t, haDeployConfig)
// 	// Add assertions to verify the error message and HaDeployConfig is nil for the deployed config not found scenario
// }

func TestCopyCertsByIP(t *testing.T) {
	// Prepare test data
	existing := []CertByIP{
		{
			IP:         "10.0.0.1",
			PrivateKey: "/existing/private/key1",
			PublicKey:  "/existing/public/key1",
			NodesDn:    "/existing/nodes/dn1",
		},
		{
			IP:         "10.0.0.2",
			PrivateKey: "/existing/private/key2",
			PublicKey:  "/existing/public/key2",
			NodesDn:    "/existing/nodes/dn2",
		},
	}

	haDeploy := &[]config.CertByIP{}

	// Call the function
	CopyCertsByIP(haDeploy, existing)

	// Assert the values in haDeploy
	assert.Len(t, *haDeploy, 2)

	// Assert the copied values for the first CertByIP
	assert.Equal(t, "10.0.0.1", (*haDeploy)[0].IP)
	assert.Equal(t, "/existing/private/key1", (*haDeploy)[0].PrivateKey)
	assert.Equal(t, "/existing/public/key1", (*haDeploy)[0].PublicKey)
	assert.Equal(t, "/existing/nodes/dn1", (*haDeploy)[0].NodesDn)

	// Assert the copied values for the second CertByIP
	assert.Equal(t, "10.0.0.2", (*haDeploy)[1].IP)
	assert.Equal(t, "/existing/private/key2", (*haDeploy)[1].PrivateKey)
	assert.Equal(t, "/existing/public/key2", (*haDeploy)[1].PublicKey)
	assert.Equal(t, "/existing/nodes/dn2", (*haDeploy)[1].NodesDn)
}

func TestCopyExistingInfra(t *testing.T) {
	existingInfraConfig := &ExistingInfraConfigToml{
		Architecture: ExistingInfraArchitectureToml{
			ConfigInitials: ExistingInfraConfigInitialsToml{
				SSHUser:                     "existing-ssh-user",
				Architecture:                "existing-architecture",
				BackupConfig:                "object_storage",
				BackupMount:                 "/existing/backup/mount",
				HabitatUIDGid:               "existing-habitat-uid-gid",
				LoggingMonitoringManagement: "true",
				SSHGroupName:                "existing-ssh-group",
				SSHKeyFile:                  "/existing/ssh/key/file",
				SSHPort:                     "22",
				SecretsKeyFile:              "/existing/secrets/key/file",
				SecretsStoreFile:            "/existing/secrets/store/file",
				WorkspacePath:               "/existing/workspace/path",
			},
		},
		Automate: ExistingInfraAutomateToml{
			Config: ExistingInfraAutomateConfigToml{
				AdminPassword:     "existing-admin-password",
				ConfigFile:        "/existing/config/file",
				EnableCustomCerts: true,
				Fqdn:              "existing-fqdn",
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				TeamsPort:         "8080",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
		ChefServer: ChefServerToml{
			Config: ChefServerConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
		Postgresql: PostgresqlToml{
			Config: PgConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
		Opensearch: OpensearchToml{
			Config: OsConfigToml{
				AdminCert:         "/existing/admin/cert",
				AdminDn:           "/existing/admin/dn",
				AdminKey:          "/existing/admin/key",
				EnableCustomCerts: true,
				InstanceCount:     "3",
				NodesDn:           "/existing/nodes/dn",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
				CertsByIP: []CertByIP{
					{
						IP:         "10.0.0.1",
						PrivateKey: "/existing/cert1/private/key",
						PublicKey:  "/existing/cert1/public/key",
						NodesDn:    "/existing/cert1/nodes/dn",
					},
				},
			},
		},
		ExistingInfra: ExistingInfraToml{
			Config: ExistingInfraIpsToml{
				AutomatePrivateIps:   []string{"10.0.0.1", "10.0.0.2"},
				ChefServerPrivateIps: []string{"10.0.1.1", "10.0.1.2"},
				PostgresqlPrivateIps: []string{"10.0.2.1", "10.0.2.2"},
				OpensearchPrivateIps: []string{"10.0.3.1", "10.0.3.2"},
			},
		},
		ExternalDB: ExternalDBToml{
			Database: ExternalDbToml{
				Type: "aws",
				Opensearch: ExternalOpensearchToml{
					OpensearchRootCert:          "root-cert",
					OpensearchDomainName:        "domain",
					OpensearchInstanceURL:       "instance-url",
					OpensearchSuperUserPassword: "superuser-password",
					OpensearchSuperUserName:     "superuser-name",
					AWS:                         ExternalAwsToml{AwsOsSnapshotRoleArn: "snapshot-role-arn", OsUserAccessKeyId: "access-key-id", OsUserAccessKeySecret: "access-key-secret"},
				},
				PostgreSQL: ExternalPostgreSQLToml{
					PostgreSQLDBUserPassword:    "dbuser-password",
					PostgreSQLDBUserName:        "dbuser-name",
					PostgreSQLInstanceURL:       "instance-url",
					PostgreSQLRootCert:          "root-cert",
					PostgreSQLSuperUserPassword: "superuser-password",
					PostgreSQLSuperUserName:     "superuser-name",
				},
			},
		},
		ObjectStorage: ObjectStorageToml{
			Config: ObjectStorageConfigToml{
				AccessKey:  "existing-access-key",
				BucketName: "existing-bucket",
				Endpoint:   "existing-endpoint",
				Region:     "existing-region",
				SecretKey:  "existing-secret-key",
			},
		},
	}
	haDeployConfig := CopyExistingInfra(existingInfraConfig)

	assert.Equal(t, "existing-ssh-user", haDeployConfig.Architecture.ExistingInfra.SSHUser)
	assert.Equal(t, "existing-architecture", haDeployConfig.Architecture.ExistingInfra.Architecture)
	assert.Equal(t, "object_storage", haDeployConfig.Architecture.ExistingInfra.BackupConfig)
	assert.Equal(t, "/existing/backup/mount", haDeployConfig.Architecture.ExistingInfra.BackupMount)
	assert.Equal(t, "existing-habitat-uid-gid", haDeployConfig.Architecture.ExistingInfra.HabitatUIDGid)
	assert.Equal(t, "true", haDeployConfig.Architecture.ExistingInfra.LoggingMonitoringManagement)
	assert.Equal(t, "existing-ssh-group", haDeployConfig.Architecture.ExistingInfra.SSHGroupName)
	assert.Equal(t, "/existing/ssh/key/file", haDeployConfig.Architecture.ExistingInfra.SSHKeyFile)
	assert.Equal(t, "22", haDeployConfig.Architecture.ExistingInfra.SSHPort)
	assert.Equal(t, "/existing/secrets/key/file", haDeployConfig.Architecture.ExistingInfra.SecretsKeyFile)
	assert.Equal(t, "/existing/secrets/store/file", haDeployConfig.Architecture.ExistingInfra.SecretsStoreFile)
	assert.Equal(t, "/existing/workspace/path", haDeployConfig.Architecture.ExistingInfra.WorkspacePath)

	assert.Equal(t, "existing-admin-password", haDeployConfig.Automate.Config.AdminPassword)
	assert.Equal(t, "/existing/config/file", haDeployConfig.Automate.Config.ConfigFile)
	assert.Equal(t, true, haDeployConfig.Automate.Config.EnableCustomCerts)
	assert.Equal(t, "existing-fqdn", haDeployConfig.Automate.Config.Fqdn)
	assert.Equal(t, "3", haDeployConfig.Automate.Config.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployConfig.Automate.Config.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfig.Automate.Config.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployConfig.Automate.Config.RootCA)
	assert.Equal(t, "8080", haDeployConfig.Automate.Config.TeamsPort)
	assert.Equal(t, "10.0.0.1", (*haDeployConfig.Automate.Config.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfig.Automate.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfig.Automate.Config.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfig.Automate.Config.CertsByIP)[0].NodesDn)

	assert.Equal(t, true, haDeployConfig.ChefServer.Config.EnableCustomCerts)
	assert.Equal(t, "3", haDeployConfig.ChefServer.Config.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployConfig.ChefServer.Config.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfig.ChefServer.Config.PublicKey)
	assert.Equal(t, "10.0.0.1", (*haDeployConfig.ChefServer.Config.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfig.ChefServer.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfig.ChefServer.Config.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfig.ChefServer.Config.CertsByIP)[0].NodesDn)

	assert.Equal(t, true, haDeployConfig.Postgresql.Config.EnableCustomCerts)
	assert.Equal(t, "3", haDeployConfig.Postgresql.Config.InstanceCount)
	assert.Equal(t, "/existing/private/key", haDeployConfig.Postgresql.Config.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfig.Postgresql.Config.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployConfig.Postgresql.Config.RootCA)
	assert.Equal(t, "10.0.0.1", (*haDeployConfig.Postgresql.Config.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfig.Postgresql.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfig.Postgresql.Config.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfig.Postgresql.Config.CertsByIP)[0].NodesDn)

	assert.Equal(t, "/existing/admin/cert", haDeployConfig.Opensearch.Config.AdminCert)
	assert.Equal(t, "/existing/admin/dn", haDeployConfig.Opensearch.Config.AdminDn)
	assert.Equal(t, "/existing/admin/key", haDeployConfig.Opensearch.Config.AdminKey)
	assert.Equal(t, true, haDeployConfig.Opensearch.Config.EnableCustomCerts)
	assert.Equal(t, "3", haDeployConfig.Opensearch.Config.InstanceCount)
	assert.Equal(t, "/existing/nodes/dn", haDeployConfig.Opensearch.Config.NodesDn)
	assert.Equal(t, "/existing/private/key", haDeployConfig.Opensearch.Config.PrivateKey)
	assert.Equal(t, "/existing/public/key", haDeployConfig.Opensearch.Config.PublicKey)
	assert.Equal(t, "/existing/root/ca", haDeployConfig.Opensearch.Config.RootCA)
	assert.Equal(t, "10.0.0.1", (*haDeployConfig.Opensearch.Config.CertsByIP)[0].IP)
	assert.Equal(t, "/existing/cert1/private/key", (*haDeployConfig.Opensearch.Config.CertsByIP)[0].PrivateKey)
	assert.Equal(t, "/existing/cert1/public/key", (*haDeployConfig.Opensearch.Config.CertsByIP)[0].PublicKey)
	assert.Equal(t, "/existing/cert1/nodes/dn", (*haDeployConfig.Opensearch.Config.CertsByIP)[0].NodesDn)

	assert.Equal(t, []string{"10.0.0.1", "10.0.0.2"}, haDeployConfig.ExistingInfra.Config.AutomatePrivateIps)
	assert.Equal(t, []string{"10.0.1.1", "10.0.1.2"}, haDeployConfig.ExistingInfra.Config.ChefServerPrivateIps)
	assert.Equal(t, []string{"10.0.2.1", "10.0.2.2"}, haDeployConfig.ExistingInfra.Config.PostgresqlPrivateIps)
	assert.Equal(t, []string{"10.0.3.1", "10.0.3.2"}, haDeployConfig.ExistingInfra.Config.OpensearchPrivateIps)

	assert.Equal(t, "aws", haDeployConfig.External.Database.Type)
	assert.Equal(t, "root-cert", haDeployConfig.External.Database.OpenSearch.OpensearchRootCert)
	assert.Equal(t, "domain", haDeployConfig.External.Database.OpenSearch.OpensearchDomainName)
	assert.Equal(t, "instance-url", haDeployConfig.External.Database.OpenSearch.OpensearchDomainURL)
	assert.Equal(t, "superuser-password", haDeployConfig.External.Database.OpenSearch.OpensearchUserPassword)
	assert.Equal(t, "superuser-name", haDeployConfig.External.Database.OpenSearch.OpensearchUsername)
	assert.Equal(t, "snapshot-role-arn", haDeployConfig.External.Database.OpenSearch.Aws.AwsOsSnapshotRoleArn)
	assert.Equal(t, "access-key-id", haDeployConfig.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeyID)
	assert.Equal(t, "access-key-secret", haDeployConfig.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeySecret)
	assert.Equal(t, "dbuser-password", haDeployConfig.External.Database.PostgreSQL.DbuserPassword)
	assert.Equal(t, "dbuser-name", haDeployConfig.External.Database.PostgreSQL.DbuserUsername)
	assert.Equal(t, "instance-url", haDeployConfig.External.Database.PostgreSQL.InstanceURL)
	assert.Equal(t, "root-cert", haDeployConfig.External.Database.PostgreSQL.PostgresqlRootCert)
	assert.Equal(t, "superuser-password", haDeployConfig.External.Database.PostgreSQL.SuperuserPassword)
	assert.Equal(t, "superuser-name", haDeployConfig.External.Database.PostgreSQL.SuperuserUsername)

	assert.Equal(t, "existing-access-key", haDeployConfig.ObjectStorage.Config.AccessKey)
	assert.Equal(t, "existing-bucket", haDeployConfig.ObjectStorage.Config.BucketName)
	assert.Equal(t, "existing-endpoint", haDeployConfig.ObjectStorage.Config.Endpoint)
	assert.Equal(t, "existing-region", haDeployConfig.ObjectStorage.Config.Region)
	assert.Equal(t, "existing-secret-key", haDeployConfig.ObjectStorage.Config.SecretKey)

}

func TestCopyAws(t *testing.T) {
	awsConfig := &AwsConfigToml{
		Architecture: AwsArchitectureToml{
			ConfigInitials: AwsConfigInitialsToml{
				SSHUser:                     "aws-ssh-user",
				Architecture:                "aws-architecture",
				BackupConfig:                "s3",
				S3BucketName:                "aws-s3-bucket",
				BackupMount:                 "/aws/backup/mount",
				HabitatUIDGid:               "aws-habitat-uid-gid",
				LoggingMonitoringManagement: "false",
				SSHGroupName:                "aws-ssh-group",
				SSHKeyFile:                  "/aws/ssh/key/file",
				SSHPort:                     "2222",
				SecretsKeyFile:              "/aws/secrets/key/file",
				SecretsStoreFile:            "/aws/secrets/store/file",
				WorkspacePath:               "/aws/workspace/path",
			},
		},
		Automate: AwsAutomateToml{
			Config: AwsAutomateConfigToml{
				AdminPassword:     "aws-admin-password",
				ConfigFile:        "/aws/config/file",
				EnableCustomCerts: false,
				Fqdn:              "aws-fqdn",
				InstanceCount:     "5",
				PrivateKey:        "/aws/private/key",
				PublicKey:         "/aws/public/key",
				RootCA:            "/aws/root/ca",
				TeamsPort:         "9090",
			},
		},
		ChefServer: ChefServerToml{
			Config: ChefServerConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
			},
		},
		Postgresql: PostgresqlToml{
			Config: PgConfigToml{
				EnableCustomCerts: true,
				InstanceCount:     "3",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
			},
		},
		Opensearch: OpensearchToml{
			Config: OsConfigToml{
				AdminCert:         "/existing/admin/cert",
				AdminDn:           "/existing/admin/dn",
				AdminKey:          "/existing/admin/key",
				EnableCustomCerts: true,
				InstanceCount:     "3",
				NodesDn:           "/existing/nodes/dn",
				PrivateKey:        "/existing/private/key",
				PublicKey:         "/existing/public/key",
				RootCA:            "/existing/root/ca",
			},
		},
		Aws: AwsToml{
			Config: ConfigToml{
				Profile:                      "my-profile",
				Region:                       "us-west-2",
				AwsVpcId:                     "vpc-12345",
				AwsCidrBlockAddr:             "10.0.0.0/16",
				PrivateCustomSubnets:         []string{"subnet-12345", "subnet-67890"},
				PublicCustomSubnets:          []string{"subnet-54321", "subnet-09876"},
				SSHKeyPairName:               "my-keypair",
				AmiID:                        "ami-08d4ac5b634553e16",
				DeleteOnTermination:          true,
				SetupManagedServices:         true,
				AutomateServerInstanceType:   "t3.medium",
				ChefServerInstanceType:       "t3.medium",
				PostgresqlServerInstanceType: "m5.large",
				OpensearchServerInstanceType: "m5.large",
				AutomateLbCertificateArn:     "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				ChefServerLbCertificateArn:   "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e",
				AutomateEbsVolumeIops:        "100",
				AutomateEbsVolumeSize:        "50",
				AutomateEbsVolumeType:        "gp3",
				ChefEbsVolumeIops:            "100",
				ChefEbsVolumeSize:            "50",
				ChefEbsVolumeType:            "gp3",
				OpensearchEbsVolumeIops:      "100",
				OpensearchEbsVolumeSize:      "50",
				OpensearchEbsVolumeType:      "gp3",
				PostgresqlEbsVolumeIops:      "100",
				PostgresqlEbsVolumeSize:      "50",
				PostgresqlEbsVolumeType:      "gp3",
				LBAccessLogs:                 "false",
				OpensearchDomainName:         "opensearch-domain",
				OpensearchDomainUrl:          "opensearch-url",
				OpensearchUserPassword:       "opensearch-password",
				OpensearchUsername:           "opensearch-username",
				OpensearchCertificate:        "opensearch-certificate",
				OsUserAccessKeyId:            "access-key-id",
				OsUserAccessKeySecret:        "access-key-secret",
				AwsOsSnapshotRoleArn:         "snapshot-role-arn",
				RDSCertificate:               "rds-certificate",
				RDSDBUserPassword:            "rds-db-password",
				RDSDBUserName:                "rds-db-username",
				RDSInstanceUrl:               "rds-instance-url",
				RDSSuperUserPassword:         "rds-superuser-password",
				RDSSuperUserName:             "rds-superuser-username",
			},
		},
	}

	haDeployConfig := CopyAws(awsConfig)

	// Verify the copied values in haDeployConfig
	arch := haDeployConfig.Architecture
	assert.Equal(t, "aws-ssh-user", arch.Aws.SSHUser)
	assert.Equal(t, "aws-architecture", arch.Aws.Architecture)
	assert.Equal(t, "s3", arch.Aws.BackupConfig)
	assert.Equal(t, "aws-s3-bucket", arch.Aws.S3BucketName)
	assert.Equal(t, "/aws/backup/mount", arch.Aws.BackupMount)
	assert.Equal(t, "aws-habitat-uid-gid", arch.Aws.HabitatUIDGid)
	assert.Equal(t, "false", arch.Aws.LoggingMonitoringManagement)
	assert.Equal(t, "aws-ssh-group", arch.Aws.SSHGroupName)
	assert.Equal(t, "/aws/ssh/key/file", arch.Aws.SSHKeyFile)
	assert.Equal(t, "2222", arch.Aws.SSHPort)
	assert.Equal(t, "/aws/secrets/key/file", arch.Aws.SecretsKeyFile)
	assert.Equal(t, "/aws/secrets/store/file", arch.Aws.SecretsStoreFile)
	assert.Equal(t, "/aws/workspace/path", arch.Aws.WorkspacePath)

	automate := haDeployConfig.Automate.Config
	assert.Equal(t, "aws-admin-password", automate.AdminPassword)
	assert.Equal(t, "/aws/config/file", automate.ConfigFile)
	assert.Equal(t, false, automate.EnableCustomCerts)
	assert.Equal(t, "aws-fqdn", automate.Fqdn)
	assert.Equal(t, "5", automate.InstanceCount)
	assert.Equal(t, "/aws/private/key", automate.PrivateKey)
	assert.Equal(t, "/aws/public/key", automate.PublicKey)
	assert.Equal(t, "/aws/root/ca", automate.RootCA)
	assert.Equal(t, "9090", automate.TeamsPort)

	chefServer := haDeployConfig.ChefServer.Config
	assert.Equal(t, true, chefServer.EnableCustomCerts)
	assert.Equal(t, "3", chefServer.InstanceCount)
	assert.Equal(t, "/existing/private/key", chefServer.PrivateKey)
	assert.Equal(t, "/existing/public/key", chefServer.PublicKey)

	postgresql := haDeployConfig.Postgresql.Config
	assert.Equal(t, true, postgresql.EnableCustomCerts)
	assert.Equal(t, "3", postgresql.InstanceCount)
	assert.Equal(t, "/existing/private/key", postgresql.PrivateKey)
	assert.Equal(t, "/existing/public/key", postgresql.PublicKey)
	assert.Equal(t, "/existing/root/ca", postgresql.RootCA)

	opensearch := haDeployConfig.Opensearch.Config
	assert.Equal(t, "/existing/admin/cert", opensearch.AdminCert)
	assert.Equal(t, "/existing/admin/dn", opensearch.AdminDn)
	assert.Equal(t, "/existing/admin/key", opensearch.AdminKey)
	assert.Equal(t, true, opensearch.EnableCustomCerts)
	assert.Equal(t, "3", opensearch.InstanceCount)
	assert.Equal(t, "/existing/nodes/dn", opensearch.NodesDn)
	assert.Equal(t, "/existing/private/key", opensearch.PrivateKey)
	assert.Equal(t, "/existing/public/key", opensearch.PublicKey)
	assert.Equal(t, "/existing/root/ca", opensearch.RootCA)

	aws := haDeployConfig.Aws.Config
	assert.Equal(t, "my-profile", aws.Profile)
	assert.Equal(t, "us-west-2", aws.Region)
	assert.Equal(t, "vpc-12345", aws.AwsVpcID)
	assert.Equal(t, "10.0.0.0/16", aws.AwsCidrBlockAddr)
	assert.Equal(t, []string{"subnet-12345", "subnet-67890"}, aws.PrivateCustomSubnets)
	assert.Equal(t, []string{"subnet-54321", "subnet-09876"}, aws.PublicCustomSubnets)
	assert.Equal(t, "my-keypair", aws.SSHKeyPairName)
	assert.Equal(t, "ami-08d4ac5b634553e16", aws.AmiID)
	assert.Equal(t, true, aws.DeleteOnTermination)
	assert.Equal(t, "t3.medium", aws.AutomateServerInstanceType)
	assert.Equal(t, "t3.medium", aws.ChefServerInstanceType)
	assert.Equal(t, "m5.large", aws.PostgresqlServerInstanceType)
	assert.Equal(t, "m5.large", aws.OpensearchServerInstanceType)
	assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.AutomateLbCertificateArn)
	assert.Equal(t, "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e", aws.ChefServerLbCertificateArn)
	assert.Equal(t, "100", aws.AutomateEbsVolumeIops)
	assert.Equal(t, "50", aws.AutomateEbsVolumeSize)
	assert.Equal(t, "gp3", aws.AutomateEbsVolumeType)
	assert.Equal(t, "100", aws.ChefEbsVolumeIops)
	assert.Equal(t, "50", aws.ChefEbsVolumeSize)
	assert.Equal(t, "gp3", aws.ChefEbsVolumeType)
	assert.Equal(t, "100", aws.OpensearchEbsVolumeIops)
	assert.Equal(t, "50", aws.OpensearchEbsVolumeSize)
	assert.Equal(t, "gp3", aws.OpensearchEbsVolumeType)
	assert.Equal(t, "100", aws.PostgresqlEbsVolumeIops)
	assert.Equal(t, "50", aws.PostgresqlEbsVolumeSize)
	assert.Equal(t, "gp3", aws.PostgresqlEbsVolumeType)
	assert.Equal(t, "false", aws.LbAccessLogs)
	assert.Equal(t, "opensearch-domain", aws.ManagedOpensearchDomainName)
	assert.Equal(t, "opensearch-url", aws.ManagedOpensearchDomainURL)
	assert.Equal(t, "opensearch-password", aws.ManagedOpensearchUserPassword)
	assert.Equal(t, "opensearch-username", aws.ManagedOpensearchUsername)
	assert.Equal(t, "opensearch-certificate", aws.ManagedOpensearchCertificate)
	assert.Equal(t, "access-key-id", aws.OsSnapshotUserAccessKeyID)
	assert.Equal(t, "access-key-secret", aws.OsSnapshotUserAccessKeySecret)
	assert.Equal(t, "snapshot-role-arn", aws.AwsOsSnapshotRoleArn)
	assert.Equal(t, "rds-certificate", aws.ManagedRdsCertificate)
	assert.Equal(t, "rds-db-password", aws.ManagedRdsDbuserPassword)
	assert.Equal(t, "rds-db-username", aws.ManagedRdsDbuserUsername)
	assert.Equal(t, "rds-instance-url", aws.ManagedRdsInstanceURL)
	assert.Equal(t, "rds-superuser-password", aws.ManagedRdsSuperuserPassword)
	assert.Equal(t, "rds-superuser-username", aws.ManagedRdsSuperuserUsername)
}
