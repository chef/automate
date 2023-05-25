package config_verify

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/config_parser"
)

const (
	LOCALHOST_IP      = "127.0.0.1"
	SSH_KEY_FILE_PATH = "./testdata/admin.pem"
)

func GetTestOnPremConfigData() *config_parser.HAOnPremConfigToml {
	configOnprem := &config_parser.HAOnPremConfigToml{}

	// ConfigInitials
	configOnprem.Architecture.ConfigInitials.SecretsKeyFile = "key.pem"
	configOnprem.Architecture.ConfigInitials.SecretsStoreFile = "s/store/file"
	configOnprem.Architecture.ConfigInitials.Architecture = "Architecture"
	configOnprem.Architecture.ConfigInitials.WorkspacePath = "/path/workspace"
	configOnprem.Architecture.ConfigInitials.SSHUser = "username"
	configOnprem.Architecture.ConfigInitials.SSHKeyFile = SSH_KEY_FILE_PATH
	configOnprem.Architecture.ConfigInitials.SSHPort = "24"
	configOnprem.Architecture.ConfigInitials.SudoPassword = "password"
	configOnprem.Architecture.ConfigInitials.LoggingMonitoringManagement = "logging_monitoring"
	configOnprem.Architecture.ConfigInitials.NewElk = "elk"
	configOnprem.Architecture.ConfigInitials.ExistingElkInstanceIP = "127.0.0.0"
	configOnprem.Architecture.ConfigInitials.ExistingElkPort = "9300"
	configOnprem.Architecture.ConfigInitials.ExistingElkCert = "/path/to/existing/elk/cert"
	configOnprem.Architecture.ConfigInitials.ExistingElkUsername = "elkusername"
	configOnprem.Architecture.ConfigInitials.ExistingElkPassword = "elkpassword"
	configOnprem.Architecture.ConfigInitials.BackupMount = "/mnt/automate_backups"
	configOnprem.Architecture.ConfigInitials.HabitatUIDGid = "uid_gid"
	configOnprem.Architecture.ConfigInitials.BackupConfig = "object_storage"
	configOnprem.ObjectStorage.Config.BucketName = "s3"

	// Automate Config
	configOnprem.Automate.Config.AdminPassword = "adminpassword"
	configOnprem.Automate.Config.Fqdn = "sampledomain.com"
	configOnprem.Automate.Config.InstanceCount = "1"
	configOnprem.Automate.Config.TeamsPort = "8080"
	configOnprem.Automate.Config.ConfigFile = "/path/to/config/file"
	configOnprem.Automate.Config.EnableCustomCerts = false
	configOnprem.Automate.Config.RootCA = "/path/to/root/ca/file"
	configOnprem.Automate.Config.PrivateKey = "/key"
	configOnprem.Automate.Config.PublicKey = "/path/key"
	configOnprem.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ChefServer Config
	configOnprem.ChefServer.Config.InstanceCount = "1"
	configOnprem.ChefServer.Config.EnableCustomCerts = false
	configOnprem.ChefServer.Config.PrivateKey = "/path/to/key"
	configOnprem.ChefServer.Config.PublicKey = "/key"
	configOnprem.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Opensearch Config
	configOnprem.Opensearch.Config.InstanceCount = "1"
	configOnprem.Opensearch.Config.EnableCustomCerts = false
	configOnprem.Opensearch.Config.RootCA = "/path/to/root/ca"
	configOnprem.Opensearch.Config.AdminCert = "/path/to/admin/cert"
	configOnprem.Opensearch.Config.AdminKey = "/path/to/admin/key"
	configOnprem.Opensearch.Config.PrivateKey = "/path/to/private/key"
	configOnprem.Opensearch.Config.PublicKey = "public/key"
	configOnprem.Opensearch.Config.AdminDn = "admin_dn"
	configOnprem.Opensearch.Config.NodesDn = "nodes_dn"
	configOnprem.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Postgresql Config
	configOnprem.Postgresql.Config.InstanceCount = "1"
	configOnprem.Postgresql.Config.EnableCustomCerts = false
	configOnprem.Postgresql.Config.RootCA = "/path/to/root/ca"
	configOnprem.Postgresql.Config.PrivateKey = "key"
	configOnprem.Postgresql.Config.PublicKey = "/path/to/public/key"
	configOnprem.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ExistingInfra Config
	configOnprem.ExistingInfra.Config.AutomatePrivateIps = []string{LOCALHOST_IP, LOCALHOST_IP}
	configOnprem.ExistingInfra.Config.ChefServerPrivateIps = []string{LOCALHOST_IP, LOCALHOST_IP}
	configOnprem.ExistingInfra.Config.OpensearchPrivateIps = []string{LOCALHOST_IP, LOCALHOST_IP}
	configOnprem.ExistingInfra.Config.PostgresqlPrivateIps = []string{LOCALHOST_IP, LOCALHOST_IP}

	// ExternalDB Config
	configOnprem.ExternalDB.Database.Type = ""

	// ObjectStorage Config
	configOnprem.ObjectStorage.Config.AccessKey = "sample-access-key"
	configOnprem.ObjectStorage.Config.SecretKey = "sample-secret-key"
	configOnprem.ObjectStorage.Config.Endpoint = "sample-endpoint"
	configOnprem.ObjectStorage.Config.Region = "sample-region"

	return configOnprem
}

func GetTestOnPremFailedConfigData() *config_parser.HAOnPremConfigToml {
	configOnpremFailed := GetTestOnPremConfigData()
	configOnpremFailed.Architecture.ConfigInitials.SSHKeyFile = "./pem"
	configOnpremFailed.Architecture.ConfigInitials.BackupMount = "/mnt/"
	configOnpremFailed.Automate.Config.Fqdn = ".com"
	configOnpremFailed.ChefServer.Config.EnableCustomCerts = true
	configOnpremFailed.ChefServer.Config.PrivateKey = ""
	configOnpremFailed.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "key", PublicKey: "key", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "key", PublicKey: "key", NodesDn: ""},
	}
	configOnpremFailed.Architecture.ConfigInitials.BackupConfig = "file_system"
	configOnpremFailed.ExistingInfra.Config.AutomatePrivateIps = []string{}
	configOnpremFailed.ExistingInfra.Config.ChefServerPrivateIps = []string{}
	configOnpremFailed.ExistingInfra.Config.OpensearchPrivateIps = []string{"11", "12.1"}
	configOnpremFailed.ExistingInfra.Config.PostgresqlPrivateIps = []string{"12.1", LOCALHOST_IP}

	configOnpremFailed.Architecture.ConfigInitials.SecretsStoreFile = ""
	configOnpremFailed.Architecture.ConfigInitials.Architecture = ""
	configOnpremFailed.Architecture.ConfigInitials.WorkspacePath = "34"
	configOnpremFailed.Automate.Config.EnableCustomCerts = true
	configOnpremFailed.Automate.Config.PublicKey = ""
	configOnpremFailed.Postgresql.Config.EnableCustomCerts = true
	configOnpremFailed.Postgresql.Config.PrivateKey = ""
	configOnpremFailed.Opensearch.Config.EnableCustomCerts = true
	configOnpremFailed.Opensearch.Config.RootCA = ""
	configOnpremFailed.Opensearch.Config.AdminCert = "ssf"
	configOnpremFailed.Opensearch.Config.AdminKey = ""
	configOnpremFailed.Opensearch.Config.PrivateKey = ""
	configOnpremFailed.Opensearch.Config.PublicKey = ""
	configOnpremFailed.Opensearch.Config.PublicKey = ""
	configOnpremFailed.Architecture.ConfigInitials.SSHUser = ""
	configOnpremFailed.ObjectStorage.Config.BucketName = "s4"

	return configOnpremFailed
}

func GetTestAWSConfigData() *config_parser.HAAwsConfigToml {
	configAws := &config_parser.HAAwsConfigToml{}

	configAws.Architecture.ConfigInitials.SecretsKeyFile = "/testdata/key.pem"
	configAws.Architecture.ConfigInitials.SecretsStoreFile = "/path/to/secrets_store_file"
	configAws.Architecture.ConfigInitials.Architecture = "HA"
	configAws.Architecture.ConfigInitials.WorkspacePath = "/path/to/workspace"
	configAws.Architecture.ConfigInitials.SSHUser = "username"
	configAws.Architecture.ConfigInitials.SSHKeyFile = SSH_KEY_FILE_PATH
	configAws.Architecture.ConfigInitials.SSHPort = "22"
	configAws.Architecture.ConfigInitials.SudoPassword = "password"
	configAws.Architecture.ConfigInitials.LoggingMonitoringManagement = "ELK"
	configAws.Architecture.ConfigInitials.NewElk = "true"
	configAws.Architecture.ConfigInitials.ExistingElkInstanceIP = "0.0.0.0"
	configAws.Architecture.ConfigInitials.ExistingElkPort = "9200"
	configAws.Architecture.ConfigInitials.ExistingElkCert = "/path/to/elk_cert"
	configAws.Architecture.ConfigInitials.ExistingElkUsername = "elk_user"
	configAws.Architecture.ConfigInitials.ExistingElkPassword = "elk_password"
	configAws.Architecture.ConfigInitials.BackupMount = "/mnt/automate_backups"
	configAws.Architecture.ConfigInitials.BackupConfig = "s3"
	configAws.Architecture.ConfigInitials.S3BucketName = "bucket_name"
	configAws.Architecture.ConfigInitials.HabitatUIDGid = "123:456"

	configAws.Automate.Config.AdminPassword = "admin_password"
	configAws.Automate.Config.Fqdn = "example.com"
	configAws.Automate.Config.AutomateSetupType = "tiered"
	configAws.Automate.Config.InstanceCount = "3"
	configAws.Automate.Config.TeamsPort = "8080"
	configAws.Automate.Config.ConfigFile = "/path/to/automate_config"
	configAws.Automate.Config.EnableCustomCerts = false
	configAws.Automate.Config.RootCA = "/path/to/root_ca"
	configAws.Automate.Config.PrivateKey = "/path/to/private_key"
	configAws.Automate.Config.PublicKey = "/path/to/public_key"
	configAws.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	configAws.ChefServer.Config.InstanceCount = "1"
	configAws.ChefServer.Config.EnableCustomCerts = false
	configAws.ChefServer.Config.PrivateKey = "/path/to/chef_private_key"
	configAws.ChefServer.Config.PublicKey = "/path/to/chef_public_key"
	configAws.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	configAws.Opensearch.Config.InstanceCount = "1"
	configAws.Opensearch.Config.EnableCustomCerts = false
	configAws.Opensearch.Config.RootCA = "/path/to/opensearch_root_ca"
	configAws.Opensearch.Config.AdminCert = "/path/to/admin_cert"
	configAws.Opensearch.Config.AdminKey = "/path/to/admin_key"
	configAws.Opensearch.Config.PrivateKey = "/path/to/opensearch_private_key"
	configAws.Opensearch.Config.PublicKey = "/path/to/opensearch_public_key"
	configAws.Opensearch.Config.AdminDn = "CN=admin"
	configAws.Opensearch.Config.NodesDn = "CN=nodes"
	configAws.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	configAws.Postgresql.Config.InstanceCount = "1"
	configAws.Postgresql.Config.EnableCustomCerts = false
	configAws.Postgresql.Config.RootCA = "/path/to/postgresql_root_ca"
	configAws.Postgresql.Config.PrivateKey = "/path/to/postgresql_private_key"
	configAws.Postgresql.Config.PublicKey = "/path/to/postgresql_public_key"
	configAws.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: LOCALHOST_IP, PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	configAws.Aws.Config.Profile = "my_aws_profile"
	configAws.Aws.Config.Region = "us-west-2"
	configAws.Aws.Config.AwsVpcId = "vpc-12345678"
	configAws.Aws.Config.AwsCidrBlockAddr = "0.0.0.0/00"
	configAws.Aws.Config.PrivateCustomSubnets = []string{"subnet-abcdef01", "subnet-abcdef02"}
	configAws.Aws.Config.PublicCustomSubnets = []string{"subnet-abcdef03", "subnet-abcdef04"}
	configAws.Aws.Config.SSHKeyPairName = "my_key_pair"
	configAws.Aws.Config.SetupManagedServices = false
	configAws.Aws.Config.OpensearchDomainName = "my-opensearch-domain"
	configAws.Aws.Config.OpensearchDomainUrl = "https://my-opensearch-domain.domain.com"
	configAws.Aws.Config.OpensearchUsername = "opensearch_user"
	configAws.Aws.Config.OpensearchUserPassword = "opensearch_password"
	configAws.Aws.Config.OpensearchCertificate = "/path/to/managed_opensearch_cert"
	configAws.Aws.Config.AwsOsSnapshotRoleArn = "arn:aws:iam::1234567890:role/my-os-snapshot-role"
	configAws.Aws.Config.OsUserAccessKeyId = "my_os_user_access_key_id"
	configAws.Aws.Config.OsUserAccessKeySecret = "my_os_user_access_key_secret"
	configAws.Aws.Config.RDSInstanceUrl = "https://my-rds-instance.domain.com"
	configAws.Aws.Config.RDSSuperUserName = "rds_superuser"
	configAws.Aws.Config.RDSSuperUserPassword = "rds_superuser_password"
	configAws.Aws.Config.RDSDBUserName = "rds_dbuser"
	configAws.Aws.Config.RDSDBUserPassword = "rds_dbuser_password"
	configAws.Aws.Config.RDSCertificate = "/path/to/managed_rds_cert"
	configAws.Aws.Config.AmiFilterName = "my-ami-filter"
	configAws.Aws.Config.AmiFilterVirtType = "hvm"
	configAws.Aws.Config.AmiFilterOwner = "self"
	configAws.Aws.Config.AmiID = "ami-12345678"
	configAws.Aws.Config.LBAccessLogs = "/path/to/lb_access_logs"
	configAws.Aws.Config.DeleteOnTermination = true
	configAws.Aws.Config.AutomateServerInstanceType = "t2.medium"
	configAws.Aws.Config.ChefServerInstanceType = "t2.large"
	configAws.Aws.Config.OpensearchServerInstanceType = "r5.large"
	configAws.Aws.Config.PostgresqlServerInstanceType = "db.m5.large"
	configAws.Aws.Config.AutomateLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/automate-lb-cert"
	configAws.Aws.Config.ChefServerLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/chef-server-lb-cert"
	configAws.Aws.Config.AutomateEbsVolumeIops = "100"
	configAws.Aws.Config.AutomateEbsVolumeSize = "100"
	configAws.Aws.Config.AutomateEbsVolumeType = "gp2"
	configAws.Aws.Config.ChefEbsVolumeIops = "200"
	configAws.Aws.Config.ChefEbsVolumeSize = "200"
	configAws.Aws.Config.ChefEbsVolumeType = "gp2"
	configAws.Aws.Config.OpensearchEbsVolumeIops = "300"
	configAws.Aws.Config.OpensearchEbsVolumeSize = "300"
	configAws.Aws.Config.OpensearchEbsVolumeType = "gp2"
	configAws.Aws.Config.PostgresqlEbsVolumeIops = "400"
	configAws.Aws.Config.PostgresqlEbsVolumeSize = "400"
	configAws.Aws.Config.PostgresqlEbsVolumeType = "gp2"

	return configAws
}

func GetFailedTestAWSConfigData() *config_parser.HAAwsConfigToml {
	configAwsFailed := GetTestAWSConfigData()
	configAwsFailed.Architecture.ConfigInitials.SSHKeyFile = "./pem"
	configAwsFailed.Automate.Config.Fqdn = " "
	configAwsFailed.ChefServer.Config.EnableCustomCerts = true

	configAwsFailed.ChefServer.Config.PrivateKey = "chef_private_key"
	configAwsFailed.ChefServer.Config.PublicKey = "chef_public_key"
	configAwsFailed.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: LOCALHOST_IP, PrivateKey: "chef_private_key", PublicKey: "public_key", NodesDn: "test.com"},
		{IP: LOCALHOST_IP, PrivateKey: "private_key", PublicKey: "chef_public_key", NodesDn: "test.co"},
	}
	configAwsFailed.Aws.Config.Profile = ""
	configAwsFailed.Aws.Config.Region = ""
	configAwsFailed.Aws.Config.AwsVpcId = ""
	configAwsFailed.Aws.Config.AwsCidrBlockAddr = "block"
	configAwsFailed.Aws.Config.PrivateCustomSubnets = []string{}
	configAwsFailed.Aws.Config.PublicCustomSubnets = []string{}
	configAwsFailed.Aws.Config.AmiID = ""
	configAwsFailed.Aws.Config.LBAccessLogs = "34"
	configAwsFailed.Aws.Config.AutomateServerInstanceType = ""
	configAwsFailed.Aws.Config.ChefServerInstanceType = ""
	configAwsFailed.Aws.Config.OpensearchServerInstanceType = ""
	configAwsFailed.Aws.Config.PostgresqlServerInstanceType = "454"
	configAwsFailed.Aws.Config.AutomateLbCertificateArn = ""
	configAwsFailed.Aws.Config.ChefServerLbCertificateArn = ""

	configAwsFailed.Architecture.ConfigInitials.SecretsKeyFile = ""
	configAwsFailed.Architecture.ConfigInitials.SecretsStoreFile = ""
	configAwsFailed.Architecture.ConfigInitials.Architecture = ""
	configAwsFailed.Architecture.ConfigInitials.WorkspacePath = ""
	configAwsFailed.Architecture.ConfigInitials.SSHUser = ""
	configAwsFailed.Architecture.ConfigInitials.SSHKeyFile = ""
	configAwsFailed.Architecture.ConfigInitials.SSHPort = ""
	configAwsFailed.Architecture.ConfigInitials.BackupMount = "/mnt/"
	configAwsFailed.Architecture.ConfigInitials.S3BucketName = "failed_s3"
	configAwsFailed.Automate.Config.EnableCustomCerts = true
	configAwsFailed.Automate.Config.PrivateKey = ""
	configAwsFailed.Postgresql.Config.EnableCustomCerts = true
	configAwsFailed.Postgresql.Config.RootCA = ""
	configAwsFailed.Postgresql.Config.PrivateKey = ""
	configAwsFailed.Postgresql.Config.PublicKey = ""
	configAwsFailed.Postgresql.Config.PrivateKey = ""
	configAwsFailed.Aws.Config.SetupManagedServices = true
	configAwsFailed.Opensearch.Config.EnableCustomCerts = true
	configAwsFailed.Opensearch.Config.PrivateKey = ""
	configAwsFailed.Aws.Config.AutomateEbsVolumeIops = "ten"
	configAwsFailed.Aws.Config.AutomateEbsVolumeSize = "twenty"
	configAwsFailed.Aws.Config.PostgresqlEbsVolumeIops = "q2ds"
	configAwsFailed.Aws.Config.PostgresqlEbsVolumeSize = "feg2"

	configAwsFailed.Opensearch.Config.EnableCustomCerts = true

	return configAwsFailed
}

func GetAutomateScConfigTestData() *sc.AutomateConfig {

	c := sc.NewAutomateConfig()
	c.Global.V1.Fqdn = w.String("test.com")
	c.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
	c.Deployment.V1.Svc.DeploymentType = w.String("local")
	c.Deployment.V1.Svc.Channel = w.String("current")
	c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
	c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")
	c.Deployment.V1.Svc.Origin = w.String("chef")
	c.Deployment.V1.Svc.PackageCleanupMode = w.String("conservative")
	c.Deployment.V1.Svc.Products = []string{"automate", "chef-server"}
	c.Deployment.V1.Svc.ManifestCacheExpiry = w.String("4m")

	return c
}

func GetFailedAutomateScConfigTestData() *sc.AutomateConfig {
	cFailed := GetAutomateScConfigTestData()
	cFailed.Global.V1.Fqdn = w.String("-com-")
	cFailed.Deployment.V1.Svc.DeploymentType = w.String("test")
	cFailed.Deployment.V1.Svc.Channel = w.String("none")
	cFailed.Deployment.V1.Svc.AdminUser.Username = w.String("")
	cFailed.Deployment.V1.Svc.AdminUser.Password = w.String("")
	cFailed.Deployment.V1.Svc.Origin = w.String("12")
	cFailed.Deployment.V1.Svc.PackageCleanupMode = w.String("xtz")
	cFailed.Deployment.V1.Svc.Products = []string{"ch", "che"}
	cFailed.Deployment.V1.Svc.ManifestCacheExpiry = w.String("")

	return cFailed
}

func validTLSCredentialForTest() *shared.FrontendTLSCredential {
	return &shared.FrontendTLSCredential{
		ServerName: "test.example",
		CertPath:   "./testdata/admin.crt",
		KeyPath:    SSH_KEY_FILE_PATH,
	}
}

func validTLSCredentialSliceForTest() []*shared.FrontendTLSCredential {
	return []*shared.FrontendTLSCredential{validTLSCredentialForTest()}
}
