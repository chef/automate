package config_verify

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/config_parser"
)

func GetTestOnPremConfigData() *config_parser.HAOnPremConfigToml {
	config_onprem := &config_parser.HAOnPremConfigToml{}

	// ConfigInitials
	config_onprem.Architecture.ConfigInitials.SecretsKeyFile = "key.pem"
	config_onprem.Architecture.ConfigInitials.SecretsStoreFile = "s/store/file"
	config_onprem.Architecture.ConfigInitials.Architecture = "Architecture"
	config_onprem.Architecture.ConfigInitials.WorkspacePath = "/path/workspace"
	config_onprem.Architecture.ConfigInitials.SSHUser = "username"
	config_onprem.Architecture.ConfigInitials.SSHKeyFile = "./testdata/admin.pem"
	config_onprem.Architecture.ConfigInitials.SSHPort = "24"
	config_onprem.Architecture.ConfigInitials.SudoPassword = "password"
	config_onprem.Architecture.ConfigInitials.LoggingMonitoringManagement = "logging_monitoring"
	config_onprem.Architecture.ConfigInitials.NewElk = "elk"
	config_onprem.Architecture.ConfigInitials.ExistingElkInstanceIP = "127.0.0.0"
	config_onprem.Architecture.ConfigInitials.ExistingElkPort = "9300"
	config_onprem.Architecture.ConfigInitials.ExistingElkCert = "/path/to/existing/elk/cert"
	config_onprem.Architecture.ConfigInitials.ExistingElkUsername = "elkusername"
	config_onprem.Architecture.ConfigInitials.ExistingElkPassword = "elkpassword"
	config_onprem.Architecture.ConfigInitials.BackupMount = "/mnt/automate_backups"
	config_onprem.Architecture.ConfigInitials.HabitatUIDGid = "uid_gid"
	config_onprem.Architecture.ConfigInitials.BackupConfig = "object_storage"
	config_onprem.ObjectStorage.Config.BucketName = "s3"

	// Automate Config
	config_onprem.Automate.Config.AdminPassword = "adminpassword"
	config_onprem.Automate.Config.Fqdn = "sampledomain.com"
	config_onprem.Automate.Config.InstanceCount = "1"
	config_onprem.Automate.Config.TeamsPort = "8080"
	config_onprem.Automate.Config.ConfigFile = "/path/to/config/file"
	config_onprem.Automate.Config.EnableCustomCerts = false
	config_onprem.Automate.Config.RootCA = "/path/to/root/ca/file"
	config_onprem.Automate.Config.PrivateKey = "/path/to/private/key"
	config_onprem.Automate.Config.PublicKey = "/path/to/public/key"
	config_onprem.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ChefServer Config
	config_onprem.ChefServer.Config.InstanceCount = "1"
	config_onprem.ChefServer.Config.EnableCustomCerts = false
	config_onprem.ChefServer.Config.PrivateKey = "/path/to/private/key"
	config_onprem.ChefServer.Config.PublicKey = "/path/to/public/key"
	config_onprem.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Opensearch Config
	config_onprem.Opensearch.Config.InstanceCount = "1"
	config_onprem.Opensearch.Config.EnableCustomCerts = false
	config_onprem.Opensearch.Config.RootCA = "/path/to/root/ca"
	config_onprem.Opensearch.Config.AdminCert = "/path/to/admin/cert"
	config_onprem.Opensearch.Config.AdminKey = "/path/to/admin/key"
	config_onprem.Opensearch.Config.PrivateKey = "/path/to/private/key"
	config_onprem.Opensearch.Config.PublicKey = "/path/to/public/key"
	config_onprem.Opensearch.Config.AdminDn = "admin_dn"
	config_onprem.Opensearch.Config.NodesDn = "nodes_dn"
	config_onprem.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Postgresql Config
	config_onprem.Postgresql.Config.InstanceCount = "1"
	config_onprem.Postgresql.Config.EnableCustomCerts = false
	config_onprem.Postgresql.Config.RootCA = "/path/to/root/ca"
	config_onprem.Postgresql.Config.PrivateKey = "/path/to/private/key"
	config_onprem.Postgresql.Config.PublicKey = "/path/to/public/key"
	config_onprem.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ExistingInfra Config
	config_onprem.ExistingInfra.Config.AutomatePrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config_onprem.ExistingInfra.Config.ChefServerPrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config_onprem.ExistingInfra.Config.OpensearchPrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config_onprem.ExistingInfra.Config.PostgresqlPrivateIps = []string{"127.0.0.1", "127.0.0.1"}

	// ExternalDB Config
	config_onprem.ExternalDB.Database.Type = ""

	// ObjectStorage Config
	config_onprem.ObjectStorage.Config.AccessKey = "sample-access-key"
	config_onprem.ObjectStorage.Config.SecretKey = "sample-secret-key"
	config_onprem.ObjectStorage.Config.Endpoint = "sample-endpoint"
	config_onprem.ObjectStorage.Config.Region = "sample-region"

	return config_onprem
}

func GetTestOnPremFailedConfigData() *config_parser.HAOnPremConfigToml {
	config_onprem_failed := GetTestOnPremConfigData()
	config_onprem_failed.Architecture.ConfigInitials.SSHKeyFile = "./pem"
	config_onprem_failed.Architecture.ConfigInitials.BackupMount = "/mnt/"
	config_onprem_failed.Automate.Config.Fqdn = ".com"
	config_onprem_failed.ChefServer.Config.EnableCustomCerts = true
	config_onprem_failed.ChefServer.Config.PrivateKey = ""
	config_onprem_failed.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "key", PublicKey: "key", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "key", PublicKey: "key", NodesDn: ""},
	}
	config_onprem_failed.Architecture.ConfigInitials.BackupConfig = "file_system"
	config_onprem_failed.ExistingInfra.Config.AutomatePrivateIps = []string{}
	config_onprem_failed.ExistingInfra.Config.ChefServerPrivateIps = []string{}
	config_onprem_failed.ExistingInfra.Config.OpensearchPrivateIps = []string{"11", "12.1"}
	config_onprem_failed.ExistingInfra.Config.PostgresqlPrivateIps = []string{"12.1", "127.0.0.1"}

	config_onprem_failed.Architecture.ConfigInitials.SecretsStoreFile = ""
	config_onprem_failed.Architecture.ConfigInitials.Architecture = ""
	config_onprem_failed.Architecture.ConfigInitials.WorkspacePath = "34"
	config_onprem_failed.Automate.Config.EnableCustomCerts = true
	config_onprem_failed.Automate.Config.PublicKey = ""
	config_onprem_failed.Postgresql.Config.EnableCustomCerts = true
	config_onprem_failed.Postgresql.Config.PrivateKey = ""
	config_onprem_failed.Opensearch.Config.EnableCustomCerts = true
	config_onprem_failed.Opensearch.Config.RootCA = ""
	config_onprem_failed.Opensearch.Config.AdminCert = "ssf"
	config_onprem_failed.Opensearch.Config.AdminKey = ""
	config_onprem_failed.Opensearch.Config.PrivateKey = ""
	config_onprem_failed.Opensearch.Config.PublicKey = ""
	config_onprem_failed.Opensearch.Config.PublicKey = ""
	config_onprem_failed.Architecture.ConfigInitials.SSHUser = ""
	config_onprem_failed.ObjectStorage.Config.BucketName = "s4"

	return config_onprem_failed
}

func GetTestAWSConfigData() *config_parser.HAAwsConfigToml {
	config_aws := &config_parser.HAAwsConfigToml{}

	config_aws.Architecture.ConfigInitials.SecretsKeyFile = "/testdata/key.pem"
	config_aws.Architecture.ConfigInitials.SecretsStoreFile = "/path/to/secrets_store_file"
	config_aws.Architecture.ConfigInitials.Architecture = "HA"
	config_aws.Architecture.ConfigInitials.WorkspacePath = "/path/to/workspace"
	config_aws.Architecture.ConfigInitials.SSHUser = "username"
	config_aws.Architecture.ConfigInitials.SSHKeyFile = "./testdata/admin.pem"
	config_aws.Architecture.ConfigInitials.SSHPort = "22"
	config_aws.Architecture.ConfigInitials.SudoPassword = "password"
	config_aws.Architecture.ConfigInitials.LoggingMonitoringManagement = "ELK"
	config_aws.Architecture.ConfigInitials.NewElk = "true"
	config_aws.Architecture.ConfigInitials.ExistingElkInstanceIP = "0.0.0.0"
	config_aws.Architecture.ConfigInitials.ExistingElkPort = "9200"
	config_aws.Architecture.ConfigInitials.ExistingElkCert = "/path/to/elk_cert"
	config_aws.Architecture.ConfigInitials.ExistingElkUsername = "elk_user"
	config_aws.Architecture.ConfigInitials.ExistingElkPassword = "elk_password"
	config_aws.Architecture.ConfigInitials.BackupMount = "/mnt/automate_backups"
	config_aws.Architecture.ConfigInitials.BackupConfig = "/path/to/backup_config"
	config_aws.Architecture.ConfigInitials.S3BucketName = "s3"
	config_aws.Architecture.ConfigInitials.HabitatUIDGid = "123:456"

	config_aws.Automate.Config.AdminPassword = "admin_password"
	config_aws.Automate.Config.Fqdn = "example.com"
	config_aws.Automate.Config.AutomateSetupType = "tiered"
	config_aws.Automate.Config.InstanceCount = "3"
	config_aws.Automate.Config.TeamsPort = "8080"
	config_aws.Automate.Config.ConfigFile = "/path/to/automate_config"
	config_aws.Automate.Config.EnableCustomCerts = false
	config_aws.Automate.Config.RootCA = "/path/to/root_ca"
	config_aws.Automate.Config.PrivateKey = "/path/to/private_key"
	config_aws.Automate.Config.PublicKey = "/path/to/public_key"
	config_aws.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	config_aws.ChefServer.Config.InstanceCount = "1"
	config_aws.ChefServer.Config.EnableCustomCerts = false
	config_aws.ChefServer.Config.PrivateKey = "/path/to/chef_private_key"
	config_aws.ChefServer.Config.PublicKey = "/path/to/chef_public_key"
	config_aws.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	config_aws.Opensearch.Config.InstanceCount = "1"
	config_aws.Opensearch.Config.EnableCustomCerts = false
	config_aws.Opensearch.Config.RootCA = "/path/to/opensearch_root_ca"
	config_aws.Opensearch.Config.AdminCert = "/path/to/admin_cert"
	config_aws.Opensearch.Config.AdminKey = "/path/to/admin_key"
	config_aws.Opensearch.Config.PrivateKey = "/path/to/opensearch_private_key"
	config_aws.Opensearch.Config.PublicKey = "/path/to/opensearch_public_key"
	config_aws.Opensearch.Config.AdminDn = "CN=admin"
	config_aws.Opensearch.Config.NodesDn = "CN=nodes"
	config_aws.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	config_aws.Postgresql.Config.InstanceCount = "1"
	config_aws.Postgresql.Config.EnableCustomCerts = false
	config_aws.Postgresql.Config.RootCA = "/path/to/postgresql_root_ca"
	config_aws.Postgresql.Config.PrivateKey = "/path/to/postgresql_private_key"
	config_aws.Postgresql.Config.PublicKey = "/path/to/postgresql_public_key"
	config_aws.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	config_aws.Aws.Config.Profile = "my_aws_profile"
	config_aws.Aws.Config.Region = "us-west-2"
	config_aws.Aws.Config.AwsVpcId = "vpc-12345678"
	config_aws.Aws.Config.AwsCidrBlockAddr = "0.0.0.0/00"
	config_aws.Aws.Config.PrivateCustomSubnets = []string{"subnet-abcdef01", "subnet-abcdef02"}
	config_aws.Aws.Config.PublicCustomSubnets = []string{"subnet-abcdef03", "subnet-abcdef04"}
	config_aws.Aws.Config.SSHKeyPairName = "my_key_pair"
	config_aws.Aws.Config.SetupManagedServices = false
	config_aws.Aws.Config.OpensearchDomainName = "my-opensearch-domain"
	config_aws.Aws.Config.OpensearchDomainUrl = "https://my-opensearch-domain.domain.com"
	config_aws.Aws.Config.OpensearchUsername = "opensearch_user"
	config_aws.Aws.Config.OpensearchUserPassword = "opensearch_password"
	config_aws.Aws.Config.OpensearchCertificate = "/path/to/managed_opensearch_cert"
	config_aws.Aws.Config.AwsOsSnapshotRoleArn = "arn:aws:iam::1234567890:role/my-os-snapshot-role"
	config_aws.Aws.Config.OsUserAccessKeyId = "my_os_user_access_key_id"
	config_aws.Aws.Config.OsUserAccessKeySecret = "my_os_user_access_key_secret"
	config_aws.Aws.Config.RDSInstanceUrl = "https://my-rds-instance.domain.com"
	config_aws.Aws.Config.RDSSuperUserName = "rds_superuser"
	config_aws.Aws.Config.RDSSuperUserPassword = "rds_superuser_password"
	config_aws.Aws.Config.RDSDBUserName = "rds_dbuser"
	config_aws.Aws.Config.RDSDBUserPassword = "rds_dbuser_password"
	config_aws.Aws.Config.RDSCertificate = "/path/to/managed_rds_cert"
	config_aws.Aws.Config.AmiFilterName = "my-ami-filter"
	config_aws.Aws.Config.AmiFilterVirtType = "hvm"
	config_aws.Aws.Config.AmiFilterOwner = "self"
	config_aws.Aws.Config.AmiID = "ami-12345678"
	config_aws.Aws.Config.LBAccessLogs = "/path/to/lb_access_logs"
	config_aws.Aws.Config.DeleteOnTermination = true
	config_aws.Aws.Config.AutomateServerInstanceType = "t2.medium"
	config_aws.Aws.Config.ChefServerInstanceType = "t2.large"
	config_aws.Aws.Config.OpensearchServerInstanceType = "r5.large"
	config_aws.Aws.Config.PostgresqlServerInstanceType = "db.m5.large"
	config_aws.Aws.Config.AutomateLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/automate-lb-cert"
	config_aws.Aws.Config.ChefServerLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/chef-server-lb-cert"
	config_aws.Aws.Config.AutomateEbsVolumeIops = "100"
	config_aws.Aws.Config.AutomateEbsVolumeSize = "100"
	config_aws.Aws.Config.AutomateEbsVolumeType = "gp2"
	config_aws.Aws.Config.ChefEbsVolumeIops = "200"
	config_aws.Aws.Config.ChefEbsVolumeSize = "200"
	config_aws.Aws.Config.ChefEbsVolumeType = "gp2"
	config_aws.Aws.Config.OpensearchEbsVolumeIops = "300"
	config_aws.Aws.Config.OpensearchEbsVolumeSize = "300"
	config_aws.Aws.Config.OpensearchEbsVolumeType = "gp2"
	config_aws.Aws.Config.PostgresqlEbsVolumeIops = "400"
	config_aws.Aws.Config.PostgresqlEbsVolumeSize = "400"
	config_aws.Aws.Config.PostgresqlEbsVolumeType = "gp2"

	return config_aws
}

func GetFailedTestAWSConfigData() *config_parser.HAAwsConfigToml {
	config_aws_failed := GetTestAWSConfigData()
	config_aws_failed.Architecture.ConfigInitials.SSHKeyFile = "./pem"
	config_aws_failed.Automate.Config.Fqdn = " "
	config_aws_failed.ChefServer.Config.EnableCustomCerts = true

	config_aws_failed.ChefServer.Config.PrivateKey = "chef_private_key"
	config_aws_failed.ChefServer.Config.PublicKey = "chef_public_key"
	config_aws_failed.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "chef_private_key", PublicKey: "public_key", NodesDn: "test.com"},
		{IP: "127.0.0.1", PrivateKey: "private_key", PublicKey: "chef_public_key", NodesDn: "test.co"},
	}
	config_aws_failed.Aws.Config.Profile = ""
	config_aws_failed.Aws.Config.Region = ""
	config_aws_failed.Aws.Config.AwsVpcId = ""
	config_aws_failed.Aws.Config.AwsCidrBlockAddr = "block"
	config_aws_failed.Aws.Config.PrivateCustomSubnets = []string{}
	config_aws_failed.Aws.Config.PublicCustomSubnets = []string{}
	config_aws_failed.Aws.Config.AmiID = ""
	config_aws_failed.Aws.Config.LBAccessLogs = "34"
	config_aws_failed.Aws.Config.AutomateServerInstanceType = ""
	config_aws_failed.Aws.Config.ChefServerInstanceType = ""
	config_aws_failed.Aws.Config.OpensearchServerInstanceType = ""
	config_aws_failed.Aws.Config.PostgresqlServerInstanceType = "454"
	config_aws_failed.Aws.Config.AutomateLbCertificateArn = ""
	config_aws_failed.Aws.Config.ChefServerLbCertificateArn = ""

	config_aws_failed.Architecture.ConfigInitials.SecretsKeyFile = ""
	config_aws_failed.Architecture.ConfigInitials.SecretsStoreFile = ""
	config_aws_failed.Architecture.ConfigInitials.Architecture = ""
	config_aws_failed.Architecture.ConfigInitials.WorkspacePath = ""
	config_aws_failed.Architecture.ConfigInitials.SSHUser = ""
	config_aws_failed.Architecture.ConfigInitials.SSHKeyFile = ""
	config_aws_failed.Architecture.ConfigInitials.SSHPort = ""
	config_aws_failed.Architecture.ConfigInitials.BackupMount = "/mnt/"
	config_aws_failed.Architecture.ConfigInitials.S3BucketName = "failed_s3"
	config_aws_failed.Automate.Config.EnableCustomCerts = true
	config_aws_failed.Automate.Config.PrivateKey = ""
	config_aws_failed.Postgresql.Config.EnableCustomCerts = true
	config_aws_failed.Postgresql.Config.RootCA = ""
	config_aws_failed.Postgresql.Config.PrivateKey = ""
	config_aws_failed.Postgresql.Config.PublicKey = ""
	config_aws_failed.Postgresql.Config.PrivateKey = ""
	config_aws_failed.Aws.Config.SetupManagedServices = true
	config_aws_failed.Opensearch.Config.EnableCustomCerts = true
	config_aws_failed.Opensearch.Config.PrivateKey = ""
	config_aws_failed.Aws.Config.AutomateEbsVolumeIops = "ten"
	config_aws_failed.Aws.Config.AutomateEbsVolumeSize = "twenty"
	config_aws_failed.Aws.Config.PostgresqlEbsVolumeIops = "q2ds"
	config_aws_failed.Aws.Config.PostgresqlEbsVolumeSize = "feg2"

	config_aws_failed.Opensearch.Config.EnableCustomCerts = true

	return config_aws_failed
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

func GetFAiledAutomateScConfigTestData() *sc.AutomateConfig {
	c_failed := GetAutomateScConfigTestData()
	c_failed.Global.V1.Fqdn = w.String(".com")
	c_failed.Deployment.V1.Svc.DeploymentType = w.String("test")
	c_failed.Deployment.V1.Svc.Channel = w.String("none")
	c_failed.Deployment.V1.Svc.AdminUser.Username = w.String("")
	c_failed.Deployment.V1.Svc.AdminUser.Password = w.String("")
	c_failed.Deployment.V1.Svc.Origin = w.String("12")
	c_failed.Deployment.V1.Svc.PackageCleanupMode = w.String("xtz")
	c_failed.Deployment.V1.Svc.Products = []string{"ch", "che"}
	c_failed.Deployment.V1.Svc.ManifestCacheExpiry = w.String("")

	return c_failed
}

func validTLSCredentialForTest() *shared.FrontendTLSCredential {
	return &shared.FrontendTLSCredential{
		ServerName: "test.example",
		CertPath:   "./testdata/admin.crt",
		KeyPath:    "./testdata/admin.pem",
	}
}

func validTLSCredentialSliceForTest() []*shared.FrontendTLSCredential {
	return []*shared.FrontendTLSCredential{validTLSCredentialForTest()}
}
