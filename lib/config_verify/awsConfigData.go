package config_verify

import (
	"github.com/chef/automate/lib/config_parser"
)

func GetTestAWSConfigData() *config_parser.HAAwsConfigToml {
	config := &config_parser.HAAwsConfigToml{}

	config.Architecture.ConfigInitials.SecretsKeyFile = "/testdata/key.pem"
	config.Architecture.ConfigInitials.SecretsStoreFile = "/path/to/secrets_store_file"
	config.Architecture.ConfigInitials.Architecture = "HA"
	config.Architecture.ConfigInitials.WorkspacePath = "/path/to/workspace"
	config.Architecture.ConfigInitials.SSHUser = "username"
	config.Architecture.ConfigInitials.SSHKeyFile = "./testdata/admin.pem"
	config.Architecture.ConfigInitials.SSHPort = "22"
	config.Architecture.ConfigInitials.SudoPassword = "password"
	config.Architecture.ConfigInitials.LoggingMonitoringManagement = "ELK"
	config.Architecture.ConfigInitials.NewElk = "true"
	config.Architecture.ConfigInitials.ExistingElkInstanceIP = "0.0.0.0"
	config.Architecture.ConfigInitials.ExistingElkPort = "9200"
	config.Architecture.ConfigInitials.ExistingElkCert = "/path/to/elk_cert"
	config.Architecture.ConfigInitials.ExistingElkUsername = "elk_user"
	config.Architecture.ConfigInitials.ExistingElkPassword = "elk_password"
	config.Architecture.ConfigInitials.BackupMount = "/mnt/automate_backups"
	config.Architecture.ConfigInitials.BackupConfig = "/path/to/backup_config"
	config.Architecture.ConfigInitials.S3BucketName = "s3"
	config.Architecture.ConfigInitials.HabitatUIDGid = "123:456"

	config.Automate.Config.AdminPassword = "admin_password"
	config.Automate.Config.Fqdn = "example.com"
	config.Automate.Config.AutomateSetupType = "tiered"
	config.Automate.Config.InstanceCount = "3"
	config.Automate.Config.TeamsPort = "8080"
	config.Automate.Config.ConfigFile = "/path/to/automate_config"
	config.Automate.Config.EnableCustomCerts = false
	config.Automate.Config.RootCA = "/path/to/root_ca"
	config.Automate.Config.PrivateKey = "/path/to/private_key"
	config.Automate.Config.PublicKey = "/path/to/public_key"
	config.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	config.ChefServer.Config.InstanceCount = "1"
	config.ChefServer.Config.EnableCustomCerts = false
	config.ChefServer.Config.PrivateKey = "/path/to/chef_private_key"
	config.ChefServer.Config.PublicKey = "/path/to/chef_public_key"
	config.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	config.Opensearch.Config.InstanceCount = "1"
	config.Opensearch.Config.EnableCustomCerts = false
	config.Opensearch.Config.RootCA = "/path/to/opensearch_root_ca"
	config.Opensearch.Config.AdminCert = "/path/to/admin_cert"
	config.Opensearch.Config.AdminKey = "/path/to/admin_key"
	config.Opensearch.Config.PrivateKey = "/path/to/opensearch_private_key"
	config.Opensearch.Config.PublicKey = "/path/to/opensearch_public_key"
	config.Opensearch.Config.AdminDn = "CN=admin"
	config.Opensearch.Config.NodesDn = "CN=nodes"
	config.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}
	config.Postgresql.Config.InstanceCount = "1"
	config.Postgresql.Config.EnableCustomCerts = false
	config.Postgresql.Config.RootCA = "/path/to/postgresql_root_ca"
	config.Postgresql.Config.PrivateKey = "/path/to/postgresql_private_key"
	config.Postgresql.Config.PublicKey = "/path/to/postgresql_public_key"
	config.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	config.Aws.Config.Profile = "my_aws_profile"
	config.Aws.Config.Region = "us-west-2"
	config.Aws.Config.AwsVpcId = "vpc-12345678"
	config.Aws.Config.AwsCidrBlockAddr = "0.0.0.0/00"
	config.Aws.Config.PrivateCustomSubnets = []string{"subnet-abcdef01", "subnet-abcdef02"}
	config.Aws.Config.PublicCustomSubnets = []string{"subnet-abcdef03", "subnet-abcdef04"}
	config.Aws.Config.SSHKeyPairName = "my_key_pair"
	config.Aws.Config.SetupManagedServices = false
	config.Aws.Config.OpensearchDomainName = "my-opensearch-domain"
	config.Aws.Config.OpensearchDomainUrl = "https://my-opensearch-domain.domain.com"
	config.Aws.Config.OpensearchUsername = "opensearch_user"
	config.Aws.Config.OpensearchUserPassword = "opensearch_password"
	config.Aws.Config.OpensearchCertificate = "/path/to/managed_opensearch_cert"
	config.Aws.Config.AwsOsSnapshotRoleArn = "arn:aws:iam::1234567890:role/my-os-snapshot-role"
	config.Aws.Config.OsUserAccessKeyId = "my_os_user_access_key_id"
	config.Aws.Config.OsUserAccessKeySecret = "my_os_user_access_key_secret"
	config.Aws.Config.RDSInstanceUrl = "https://my-rds-instance.domain.com"
	config.Aws.Config.RDSSuperUserName = "rds_superuser"
	config.Aws.Config.RDSSuperUserPassword = "rds_superuser_password"
	config.Aws.Config.RDSDBUserName = "rds_dbuser"
	config.Aws.Config.RDSDBUserPassword = "rds_dbuser_password"
	config.Aws.Config.RDSCertificate = "/path/to/managed_rds_cert"
	config.Aws.Config.AmiFilterName = "my-ami-filter"
	config.Aws.Config.AmiFilterVirtType = "hvm"
	config.Aws.Config.AmiFilterOwner = "self"
	config.Aws.Config.AmiID = "ami-12345678"
	config.Aws.Config.LBAccessLogs = "/path/to/lb_access_logs"
	config.Aws.Config.DeleteOnTermination = true
	config.Aws.Config.AutomateServerInstanceType = "t2.medium"
	config.Aws.Config.ChefServerInstanceType = "t2.large"
	config.Aws.Config.OpensearchServerInstanceType = "r5.large"
	config.Aws.Config.PostgresqlServerInstanceType = "db.m5.large"
	config.Aws.Config.AutomateLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/automate-lb-cert"
	config.Aws.Config.ChefServerLbCertificateArn = "arn:aws:acm:us-west-2:1234567890:certificate/chef-server-lb-cert"
	config.Aws.Config.AutomateEbsVolumeIops = "100"
	config.Aws.Config.AutomateEbsVolumeSize = "100"
	config.Aws.Config.AutomateEbsVolumeType = "gp2"
	config.Aws.Config.ChefEbsVolumeIops = "200"
	config.Aws.Config.ChefEbsVolumeSize = "200"
	config.Aws.Config.ChefEbsVolumeType = "gp2"
	config.Aws.Config.OpensearchEbsVolumeIops = "300"
	config.Aws.Config.OpensearchEbsVolumeSize = "300"
	config.Aws.Config.OpensearchEbsVolumeType = "gp2"
	config.Aws.Config.PostgresqlEbsVolumeIops = "400"
	config.Aws.Config.PostgresqlEbsVolumeSize = "400"
	config.Aws.Config.PostgresqlEbsVolumeType = "gp2"

	return config
}
