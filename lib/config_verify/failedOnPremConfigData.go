package config_verify

import (
	"github.com/chef/automate/lib/config_parser"
)

func GetTestOnPremFailedConfigData() *config_parser.HAOnPremConfigToml {
	config := &config_parser.HAOnPremConfigToml{}

	// ConfigInitials
	config.Architecture.ConfigInitials.SecretsKeyFile = "/path/to/secrets/key/file"
	config.Architecture.ConfigInitials.SecretsStoreFile = "/path/to/secrets/store/file"
	config.Architecture.ConfigInitials.Architecture = "Sample Architecture"
	config.Architecture.ConfigInitials.WorkspacePath = "/path/to/workspace"
	config.Architecture.ConfigInitials.SSHUser = "sshuser"
	config.Architecture.ConfigInitials.SSHKeyFile = "/path/to/ssh/key/file"
	config.Architecture.ConfigInitials.SSHPort = "22"
	config.Architecture.ConfigInitials.SudoPassword = "sudopassword"
	config.Architecture.ConfigInitials.LoggingMonitoringManagement = "logging_monitoring_management"
	config.Architecture.ConfigInitials.NewElk = "new_elk"
	config.Architecture.ConfigInitials.ExistingElkInstanceIP = "127.0.0.1"
	config.Architecture.ConfigInitials.ExistingElkPort = "9200"
	config.Architecture.ConfigInitials.ExistingElkCert = "/path/to/existing/elk/cert"
	config.Architecture.ConfigInitials.ExistingElkUsername = "elkusername"
	config.Architecture.ConfigInitials.ExistingElkPassword = "elkpassword"
	config.Architecture.ConfigInitials.BackupMount = "/path/to/backup/mount"
	config.Architecture.ConfigInitials.HabitatUIDGid = "habitat_uid_gid"
	config.Architecture.ConfigInitials.BackupConfig = "object_storage"
	config.ObjectStorage.Config.BucketName = "s4"
	config.ExternalDB.Database.Type = "aws"

	// Automate Config
	config.Automate.Config.AdminPassword = "adminpassword"
	config.Automate.Config.Fqdn = "sampledomain.com"
	config.Automate.Config.InstanceCount = "1"
	config.Automate.Config.TeamsPort = "8080"
	config.Automate.Config.ConfigFile = "/path/to/config/file"
	config.Automate.Config.EnableCustomCerts = true
	config.Automate.Config.RootCA = "/path/to/root/ca/file"
	config.Automate.Config.PrivateKey = "/path/to/private/key"
	config.Automate.Config.PublicKey = "/path/to/public/key"
	config.Automate.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ChefServer Config
	config.ChefServer.Config.InstanceCount = "1"
	config.ChefServer.Config.EnableCustomCerts = false
	config.ChefServer.Config.PrivateKey = "/path/to/private/key"
	config.ChefServer.Config.PublicKey = "/path/to/public/key"
	config.ChefServer.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Opensearch Config
	config.Opensearch.Config.InstanceCount = "1"
	config.Opensearch.Config.EnableCustomCerts = true
	config.Opensearch.Config.RootCA = "/path/to/root/ca"
	config.Opensearch.Config.AdminCert = "/path/to/admin/cert"
	config.Opensearch.Config.AdminKey = "/path/to/admin/key"
	config.Opensearch.Config.PrivateKey = "/path/to/private/key"
	config.Opensearch.Config.PublicKey = "/path/to/public/key"
	config.Opensearch.Config.AdminDn = "admin_dn"
	config.Opensearch.Config.NodesDn = "nodes_dn"
	config.Opensearch.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// Postgresql Config
	config.Postgresql.Config.InstanceCount = "1"
	config.Postgresql.Config.EnableCustomCerts = false
	config.Postgresql.Config.RootCA = "/path/to/root/ca"
	config.Postgresql.Config.PrivateKey = "/path/to/private/key"
	config.Postgresql.Config.PublicKey = "/path/to/public/key"
	config.Postgresql.Config.CertsByIP = []config_parser.CertByIP{
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
		{IP: "127.0.0.1", PrivateKey: "", PublicKey: "", NodesDn: ""},
	}

	// ExistingInfra Config
	config.ExistingInfra.Config.AutomatePrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config.ExistingInfra.Config.ChefServerPrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config.ExistingInfra.Config.OpensearchPrivateIps = []string{"127.0.0.1", "127.0.0.1"}
	config.ExistingInfra.Config.PostgresqlPrivateIps = []string{"127.0.0.1", "127.0.0.1"}

	// ExternalDB Config
	config.ExternalDB.Database.Type = "postgre_sql"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL = "sample-postgres-instance"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName = "superuser"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword = "superuserpassword"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName = "dbuser"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword = "dbuserpassword"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLCertificate = "/path/to/postgresql/cert"
	config.ExternalDB.Database.PostgreSQL.PostgreSQLRootCert = "/path/to/postgresql/rootcert"
	config.ExternalDB.Database.Opensearch.OpensearchInstanceURL = "sample-opensearch-instance"
	config.ExternalDB.Database.Opensearch.OpensearchSuperUserName = "opensearchuser"
	config.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword = "opensearchuserpassword"
	config.ExternalDB.Database.Opensearch.OpensearchCertificate = "/path/to/opensearch/cert"
	config.ExternalDB.Database.Opensearch.OpensearchRootCert = "/path/to/opensearch/rootcert"
	config.ExternalDB.Database.Opensearch.OpensearchDomainName = "opensearchdomain"
	config.ExternalDB.Database.Opensearch.AWS.AwsOsSnapshotRoleArn = "sample-role-arn"
	config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeyId = "sample-access-key-id"
	config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeySecret = "sample-access-key-secret"

	// ObjectStorage Config
	config.ObjectStorage.Config.BucketName = "sample-bucket"
	config.ObjectStorage.Config.AccessKey = "sample-access-key"
	config.ObjectStorage.Config.SecretKey = "sample-secret-key"
	config.ObjectStorage.Config.Endpoint = "sample-endpoint"
	config.ObjectStorage.Config.Region = "sample-region"

	return config
}
