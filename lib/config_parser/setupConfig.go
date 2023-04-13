package config_parser

type CertByIP struct {
	IP         string `toml:"ip"`
	PrivateKey string `toml:"private_key"`
	PublicKey  string `toml:"public_key"`
	NodesDn    string `toml:"nodes_dn,omitempty"`
}

type HAAwsConfigToml struct {
	Architecture struct {
		ConfigInitials struct {
			SecretsKeyFile              string `toml:"secrets_key_file"`
			SecretsStoreFile            string `toml:"secrets_store_file"`
			Architecture                string `toml:"architecture"`
			WorkspacePath               string `toml:"workspace_path"`
			SSHUser                     string `toml:"ssh_user"`
			SSHKeyFile                  string `toml:"ssh_key_file"`
			SSHPort                     string `toml:"ssh_port"`
			SudoPassword                string `toml:"sudo_password"`
			LoggingMonitoringManagement string `toml:"logging_monitoring_management"`
			NewElk                      string `toml:"new_elk"`
			ExistingElkInstanceIP       string `toml:"existing_elk_instance_ip"`
			ExistingElkPort             string `toml:"existing_elk_port"`
			ExistingElkCert             string `toml:"existing_elk_cert"`
			ExistingElkUsername         string `toml:"existing_elk_username"`
			ExistingElkPassword         string `toml:"existing_elk_password"`
			BackupMount                 string `toml:"backup_mount"`
			BackupConfig                string `toml:"backup_config"`
			S3BucketName                string `toml:"s3_bucketName"`
			HabitatUIDGid               string `toml:"habitat_uid_gid"`
		} `toml:"aws"`
	} `toml:"architecture"`
	Automate struct {
		Config struct {
			AdminPassword     string     `toml:"admin_password"`
			Fqdn              string     `toml:"fqdn"`
			AutomateSetupType string     `toml:"automate_setup_type"`
			InstanceCount     string     `toml:"instance_count"`
			TeamsPort         string     `toml:"teams_port"`
			ConfigFile        string     `toml:"config_file"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"automate"`
	ChefServer struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"chef_server"`
	Opensearch struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			AdminCert         string     `toml:"admin_cert"`
			AdminKey          string     `toml:"admin_key"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			AdminDn           string     `toml:"admin_dn"`
			NodesDn           string     `toml:"nodes_dn"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"opensearch"`
	Postgresql struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"postgresql"`
	Aws struct {
		Config struct {
			Profile                      string   `toml:"profile"`
			Region                       string   `toml:"region"`
			AwsVpcId                     string   `toml:"aws_vpc_id"`
			AwsCidrBlockAddr             string   `toml:"aws_cidr_block_addr"`
			PrivateCustomSubnets         []string `toml:"private_custom_subnets"`
			PublicCustomSubnets          []string `toml:"public_custom_subnets"`
			SSHKeyPairName               string   `toml:"ssh_key_pair_name"`
			SetupManagedServices         bool     `toml:"setup_managed_services"`
			OpensearchDomainName         string   `toml:"managed_opensearch_domain_name"`
			OpensearchDomainUrl          string   `toml:"managed_opensearch_domain_url"`
			OpensearchUsername           string   `toml:"managed_opensearch_username"`
			OpensearchUserPassword       string   `toml:"managed_opensearch_user_password"`
			OpensearchCertificate        string   `toml:"managed_opensearch_certificate"`
			AwsOsSnapshotRoleArn         string   `toml:"aws_os_snapshot_role_arn"`
			OsUserAccessKeyId            string   `toml:"os_snapshot_user_access_key_id"`
			OsUserAccessKeySecret        string   `toml:"os_snapshot_user_access_key_secret"`
			RDSInstanceUrl               string   `toml:"managed_rds_instance_url"`
			RDSSuperUserName             string   `toml:"managed_rds_superuser_username"`
			RDSSuperUserPassword         string   `toml:"managed_rds_superuser_password"`
			RDSDBUserName                string   `toml:"managed_rds_dbuser_username"`
			RDSDBUserPassword            string   `toml:"managed_rds_dbuser_password"`
			RDSCertificate               string   `toml:"managed_rds_certificate"`
			AmiFilterName                string   `toml:"ami_filter_name"`
			AmiFilterVirtType            string   `toml:"ami_filter_virt_type"`
			AmiFilterOwner               string   `toml:"ami_filter_owner"`
			AmiID                        string   `toml:"ami_id"`
			LBAccessLogs                 string   `toml:"lb_access_logs"`
			DeleteOnTermination          bool     `toml:"delete_on_termination"`
			AutomateServerInstanceType   string   `toml:"automate_server_instance_type"`
			ChefServerInstanceType       string   `toml:"chef_server_instance_type"`
			OpensearchServerInstanceType string   `toml:"opensearch_server_instance_type"`
			PostgresqlServerInstanceType string   `toml:"postgresql_server_instance_type"`
			AutomateLbCertificateArn     string   `toml:"automate_lb_certificate_arn"`
			ChefServerLbCertificateArn   string   `toml:"chef_server_lb_certificate_arn"`
			AutomateEbsVolumeIops        string   `toml:"automate_ebs_volume_iops"`
			AutomateEbsVolumeSize        string   `toml:"automate_ebs_volume_size"`
			AutomateEbsVolumeType        string   `toml:"automate_ebs_volume_type"`
			ChefEbsVolumeIops            string   `toml:"chef_ebs_volume_iops"`
			ChefEbsVolumeSize            string   `toml:"chef_ebs_volume_size"`
			ChefEbsVolumeType            string   `toml:"chef_ebs_volume_type"`
			OpensearchEbsVolumeIops      string   `toml:"opensearch_ebs_volume_iops"`
			OpensearchEbsVolumeSize      string   `toml:"opensearch_ebs_volume_size"`
			OpensearchEbsVolumeType      string   `toml:"opensearch_ebs_volume_type"`
			PostgresqlEbsVolumeIops      string   `toml:"postgresql_ebs_volume_iops"`
			PostgresqlEbsVolumeSize      string   `toml:"postgresql_ebs_volume_size"`
			PostgresqlEbsVolumeType      string   `toml:"postgresql_ebs_volume_type"`
			XContact                     string   `toml:"X-Contact"`
			XDept                        string   `toml:"X-Dept"`
			XProject                     string   `toml:"X-Project"`
			XProduction                  string   `toml:"X-Production"`
			XCustomer                    string   `toml:"X-Customer"`
			AwsAutomateRoute53Prefix     string   `toml:"aws_automate_route53_prefix"`
			AwsChefServerRoute53Prefix   string   `toml:"aws_chef_server_route53_prefix"`
			AwsRoute53HostedZone         string   `toml:"aws_route53_hosted_zone"`
			PostrgesqlDbIdentifier       string   `toml:"postgresql_db_identifier"`
			ElasticsearchDomainName      string   `toml:"elasticsearch_domain_name"`
			RDSInstanceType              string   `toml:"rds_postgresql_instance_type"`
			RDSRestoreIdentifier         string   `toml:"rds_postgresql_restore_identifier"`
			DatadogAPIKey                string   `toml:"datadog_api_key"`
			UseExistingManagedInfra      bool     `toml:"use_existing_managed_infra"`
		} `toml:"config"`
	} `toml:"aws"`
}

type HAOnPremConfigToml struct {
	Architecture struct {
		ConfigInitials struct {
			SecretsKeyFile              string `toml:"secrets_key_file,omitempty"`
			SecretsStoreFile            string `toml:"secrets_store_file,omitempty"`
			Architecture                string `toml:"architecture,omitempty"`
			WorkspacePath               string `toml:"workspace_path,omitempty"`
			SSHUser                     string `toml:"ssh_user,omitempty"`
			SSHKeyFile                  string `toml:"ssh_key_file,omitempty"`
			SSHPort                     string `toml:"ssh_port,omitempty"`
			SudoPassword                string `toml:"sudo_password,omitempty"`
			LoggingMonitoringManagement string `toml:"logging_monitoring_management,omitempty"`
			NewElk                      string `toml:"new_elk,omitempty"`
			ExistingElkInstanceIP       string `toml:"existing_elk_instance_ip,omitempty"`
			ExistingElkPort             string `toml:"existing_elk_port,omitempty"`
			ExistingElkCert             string `toml:"existing_elk_cert,omitempty"`
			ExistingElkUsername         string `toml:"existing_elk_username,omitempty"`
			ExistingElkPassword         string `toml:"existing_elk_password,omitempty"`
			BackupMount                 string `toml:"backup_mount,omitempty"`
			HabitatUIDGid               string `toml:"habitat_uid_gid,omitempty"`
			BackupConfig                string `toml:"backup_config,omitempty"`
		} `toml:"existing_infra"`
	} `toml:"architecture"`
	Automate struct {
		Config struct {
			AdminPassword     string     `toml:"admin_password"`
			Fqdn              string     `toml:"fqdn"`
			InstanceCount     string     `toml:"instance_count"`
			TeamsPort         string     `toml:"teams_port,omitempty"`
			ConfigFile        string     `toml:"config_file"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"automate"`
	ChefServer struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"chef_server"`
	Opensearch struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			AdminCert         string     `toml:"admin_cert"`
			AdminKey          string     `toml:"admin_key"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			AdminDn           string     `toml:"admin_dn"`
			NodesDn           string     `toml:"nodes_dn"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"opensearch"`
	Postgresql struct {
		Config struct {
			InstanceCount     string     `toml:"instance_count"`
			EnableCustomCerts bool       `toml:"enable_custom_certs"`
			RootCA            string     `toml:"root_ca"`
			PrivateKey        string     `toml:"private_key"`
			PublicKey         string     `toml:"public_key"`
			CertsByIP         []CertByIP `toml:"certs_by_ip"`
		} `toml:"config"`
	} `toml:"postgresql"`
	ExistingInfra struct {
		Config struct {
			AutomatePrivateIps   []string `toml:"automate_private_ips"`
			ChefServerPrivateIps []string `toml:"chef_server_private_ips"`
			OpensearchPrivateIps []string `toml:"opensearch_private_ips"`
			PostgresqlPrivateIps []string `toml:"postgresql_private_ips"`
		} `toml:"config"`
	} `toml:"existing_infra"`
	ExternalDB struct {
		Database struct {
			Type       string `toml:"type"`
			PostgreSQL struct {
				PostgreSQLInstanceURL       string `toml:"instance_url"`
				PostgreSQLSuperUserName     string `toml:"superuser_username"`
				PostgreSQLSuperUserPassword string `toml:"superuser_password"`
				PostgreSQLDBUserName        string `toml:"dbuser_username"`
				PostgreSQLDBUserPassword    string `toml:"dbuser_password"`
				PostgreSQLCertificate       string `toml:"postgresql_certificate"`
				PostgreSQLRootCert          string `toml:"postgresql_root_cert"`
			} `toml:"postgre_sql"`
			Opensearch struct {
				OpensearchInstanceURL       string `toml:"opensearch_domain_url"`
				OpensearchSuperUserName     string `toml:"opensearch_username"`
				OpensearchSuperUserPassword string `toml:"opensearch_user_password"`
				OpensearchCertificate       string `toml:"opensearch_certificate"`
				OpensearchRootCert          string `toml:"opensearch_root_cert"`
				OpensearchDomainName        string `toml:"opensearch_domain_name"`
				AWS                         struct {
					AwsOsSnapshotRoleArn  string `toml:"aws_os_snapshot_role_arn"`
					OsUserAccessKeyId     string `toml:"os_snapshot_user_access_key_id"`
					OsUserAccessKeySecret string `toml:"os_snapshot_user_access_key_secret"`
				} `toml:"aws"`
			} `toml:"open_search"`
		} `toml:"database"`
	} `toml:"external"`
	ObjectStorage struct {
		Config struct {
			BucketName string `toml:"bucket_name"`
			AccessKey  string `toml:"access_key"`
			SecretKey  string `toml:"secret_key"`
			Endpoint   string `toml:"endpoint"`
			Region     string `toml:"region"`
		} `toml:"config"`
	} `toml:"object_storage"`
}
