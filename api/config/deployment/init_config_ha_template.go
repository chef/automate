package deployment

const haAwsConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.aws]
secrets_key_file = "{{ .SecretsKeyFile }}"
secrets_store_file = "{{ .SecretsStoreFile }}"
architecture = "{{ .Architecture }}"
workspace_path = "{{ .WorkspacePath }}"
ssh_user = "{{ .SshUser }}"
ssh_key_file = "{{ .SshKeyFile }}"
sudo_password = "{{ .SudoPassword }}"
logging_monitoring_management = "{{ .LoggingMonitoringManagement }}"
new_elk = "{{ .NewElk }}"
# existing_elk_instance_ip "{{ .ExistingElk }}"
# existing_elk_port "{{ .ExistingElkPort }}"
# existing_elk_cert "{{ .ExistingElkCert }}"
# existing_elk_username "{{ .ExistingElkUsername }}"
# existing_elk_password "{{ .ExistingElkPassword }}"
backup_mount = "{{ .BackupMount }}"

[automate.config]
# admin_password = "{{ .AutomateAdminPassword }}"
# fqdn = "{{ .AutomateFQDN }}"
instance_count = "{{ .AutomateInstanceCount }}"
# teams_port = "{{ .AutomateTeamsPort }}"
config_file = "{{ .AutomateConfigFile }}"

[chef_server.config]
instance_count = "{{ .ChefServerInstanceCount }}"

[elasticsearch.config]
instance_count = "{{ .ElasticSearchInstanceCount }}"

[postgresql.config]
instance_count = "{{ .PostgresqlInstanceCount }}"

[aws.config]
profile = "{{ .AwsProfile }}"
region = "{{ .AwsRegion }}"
ssh_key_pair_name = "{{ .AwsSshKeyPairName }}"
ami_filter_name = "{{ .AwsAmiFilterName }}"
ami_filter_virt_type = "{{ .AwsAmiFilterVirtType }}"
ami_filter_owner = "{{ .AwsFilterOwner }}"
ami_id = "{{ .AwsAmiId }}"
automate_server_instance_type = "{{ .AwsAutomateServerInstaceType }}"
chef_server_instance_type = "{{ .AwsChefServerInstanceType }}"
elasticsearch_server_instance_type = "{{ .AwsElasticSearchServerInstaceType }}"
postgresql_server_instance_type = "{{ .AwsPostgresqlServerInstanceType }}"
automate_lb_certificate_arn = "{{ .AwsAutomateLBCertificateARN }}"
chef_server_lb_certificate_arn = "{{ .AwsChefServerLBCertificateARN }}"
automate_ebs_volume_iops = "{{ .AwsAutomateEbsVolumeIops }}"
automate_ebs_volume_size = "{{ .AwsAutomateEbsVolumeSize }}"
automate_ebs_volume_type = "{{ .AwsAutomateEbsVolumeType }}"
chef_ebs_volume_iops = "{{ .AwsChefEbsVolumeIops }}"
chef_ebs_volume_size = "{{ .AwsChefEbsVolumeSize }}"
chef_ebs_volume_type = "{{ .AwsChefEbsVolumeType }}"
elasticsearch_ebs_volume_iops = "{{ .AwsEsEbsVolumeIops }}"
elasticsearch_ebs_volume_size = "{{ .AwsEsEbsVolumeSize }}"
elasticsearch_ebs_volume_type = "{{ .AwsEsEbsVolumeType }}"
postgresql_ebs_volume_iops = "{{ .AwsPgsEbsVolumeIops }}"
postgresql_ebs_volume_size = "{{ .AwsPgsEbsVolumeSize }}"
postgresql_ebs_volume_type = "{{ .AwsPgsEbsVolumeType }}"
X-Contact = "{{ .AwsTagContact }}"
X-Dept = "{{ .AwsTagDept }}"
X-Project = "{{ .AwsTagProject }}"
`

const haExistingNodesConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.existing_nodes]
secrets_key_file = "{{ .SecretsKeyFile }}"
secrets_store_file = "{{ .SecretsStoreFile }}"
architecture = "{{ .Architecture }}"
workspace_path = "{{ .WorkspacePath }}"
ssh_user = "{{ .SshUser }}"
ssh_key_file = "{{ .SshKeyFile }}"
sudo_password = "{{ .SudoPassword }}"
logging_monitoring_management = "{{ .LoggingMonitoringManagement }}"
new_elk = "{{ .NewElk }}"
# existing_elk_instance_ip "{{ .ExistingElk }}"
# existing_elk_port "{{ .ExistingElkPort }}"
# existing_elk_cert "{{ .ExistingElkCert }}"
# existing_elk_username "{{ .ExistingElkUsername }}"
# existing_elk_password "{{ .ExistingElkPassword }}"
backup_mount = "{{ .BackupMount }}"

[automate.config]
# admin_password = "{{ .AutomateAdminPassword }}"
# fqdn = "{{ .AutomateFQDN }}"
instance_count = "{{ .AutomateInstanceCount }}"
# teams_port = "{{ .AutomateTeamsPort }}"
config_file = "{{ .AutomateConfigFile }}"

[chef_server.config]
instance_count = "{{ .ChefServerInstanceCount }}"

[elasticsearch.config]
instance_count = "{{ .ElasticSearchInstanceCount }}"

[postgresql.config]
instance_count = "{{ .PostgresqlInstanceCount }}"

[existing_nodes.config]
automate_ips = []
automate_private_ips = []
chef_server_ips = []
chef_server_private_ips = []
elasticsearch_ips = []
elasticsearch_private_ips = []
postgresql_ips = []
postgresql_private_ips = []
`
