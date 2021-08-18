package main

const existingNodesA2harbTemplate = `
secrets_key_file "{{ .SecretsKeyFile }}"
secrets_store_file "{{ .SecretsStoreFile }}"
architecture "{{ .Architecture }}"
workspace_path "{{ .WorkspacePath }}"
ssh_user "{{ .SshUser }}"
ssh_key_file "{{ .SshKeyFile }}"
# sudo_password "{{ .SudoPassword }}"
# logging_monitoring_management "true"
# ew_elk "false"
# existing_elk "false"
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
backup_mount "{{ .BackupMount }}"
# habitat_uid_gid ""
###############################################################
### Automate frontend node related settings                 ###
###############################################################
automate do
  # admin_password "{{ .AutomateAdminPassword }}"
  ### Leave commented out if using AWS infrastructure
  # fqdn "{{ .AutomateFQDN }}"
  instance_count {{ .AutomateInstanceCount }}
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  # teams_port "{{ .AutomateTeamsPort }}"
  config_file "{{ .AutomateConfigFile }}"
end

###############################################################
### Chef Server frontend node related settings              ###
###############################################################
chef_server do
  instance_count {{ .ChefServerInstanceCount }}
end

###############################################################
### Elasticsearch related settings                          ###
###############################################################
elasticsearch do
  instance_count {{ .ElasticSearchInstanceCount }}
end

###############################################################
### PostgreSQL related settings                             ###
###############################################################
postgresql do
  instance_count {{ .PostgresqlInstanceCount }}
end

###############################################################
### Only applies when using an existing node architecture   ###
###############################################################
existing_nodes do
  automate_ips {{ .ExistingNodesAutomateIPs }}
  automate_private_ips {{ .ExistingNodesAutomatePrivateIPs }}
  chef_server_ips {{ .ExistingNodesChefServerIPs }}
  chef_server_private_ips {{ .ExistingNodesChefServerPrivateIPs }}
  elasticsearch_ips {{ .ExistingNodesElasticsearchIPs }}
  elasticsearch_private_ips {{ .ExistingNodesElasticsearchPrivateIPs }}
  postgresql_ips {{ .ExistingNodesPostgresqlIPs }}
  postgresql_private_ips {{ .ExistingNodesPostgresqlPrivateIps }}
end
`

const awsA2harbTemplate = `
secrets_key_file "{{ .SecretsKeyFile }}"
secrets_store_file "{{ .SecretsStoreFile }}"
architecture "{{ .Architecture }}"
workspace_path "{{ .WorkspacePath }}"
ssh_user "{{ .SshUser }}"
ssh_key_file "{{ .SshKeyFile }}"
# logging_monitoring_management "true"
# new_elk "false"
# existing_elk "false"
backup_mount "{{ .BackupMount }}"
# sudo_password ""
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
# habitat_uid_gid ""
###############################################################
### Automate frontend node related settings                 ###
###############################################################
automate do
  instance_count 1
  config_file "/src/configs/automate.toml"
  # admin_password "{{ .AutomateAdminPassword }}"
  ### Leave commented out if using AWS infrastructure
  # fqdn "{{ .AutomateFQDN }}"
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  # teams_port ""
end

###############################################################
### Chef Server frontend node related settings              ###
###############################################################
chef_server do
  instance_count 1
end

###############################################################
### Elasticsearch related settings                          ###
###############################################################
elasticsearch do
  instance_count 3
end

###############################################################
### PostgreSQL related settings                             ###
###############################################################
postgresql do
  instance_count 3
end

###############################################################
### Only applies when using AWS architecture                ###
###############################################################
aws do
  ### AWS Credentials profile to use when deploying AWS infrastructure
  profile "{{ .AwsProfile }}"
  region "{{ .AwsRegion }}"
  # ssh_key_pair_name "{{ .AwsSshKeyPairName }}"
  ### Filter settings default to CentOS if left blank
  # ami_filter_name ""
  ### Filter settings default to CentOS if left blank
  # ami_filter_virt_type ""
  ### Filter settings default to CentOS if left blank
  # ami_filter_owner ""
  ### Overrides ami filter search features
  # ami_id ""
  ### EC2 instance type to use for Automate frontends, minimum >2G of RAM for test, 8G for prod
  automate_server_instance_type "{{ .AwsAutomateServerInstaceType }}"
  ### EC2 instance type to use for Chef Server frontends, minimum >2G of RAM for test, 8G for prod
  chef_server_instance_type "{{ .AwsChefServerInstanceType }}"
  ### EC2 instance type to use for Elasticsearch backends, minimum 8G of RAM for test, 16G for prod
  elasticsearch_server_instance_type "{{ .AwsElasticSearchServerInstaceType }}"
  ### EC2 instance type to use for PostgreSQL backends, minimum 4G of RAM for test, 8G for prod
  postgresql_server_instance_type "{{ .AwsPostgresqlServerInstanceType }}"
  ### AWS Certificate is specific to the region and AWS account this is being deployed to.
  automate_lb_certificate_arn "{{ .AwsAutomateLBCertificateARN }}"
  ### AWS Certificate is specific to the region and AWS account this is being deployed to.
  chef_server_lb_certificate_arn "{{ .AwsChefServerLBCertificateARN }}"
  automate_ebs_volume_iops "{{ .AwsAutomateEbsVolumeIops }}"
  automate_ebs_volume_size "{{ .AwsAutomateEbsVolumeSize }}"
  automate_ebs_volume_type "{{ .AwsAutomateEbsVolumeType }}"
  chef_ebs_volume_iops "{{ .AwsChefEbsVolumeIops }}"
  chef_ebs_volume_size "{{ .AwsChefEbsVolumeSize }}"
  chef_ebs_volume_type "{{ .AwsChefEbsVolumeType }}"
  elasticsearch_ebs_volume_iops "{{ .AwsEsEbsVolumeIops }}"
  elasticsearch_ebs_volume_size "{{ .AwsEsEbsVolumeSize }}"
  elasticsearch_ebs_volume_type "{{ .AwsEsEbsVolumeType }}"
  postgresql_ebs_volume_iops "{{ .AwsPgsEbsVolumeIops }}"
  postgresql_ebs_volume_size "{{ .AwsPgsEbsVolumeSize }}"
  postgresql_ebs_volume_type "{{ .AwsPgsEbsVolumeType }}"
  ### DEPRECATED: AWS Tag: Contact email to apply to AWS insfrastructure tags
  # contact "{{ .AwsTagContact }}"
  ### DEPRECATED: AWS Tag: Department name to apply to AWS insfrastructure tags
  # dept "{{ .AwsTagDept }}"
  ### DEPRECATED: AWS Tag: Project name to apply to AWS insfrastructure tags
  # project "{{ .AwsTagProject }}"
  tags({"X-Contact"=>"{{ .AwsTagContact }}", "X-Dept"=>"{{ .AwsTagDept }}", "X-Project"=>"{{ .AwsTagProject }}"})
end
`
