package main

const existingNodesA2harbTemplate = `
secrets_key_file "{{ .Architecture.ConfigInitials.SecretsKeyFile }}"
secrets_store_file "{{ .Architecture.ConfigInitials.SecretsStoreFile }}"
architecture "{{ .Architecture.ConfigInitials.Architecture }}"
workspace_path "{{ .Architecture.ConfigInitials.WorkspacePath }}"
ssh_user "{{ .Architecture.ConfigInitials.SSHUser }}"
ssh_key_file "{{ .Architecture.ConfigInitials.SSHKeyFile }}"
# sudo_password "{{ .Architecture.ConfigInitials.SudoPassword }}"
# logging_monitoring_management "true"
# ew_elk "false"
# existing_elk "false"
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
backup_mount "{{ .Architecture.ConfigInitials.BackupMount }}"
# habitat_uid_gid ""
###############################################################
### Automate frontend node related settings                 ###
###############################################################
automate do
  # admin_password "{{ .Automate.Config.AdminPassword }}"
  ### Leave commented out if using AWS infrastructure
  fqdn "{{ .Automate.Config.Fqdn }}"
  instance_count {{ .Automate.Config.InstanceCount }}
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  # teams_port "{{ .Automate.Config.TeamsPort }}"
  config_file "{{ .Automate.Config.ConfigFile }}"
end

###############################################################
### Chef Server frontend node related settings              ###
###############################################################
chef_server do
  instance_count {{ .ChefServer.Config.InstanceCount }}
end

###############################################################
### Elasticsearch related settings                          ###
###############################################################
elasticsearch do
  instance_count {{ .Elasticsearch.Config.InstanceCount }}
end

###############################################################
### PostgreSQL related settings                             ###
###############################################################
postgresql do
  instance_count {{ .Postgresql.Config.InstanceCount }}
end

###############################################################
### Only applies when using an existing node architecture   ###
###############################################################
existing_nodes do
  automate_ips [{{ range $index, $element := .ExistingInfra.Config.AutomateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  automate_private_ips [{{ range $index, $element := .ExistingInfra.Config.AutomatePrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  chef_server_ips [{{ range $index, $element := .ExistingInfra.Config.ChefServerIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  chef_server_private_ips [{{ range $index, $element := .ExistingInfra.Config.ChefServerPrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  elasticsearch_ips [{{ range $index, $element := .ExistingInfra.Config.ElasticsearchIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  elasticsearch_private_ips [{{ range $index, $element := .ExistingInfra.Config.ElasticsearchPrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  postgresql_ips [{{ range $index, $element := .ExistingInfra.Config.PostgresqlIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  postgresql_private_ips [{{ range $index, $element := .ExistingInfra.Config.PostgresqlPrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
end
`

const awsA2harbTemplate = `
secrets_key_file "{{ .Architecture.ConfigInitials.SecretsKeyFile }}"
secrets_store_file "{{ .Architecture.ConfigInitials.SecretsStoreFile }}"
architecture "{{ .Architecture.ConfigInitials.Architecture }}"
workspace_path "{{ .Architecture.ConfigInitials.WorkspacePath }}"
ssh_user "{{ .Architecture.ConfigInitials.SSHUser }}"
ssh_key_file "{{ .Architecture.ConfigInitials.SSHKeyFile }}"
# logging_monitoring_management "true"
# new_elk "false"
# existing_elk "false"
backup_mount "{{ .Architecture.ConfigInitials.BackupMount }}"
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
  instance_count {{ .Automate.Config.InstanceCount }}
  config_file "{{ .Automate.Config.ConfigFile }}"
  # admin_password "{{ .Automate.Config.AdminPassword }}"
  ### Leave commented out if using AWS infrastructure
  # fqdn "{{ .Automate.Config.Fqdn }}"
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  # teams_port "{{ .Automate.Config.TeamsPort }}"
end

###############################################################
### Chef Server frontend node related settings              ###
###############################################################
chef_server do
  instance_count {{ .ChefServer.Config.InstanceCount }}
end

###############################################################
### Elasticsearch related settings                          ###
###############################################################
elasticsearch do
  instance_count {{ .Elasticsearch.Config.InstanceCount }}
end

###############################################################
### PostgreSQL related settings                             ###
###############################################################
postgresql do
  instance_count {{ .Postgresql.Config.InstanceCount }}
end

###############################################################
### Only applies when using AWS architecture                ###
###############################################################
aws do
  ### AWS Credentials profile to use when deploying AWS infrastructure
  profile "{{ .Aws.Config.Profile }}"
  region "{{ .Aws.Config.Region }}"
  vpc_id "{{ .Aws.Config.AwsVpcId }}"
  cidr_block_addr "{{ .Aws.Config.AwsCidrBlockAddr }}"
  ssh_key_pair_name "{{ .Aws.Config.SSHKeyPairName }}"
  ### Filter settings default to CentOS if left blank
  # ami_filter_name "{{ .Aws.Config.AmiFilterName }}"
  ### Filter settings default to CentOS if left blank
  # ami_filter_virt_type "{{ .Aws.Config.AmiFilterVirtType }}"
  ### Filter settings default to CentOS if left blank
  # ami_filter_owner "{{ .Aws.Config.AmiFilterOwner }}"
  ### Overrides ami filter search features
  # ami_id "{{ .Aws.Config.AmiID }}"
  ### EC2 instance type to use for Automate frontends, minimum >2G of RAM for test, 8G for prod
  automate_server_instance_type "{{ .Aws.Config.AutomateServerInstanceType }}"
  ### EC2 instance type to use for Chef Server frontends, minimum >2G of RAM for test, 8G for prod
  chef_server_instance_type "{{ .Aws.Config.ChefServerInstanceType }}"
  ### EC2 instance type to use for Elasticsearch backends, minimum 8G of RAM for test, 16G for prod
  elasticsearch_server_instance_type "{{ .Aws.Config.ElasticsearchServerInstanceType }}"
  ### EC2 instance type to use for PostgreSQL backends, minimum 4G of RAM for test, 8G for prod
  postgresql_server_instance_type "{{ .Aws.Config.PostgresqlServerInstanceType }}"
  ### AWS Certificate is specific to the region and AWS account this is being deployed to.
  automate_lb_certificate_arn "{{ .Aws.Config.AutomateLbCertificateArn }}"
  ### AWS Certificate is specific to the region and AWS account this is being deployed to.
  chef_server_lb_certificate_arn "{{ .Aws.Config.ChefServerLbCertificateArn }}"
  automate_ebs_volume_iops "{{ .Aws.Config.AutomateEbsVolumeIops }}"
  automate_ebs_volume_size "{{ .Aws.Config.AutomateEbsVolumeSize }}"
  automate_ebs_volume_type "{{ .Aws.Config.AutomateEbsVolumeType }}"
  chef_ebs_volume_iops "{{ .Aws.Config.ChefEbsVolumeIops }}"
  chef_ebs_volume_size "{{ .Aws.Config.ChefEbsVolumeSize }}"
  chef_ebs_volume_type "{{ .Aws.Config.ChefEbsVolumeType }}"
  elasticsearch_ebs_volume_iops "{{ .Aws.Config.ElasticsearchEbsVolumeIops }}"
  elasticsearch_ebs_volume_size "{{ .Aws.Config.ElasticsearchEbsVolumeSize }}"
  elasticsearch_ebs_volume_type "{{ .Aws.Config.ElasticsearchEbsVolumeType }}"
  postgresql_ebs_volume_iops "{{ .Aws.Config.PostgresqlEbsVolumeIops }}"
  postgresql_ebs_volume_size "{{ .Aws.Config.PostgresqlEbsVolumeSize }}"
  postgresql_ebs_volume_type "{{ .Aws.Config.PostgresqlEbsVolumeType }}"
  ### DEPRECATED: AWS Tag: Contact email to apply to AWS insfrastructure tags
  # contact "{{ .Aws.Config.XContact }}"
  ### DEPRECATED: AWS Tag: Department name to apply to AWS insfrastructure tags
  # dept "{{ .Aws.Config.XDept }}"
  ### DEPRECATED: AWS Tag: Project name to apply to AWS insfrastructure tags
  # project "{{ .Aws.Config.XProject }}"
  tags({"X-Contact"=>"{{ .Aws.Config.XContact }}", "X-Dept"=>"{{ .Aws.Config.XDept }}", "X-Project"=>"{{ .Aws.Config.XProject }}"})
end
`
