package main

const existingNodesA2harbTemplate = `
secrets_key_file "{{ .Architecture.ConfigInitials.SecretsKeyFile }}"
secrets_store_file "{{ .Architecture.ConfigInitials.SecretsStoreFile }}"
architecture "{{ .Architecture.ConfigInitials.Architecture }}"
workspace_path "{{ .Architecture.ConfigInitials.WorkspacePath }}"
ssh_user "{{ .Architecture.ConfigInitials.SSHUser }}"
ssh_key_file "{{ .Architecture.ConfigInitials.SSHKeyFile }}"
{{ if .Architecture.ConfigInitials.SudoPassword }} sudo_password "{{ .Architecture.ConfigInitials.SudoPassword }}" {{ else }} # sudo_password "{{ .Architecture.ConfigInitials.SudoPassword }}" {{ end }}

# logging_monitoring_management "true"
# ew_elk "false"
# existing_elk "false"
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
backup_mount "{{ .Architecture.ConfigInitials.BackupMount }}"
{{ if .Architecture.ConfigInitials.HabitatUIDGid }} habitat_uid_gid "{{ .Architecture.ConfigInitials.HabitatUIDGid }}" {{ else }} # habitat_uid_gid "{{ .Architecture.ConfigInitials.HabitatUIDGid }}" {{ end }}
###############################################################
### Automate frontend node related settings                 ###
###############################################################
automate do
  {{ if .Automate.Config.AdminPassword }} admin_password "{{ .Automate.Config.AdminPassword }}" {{ else }} # admin_password "{{ .Automate.Config.AdminPassword }}" {{ end }}
  ### Leave commented out if using AWS infrastructure
  {{ if .Automate.Config.Fqdn }} fqdn "{{ .Automate.Config.Fqdn }}" {{ else }} # fqdn "{{ .Automate.Config.Fqdn }}" {{ end }}
  instance_count {{ .Automate.Config.InstanceCount }}
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  {{ if .Automate.Config.TeamsPort }} teams_port "{{ .Automate.Config.TeamsPort }}" {{ else }} # teams_port "{{ .Automate.Config.TeamsPort }}" {{ end }}
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
  automate_private_ips [{{ range $index, $element := .ExistingInfra.Config.AutomatePrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  chef_server_private_ips [{{ range $index, $element := .ExistingInfra.Config.ChefServerPrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  elasticsearch_ips [{{ range $index, $element := .ExistingInfra.Config.ElasticsearchIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
  elasticsearch_private_ips [{{ range $index, $element := .ExistingInfra.Config.ElasticsearchPrivateIps}}{{if $index}},{{end}}"{{$element}}"{{end}}]
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
backup_mount "{{ .Architecture.ConfigInitials.BackupMount }}"
backup_config "{{ .Architecture.ConfigInitials.BackupConfig }}"
{{ if  .Architecture.ConfigInitials.S3BucketName }} s3_bucketName "{{ .Architecture.ConfigInitials.S3BucketName }}" {{ else }} # s3_bucketName "{{ .Architecture.ConfigInitials.S3BucketName }}" {{ end }}
{{ if .Architecture.ConfigInitials.SudoPassword }} sudo_password "{{ .Architecture.ConfigInitials.SudoPassword }}" {{ else }} # sudo_password "{{ .Architecture.ConfigInitials.SudoPassword }}" {{ end }}
# logging_monitoring_management "true"
# new_elk "false"
# existing_elk "false"
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
{{ if .Architecture.ConfigInitials.HabitatUIDGid }} habitat_uid_gid "{{ .Architecture.ConfigInitials.HabitatUIDGid }}" {{ else }} # habitat_uid_gid "{{ .Architecture.ConfigInitials.HabitatUIDGid }}" {{ end }}
###############################################################
### Automate frontend node related settings                 ###
###############################################################
automate do
  instance_count {{ .Automate.Config.InstanceCount }}
  config_file "{{ .Automate.Config.ConfigFile }}"
  {{ if .Automate.Config.AdminPassword }} admin_password "{{ .Automate.Config.AdminPassword }}" {{ else }} # admin_password "{{ .Automate.Config.AdminPassword }}" {{ end }}
  ### Leave commented out if using AWS infrastructure
  {{ if .Automate.Config.Fqdn }} fqdn "{{ .Automate.Config.Fqdn }}" {{ else }} # fqdn "{{ .Automate.Config.Fqdn }}" {{ end }}
  ### Uncomment and set this value if the teams service
  ### port (default: 10128) conflicts with another service.
  {{ if .Automate.Config.TeamsPort }} teams_port "{{ .Automate.Config.TeamsPort }}" {{ else }} # teams_port "{{ .Automate.Config.TeamsPort }}" {{ end }}
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
  ### If lb_access logs is true then provide your s3 bucket name in next field s3_bucket_name_lb_access otherwise make it false
  lb_access_logs "{{ .Aws.Config.LBAccessLogs }}"
  setup_managed_services {{ .Aws.Config.SetupManagedServices }}
  {{ if .Aws.Config.SetupManagedServices }}managed_elasticsearch_domain_url "{{ .Aws.Config.ElasticsearchDomainUrl }}" {{ else }}#managed_elasticsearch_domain_url "{{ .Aws.Config.ElasticsearchDomainUrl }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_elasticsearch_username "{{ .Aws.Config.ElasticsearchUsername }}" {{ else }}#managed_elasticsearch_username "{{ .Aws.Config.ElasticsearchUsername }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_elasticsearch_user_password "{{ .Aws.Config.ElasticsearchUserPassword }}" {{ else }}#managed_elasticsearch_user_password "{{ .Aws.Config.ElasticsearchUserPassword }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_elasticsearch_certificate "{{ .Aws.Config.ElasticsearchCertificate }}" {{ else }}#managed_elasticsearch_certificate "{{ .Aws.Config.ElasticsearchCertificate }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_instance_url "{{ .Aws.Config.RDSInstanceUrl }}" {{ else }}#managed_rds_instance_url "{{ .Aws.Config.RDSInstanceUrl }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_superuser_username "{{ .Aws.Config.RDSSuperUserName }}" {{ else }}#managed_rds_superuser_username "{{ .Aws.Config.RDSSuperUserName }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_superuser_password "{{ .Aws.Config.RDSSuperUserPassword }}" {{ else }}#managed_rds_superuser_password "{{ .Aws.Config.RDSSuperUserPassword }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_dbuser_username "{{ .Aws.Config.RDSDBUserName }}" {{ else }}#managed_rds_dbuser_username "{{ .Aws.Config.RDSDBUserName }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_dbuser_password "{{ .Aws.Config.RDSDBUserPassword }}" {{ else }}#managed_rds_dbuser_password "{{ .Aws.Config.RDSDBUserPassword }}" {{ end }}
  {{ if .Aws.Config.SetupManagedServices }}managed_rds_certificate "{{ .Aws.Config.RDSCertificate }}" {{ else }}#managed_rds_certificate "{{ .Aws.Config.RDSCertificate }}" {{ end }}
  ### Filter settings default to CentOS if left blank
  {{ if .Aws.Config.AmiFilterName }} ami_filter_name "{{ .Aws.Config.AmiFilterName }}" {{ else }} # ami_filter_name "{{ .Aws.Config.AmiFilterName }}" {{ end }}
  ### Filter settings default to CentOS if left blank
  {{ if .Aws.Config.AmiFilterVirtType }} ami_filter_virt_type "{{ .Aws.Config.AmiFilterVirtType }}" {{ else }} # ami_filter_virt_type "{{ .Aws.Config.AmiFilterVirtType }}" {{ end }}
  ### Filter settings default to CentOS if left blank
  {{ if .Aws.Config.AmiFilterOwner }} ami_filter_owner "{{ .Aws.Config.AmiFilterOwner }}" {{ else }} # ami_filter_owner "{{ .Aws.Config.AmiFilterOwner }}" {{ end }}
  ### Overrides ami filter search features
  {{ if .Aws.Config.AmiID }} ami_id "{{ .Aws.Config.AmiID }}" {{ else }} # ami_id "{{ .Aws.Config.AmiID }}" {{ end }}
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
  {{ if .Aws.Config.XContact }} contact "{{ .Aws.Config.XContact }}" {{ else }} # contact "{{ .Aws.Config.XContact }}" {{ end }}
  ### DEPRECATED: AWS Tag: Department name to apply to AWS insfrastructure tags
  {{ if .Aws.Config.XDept }} dept "{{ .Aws.Config.XDept }}" {{ else }} # dept "{{ .Aws.Config.XDept }}" {{ end }}
  ### DEPRECATED: AWS Tag: Project name to apply to AWS insfrastructure tags
  {{ if .Aws.Config.XProject }} project "{{ .Aws.Config.XProject }}" {{ else }}  project "{{ .Aws.Config.XProject }}" {{ end }}
  tags({"X-Contact"=>"{{ .Aws.Config.XContact }}", "X-Dept"=>"{{ .Aws.Config.XDept }}", "X-Project"=>"{{ .Aws.Config.XProject }}"})
end
`
