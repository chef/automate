resource "random_id" "cluster_id" {
  byte_length = 4
}

locals {
  backend_private_ips  = tolist(setunion(var.existing_opensearch_private_ips, var.existing_postgresql_private_ips))
  backend_count        = length(local.backend_private_ips)
  frontend_private_ips = tolist(setunion(var.existing_automate_private_ips, var.existing_chef_server_private_ips))
  frontend_count       = length(local.frontend_private_ips)
}

module "system-tuning-frontend" {
  source                          = "./modules/system"
  automate_archive_disk_fs_path   = var.automate_archive_disk_fs_path
  opensearch_archive_disk_fs_path = var.setup_managed_services ? "" : var.elasticsearch_archive_disk_fs_path
  instance_count                  = local.frontend_count
  postgresql_archive_disk_fs_path = var.setup_managed_services ? "" : var.postgresql_archive_disk_fs_path
  private_ips                     = local.frontend_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_group_name                  = var.ssh_group_name
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.fe_sudo_password
  sudo_cmd                        = var.sudo_cmd
}

module "system-tuning-backend" {
  source                          = "./modules/system"
  count                           = var.setup_managed_services ? 0 : 1
  automate_archive_disk_fs_path   = var.automate_archive_disk_fs_path
  opensearch_archive_disk_fs_path = var.setup_managed_services ? "" : var.elasticsearch_archive_disk_fs_path
  instance_count                  = local.backend_count
  postgresql_archive_disk_fs_path = var.setup_managed_services ? "" : var.postgresql_archive_disk_fs_path
  private_ips                     = local.backend_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_group_name                  = var.ssh_group_name
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.be_sudo_password
  sudo_cmd                        = var.sudo_cmd
}

module "airgap_bundle-backend" {
  source            = "./modules/airgap_bundle"
  count             = var.setup_managed_services ? 0 : 1
  archive_disk_info = var.setup_managed_services ? "" : module.system-tuning-backend[0].archive_disk_info
  instance_count    = local.backend_count
  private_ips       = local.backend_private_ips
  bundle_files = [{
    source      = var.backend_aib_local_file
    destination = var.backend_aib_dest_file
  }]
  ssh_key_file = var.ssh_key_file
  ssh_user     = var.ssh_user
  ssh_port     = var.ssh_port
  tmp_path     = var.tmp_path
  depends_on   = [module.system-tuning-backend]
}

module "airgap_bundle-frontend" {
  source            = "./modules/airgap_bundle"
  archive_disk_info = module.system-tuning-frontend.archive_disk_info
  instance_count    = local.frontend_count
  private_ips       = local.frontend_private_ips
  bundle_files = [{
    source      = var.backend_aib_local_file
    destination = var.backend_aib_dest_file
    }, {
    source      = var.frontend_aib_local_file
    destination = var.frontend_aib_dest_file
  }]
  ssh_key_file = var.ssh_key_file
  ssh_user     = var.ssh_user
  ssh_port     = var.ssh_port
  tmp_path     = var.tmp_path
  depends_on = [
    module.system-tuning-frontend
  ]
}

module "habitat-backend" {
  source                          = "./modules/habitat"
  count                           = var.setup_managed_services ? 0 : 1
  airgap_info                     = var.setup_managed_services ? "" : module.airgap_bundle-backend[0].airgap_info
  hab_sup_http_gateway_auth_token = var.hab_sup_http_gateway_auth_token
  hab_sup_http_gateway_ca_cert    = var.hab_sup_http_gateway_ca_cert
  hab_sup_http_gateway_priv_key   = var.hab_sup_http_gateway_priv_key
  hab_sup_http_gateway_pub_cert   = var.hab_sup_http_gateway_pub_cert
  hab_sup_ring_key                = var.hab_sup_ring_key
  hab_sup_run_args                = var.hab_sup_run_args
  install_hab_sh_args             = ""
  instance_count                  = local.backend_count
  backend_aib_dest_file           = var.backend_aib_dest_file
  backend_aib_local_file          = var.backend_aib_local_file
  private_ips                     = local.backend_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.be_sudo_password
  sudo_cmd                        = var.sudo_cmd
  habitat_uid_gid                 = var.habitat_uid_gid
  peer_ips = setunion(
    var.existing_opensearch_private_ips,
    var.existing_postgresql_private_ips
  )
  depends_on = [module.airgap_bundle-backend]
}

module "habitat-frontend" {
  source                          = "./modules/habitat"
  airgap_info                     = module.airgap_bundle-frontend.airgap_info
  hab_sup_http_gateway_auth_token = var.hab_sup_http_gateway_auth_token
  hab_sup_http_gateway_ca_cert    = var.hab_sup_http_gateway_ca_cert
  hab_sup_http_gateway_priv_key   = var.hab_sup_http_gateway_priv_key
  hab_sup_http_gateway_pub_cert   = var.hab_sup_http_gateway_pub_cert
  hab_sup_ring_key                = var.hab_sup_ring_key
  hab_sup_run_args                = var.hab_sup_run_args
  install_hab_sh_args             = "--no-service"
  instance_count                  = local.frontend_count
  backend_aib_dest_file           = var.backend_aib_dest_file
  backend_aib_local_file          = var.backend_aib_local_file
  private_ips                     = local.frontend_private_ips
  peer_ips                        = local.frontend_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.fe_sudo_password
  sudo_cmd                        = var.sudo_cmd
  habitat_uid_gid                 = var.habitat_uid_gid
  depends_on                      = [module.airgap_bundle-frontend]
}

module "opensearch" {
  source                          = "./modules/opensearch"
  count                           = var.setup_managed_services ? 0 : 1
  airgap_info                     = var.setup_managed_services ? "" : module.airgap_bundle-backend[0].airgap_info
  backend_aib_dest_file           = var.backend_aib_dest_file
  backend_aib_local_file          = var.backend_aib_local_file
  curator_pkg_ident               = var.curator_pkg_ident
  opensearch_instance_count       = var.opensearch_instance_count
  opensearch_listen_port          = var.opensearch_listen_port
  opensearch_pkg_ident            = var.opensearch_pkg_ident
  opensearch_svc_load_args        = var.elasticsearch_svc_load_args
  opensearchsidecar_pkg_ident     = var.elasticsidecar_pkg_ident
  opensearchsidecar_svc_load_args = var.elasticsidecar_svc_load_args
  habitat_info                    = var.setup_managed_services ? "" : module.habitat-backend[0].habitat_info
  journalbeat_pkg_ident           = var.journalbeat_pkg_ident
  kibana_pkg_ident                = var.kibana_pkg_ident
  metricbeat_pkg_ident            = var.metricbeat_pkg_ident
  private_ips                     = var.existing_opensearch_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.be_sudo_password
  sudo_cmd                        = var.sudo_cmd
  backup_config_s3                = var.backup_config_s3
  backup_config_efs               = var.backup_config_efs
  s3_endpoint                     = var.s3_endpoint
  nfs_mount_path                  = var.nfs_mount_path
  access_key                      = var.access_key
  secret_key                      = var.secret_key
  opensearch_username             = var.opensearch_username
  opensearch_user_password        = var.opensearch_user_password
  depends_on                      = [module.airgap_bundle-backend, module.habitat-backend]
  opensearch_root_ca              = trimspace(var.opensearch_root_ca)
  opensearch_private_key          = trimspace(var.opensearch_private_key)
  opensearch_public_key           = trimspace(var.opensearch_public_key)
  opensearch_admin_key            = trimspace(var.opensearch_admin_key)
  opensearch_admin_cert           = trimspace(var.opensearch_admin_cert)
  opensearch_admin_dn             = var.opensearch_admin_dn
  opensearch_nodes_dn             = var.opensearch_nodes_dn
  opensearch_custom_certs_enabled = var.opensearch_custom_certs_enabled
  opensearch_certs_by_ip          = var.opensearch_certs_by_ip
}

module "postgresql" {
  count                           = var.setup_managed_services ? 0 : 1
  source                          = "./modules/postgresql"
  airgap_info                     = var.setup_managed_services ? "" : module.airgap_bundle-backend[0].airgap_info
  backend_aib_dest_file           = var.backend_aib_dest_file
  backend_aib_local_file          = var.backend_aib_local_file
  opensearch_listen_port          = var.opensearch_listen_port
  opensearch_private_ips          = var.existing_opensearch_private_ips
  habitat_info                    = var.setup_managed_services ? "" : module.habitat-backend[0].habitat_info
  journalbeat_pkg_ident           = var.journalbeat_pkg_ident
  metricbeat_pkg_ident            = var.metricbeat_pkg_ident
  pgleaderchk_listen_port         = var.pgleaderchk_listen_port
  pgleaderchk_pkg_ident           = var.pgleaderchk_pkg_ident
  pgleaderchk_svc_load_args       = var.pgleaderchk_svc_load_args
  postgresql_archive_disk_fs_path = var.setup_managed_services ? "" : var.postgresql_archive_disk_fs_path
  postgresql_instance_count       = var.postgresql_instance_count
  postgresql_listen_port          = var.postgresql_listen_port
  postgresql_pkg_ident            = var.postgresql_pkg_ident
  postgresql_pg_dump_enabled      = var.postgresql_pg_dump_enabled
  postgresql_ssl_enable           = var.postgresql_ssl_enable
  postgresql_svc_load_args        = var.postgresql_svc_load_args
  postgresql_wal_archive_enabled  = var.postgresql_wal_archive_enabled
  proxy_listen_port               = var.proxy_listen_port
  proxy_pkg_ident                 = var.proxy_pkg_ident
  proxy_svc_load_args             = var.proxy_svc_load_args
  private_ips                     = var.existing_postgresql_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.be_sudo_password
  sudo_cmd                        = var.sudo_cmd
  backup_config_efs               = var.backup_config_efs
  nfs_mount_path                  = var.nfs_mount_path
  depends_on                      = [module.airgap_bundle-backend, module.habitat-backend]
  postgresql_root_ca              = trimspace(var.postgresql_root_ca)
  postgresql_private_key          = trimspace(var.postgresql_private_key)
  postgresql_public_key           = trimspace(var.postgresql_public_key)
  postgresql_custom_certs_enabled = var.postgresql_custom_certs_enabled
  postgresql_certs_by_ip          = var.postgresql_certs_by_ip
}

module "bootstrap_automate" {
  source                             = "./modules/automate"
  airgap_info                        = module.airgap_bundle-frontend.airgap_info
  automate_admin_email               = var.automate_admin_email
  automate_admin_username            = var.automate_admin_username
  automate_admin_password            = var.automate_admin_password
  automate_config                    = file(var.automate_config_file)
  automate_root_ca                   = trimspace(var.automate_root_ca)
  opensearch_root_ca                 = trimspace(var.opensearch_root_ca)
  postgresql_root_ca                 = trimspace(var.postgresql_root_ca)
  automate_private_key               = trimspace(var.automate_private_key)
  automate_public_key                = trimspace(var.automate_public_key)
  chef_server_private_key            = trimspace(var.chef_server_private_key)
  chef_server_public_key             = trimspace(var.chef_server_public_key)
  automate_custom_certs_enabled      = var.automate_custom_certs_enabled
  chef_server_custom_certs_enabled   = var.chef_server_custom_certs_enabled
  postgresql_custom_certs_enabled    = var.postgresql_custom_certs_enabled
  opensearch_custom_certs_enabled    = var.opensearch_custom_certs_enabled
  automate_certs_by_ip               = var.automate_certs_by_ip
  opensearch_root_cert               = var.opensearch_root_cert
  postgresql_root_cert               = var.postgresql_root_cert
  managed_opensearch_domain_name     = var.managed_opensearch_domain_name
  managed_opensearch_certificate     = var.managed_opensearch_certificate
  managed_opensearch_domain_url      = var.managed_opensearch_domain_url
  managed_opensearch_user_password   = var.managed_opensearch_user_password
  managed_opensearch_username        = var.managed_opensearch_username
  aws_os_snapshot_role_arn           = var.aws_os_snapshot_role_arn
  os_snapshot_user_access_key_id     = var.os_snapshot_user_access_key_id
  os_snapshot_user_access_key_secret = var.os_snapshot_user_access_key_secret
  managed_rds_instance_url           = var.managed_rds_instance_url
  managed_rds_superuser_username     = var.managed_rds_superuser_username
  managed_rds_superuser_password     = var.managed_rds_superuser_password
  managed_rds_dbuser_username        = var.managed_rds_dbuser_username
  managed_rds_dbuser_password        = var.managed_rds_dbuser_password
  managed_rds_certificate            = var.managed_rds_certificate
  setup_managed_services             = var.setup_managed_services
  setup_self_managed_services        = var.setup_self_managed_services
  automate_dc_token                  = var.automate_dc_token
  automate_fqdn                      = var.automate_fqdn
  automate_instance_count            = 1
  automate_role                      = "bootstrap_automate"
  cluster_id                         = random_id.cluster_id.hex
  backend_aib_dest_file              = var.backend_aib_dest_file
  backend_aib_local_file             = var.backend_aib_local_file
  frontend_aib_dest_file             = var.frontend_aib_dest_file
  frontend_aib_local_file            = var.frontend_aib_local_file  
  habitat_info                       = module.habitat-frontend.habitat_info
  hab_sup_http_gateway_auth_token    = var.hab_sup_http_gateway_auth_token
  opensearch_listen_port             = var.opensearch_listen_port
  opensearch_private_ips             = var.existing_opensearch_private_ips
  proxy_listen_port                  = var.proxy_listen_port
  postgresql_private_ips             = var.existing_postgresql_private_ips
  postgresql_ssl_enable              = var.postgresql_ssl_enable
  private_ips                        = slice(var.existing_automate_private_ips, 0, 1)
  ssh_key_file                       = var.ssh_key_file
  ssh_user                           = var.ssh_user
  ssh_port                           = var.ssh_port
  ssh_user_sudo_password             = local.fe_sudo_password
  sudo_cmd                           = var.sudo_cmd
  teams_port                         = var.teams_port
  backup_config_s3                   = var.backup_config_s3
  backup_config_efs                  = var.backup_config_efs
  s3_endpoint                        = var.s3_endpoint
  bucket_name                        = var.bucket_name
  access_key                         = var.access_key
  secret_key                         = var.secret_key
  aws_region                         = var.region
  infra                              = var.infra
  nfs_mount_path                     = var.nfs_mount_path
  depends_on                         = [module.airgap_bundle-frontend, module.habitat-frontend]
}

module "automate" {
  source                             = "./modules/automate"
  airgap_info                        = module.airgap_bundle-frontend.airgap_info
  automate_admin_email               = var.automate_admin_email
  automate_admin_username            = var.automate_admin_username
  automate_admin_password            = var.automate_admin_password
  automate_config                    = file(var.automate_config_file)
  automate_root_ca                   = trimspace(var.automate_root_ca)
  opensearch_root_ca                 = trimspace(var.opensearch_root_ca)
  postgresql_root_ca                 = trimspace(var.postgresql_root_ca)
  automate_private_key               = trimspace(var.automate_private_key)
  automate_public_key                = trimspace(var.automate_public_key)
  chef_server_private_key            = trimspace(var.chef_server_private_key)
  chef_server_public_key             = trimspace(var.chef_server_public_key)
  automate_custom_certs_enabled      = var.automate_custom_certs_enabled
  chef_server_custom_certs_enabled   = var.chef_server_custom_certs_enabled
  postgresql_custom_certs_enabled    = var.postgresql_custom_certs_enabled
  opensearch_custom_certs_enabled    = var.opensearch_custom_certs_enabled
  automate_certs_by_ip               = var.automate_certs_by_ip
  managed_opensearch_domain_name     = var.managed_opensearch_domain_name
  managed_opensearch_certificate     = var.managed_opensearch_certificate
  managed_opensearch_domain_url      = var.managed_opensearch_domain_url
  managed_opensearch_user_password   = var.managed_opensearch_user_password
  managed_opensearch_username        = var.managed_opensearch_username
  aws_os_snapshot_role_arn           = var.aws_os_snapshot_role_arn
  os_snapshot_user_access_key_id     = var.os_snapshot_user_access_key_id
  os_snapshot_user_access_key_secret = var.os_snapshot_user_access_key_secret
  managed_rds_instance_url           = var.managed_rds_instance_url
  managed_rds_superuser_username     = var.managed_rds_superuser_username
  managed_rds_superuser_password     = var.managed_rds_superuser_password
  managed_rds_dbuser_username        = var.managed_rds_dbuser_username
  managed_rds_dbuser_password        = var.managed_rds_dbuser_password
  managed_rds_certificate            = var.managed_rds_certificate
  setup_managed_services             = var.setup_managed_services
  opensearch_root_cert               = var.opensearch_root_cert
  postgresql_root_cert               = var.postgresql_root_cert
  setup_self_managed_services        = var.setup_self_managed_services
  automate_dc_token                  = var.automate_dc_token
  automate_fqdn                      = var.automate_fqdn
  automate_instance_count            = var.automate_instance_count - 1
  automate_role                      = "automate"            
  cluster_id                         = random_id.cluster_id.hex
  backend_aib_dest_file              = var.backend_aib_dest_file
  frontend_aib_dest_file             = var.frontend_aib_dest_file
  backend_aib_local_file             = var.backend_aib_local_file
  frontend_aib_local_file            = var.frontend_aib_local_file
  habitat_info                       = module.habitat-frontend.habitat_info
  hab_sup_http_gateway_auth_token    = var.hab_sup_http_gateway_auth_token
  opensearch_listen_port             = var.opensearch_listen_port
  opensearch_private_ips             = var.existing_opensearch_private_ips
  proxy_listen_port                  = var.proxy_listen_port
  postgresql_private_ips             = var.existing_postgresql_private_ips
  postgresql_ssl_enable              = var.postgresql_ssl_enable
  private_ips = slice(
    var.existing_automate_private_ips,
    1,
    length(var.existing_automate_private_ips),
  )
  ssh_key_file                       = var.ssh_key_file
  ssh_user                           = var.ssh_user
  ssh_port                           = var.ssh_port
  ssh_user_sudo_password             = local.fe_sudo_password
  sudo_cmd                           = var.sudo_cmd
  teams_port                         = var.teams_port
  backup_config_s3                   = var.backup_config_s3
  backup_config_efs                  = var.backup_config_efs
  s3_endpoint                        = var.s3_endpoint
  bucket_name                        = var.bucket_name
  access_key                         = var.access_key
  secret_key                         = var.secret_key
  aws_region                         = var.region
  infra                              = var.infra
  nfs_mount_path                     = var.nfs_mount_path
  depends_on                         = [module.bootstrap_automate]
}

module "chef_server" {
  source                             = "./modules/automate"
  airgap_info                        = module.airgap_bundle-frontend.airgap_info
  automate_admin_email               = var.automate_admin_email
  automate_admin_username            = var.automate_admin_username
  automate_admin_password            = var.automate_admin_password
  automate_config                    = file(var.automate_config_file)
  automate_root_ca                   = trimspace(var.automate_root_ca)
  opensearch_root_ca                 = trimspace(var.opensearch_root_ca)
  postgresql_root_ca                 = trimspace(var.postgresql_root_ca)
  automate_private_key               = trimspace(var.automate_private_key)
  automate_public_key                = trimspace(var.automate_public_key)
  chef_server_private_key            = trimspace(var.chef_server_private_key)
  chef_server_public_key             = trimspace(var.chef_server_public_key)
  automate_custom_certs_enabled      = var.automate_custom_certs_enabled
  chef_server_custom_certs_enabled   = var.chef_server_custom_certs_enabled
  postgresql_custom_certs_enabled    = var.postgresql_custom_certs_enabled
  opensearch_custom_certs_enabled    = var.opensearch_custom_certs_enabled
  chef_server_certs_by_ip            = var.chef_server_certs_by_ip
  managed_opensearch_domain_name     = var.managed_opensearch_domain_name
  managed_opensearch_certificate     = var.managed_opensearch_certificate
  managed_opensearch_domain_url      = var.managed_opensearch_domain_url
  managed_opensearch_user_password   = var.managed_opensearch_user_password
  managed_opensearch_username        = var.managed_opensearch_username
  aws_os_snapshot_role_arn           = var.aws_os_snapshot_role_arn
  os_snapshot_user_access_key_id     = var.os_snapshot_user_access_key_id
  os_snapshot_user_access_key_secret = var.os_snapshot_user_access_key_secret
  managed_rds_instance_url           = var.managed_rds_instance_url
  managed_rds_superuser_username     = var.managed_rds_superuser_username
  managed_rds_superuser_password     = var.managed_rds_superuser_password
  managed_rds_dbuser_username        = var.managed_rds_dbuser_username
  managed_rds_dbuser_password        = var.managed_rds_dbuser_password
  managed_rds_certificate            = var.managed_rds_certificate
  setup_managed_services             = var.setup_managed_services
  opensearch_root_cert               = var.opensearch_root_cert
  postgresql_root_cert               = var.postgresql_root_cert
  setup_self_managed_services        = var.setup_self_managed_services
  automate_dc_token                  = var.automate_dc_token
  automate_fqdn                      = var.automate_fqdn
  automate_instance_count            = length(setsubtract(var.existing_chef_server_private_ips, var.existing_automate_private_ips))
  automate_role                      = "chef_api"
  cluster_id                         = random_id.cluster_id.hex
  backend_aib_dest_file              = var.backend_aib_dest_file
  backend_aib_local_file             = var.backend_aib_local_file
  frontend_aib_dest_file             = var.frontend_aib_dest_file
  frontend_aib_local_file            = var.frontend_aib_local_file
  habitat_info                       = module.habitat-frontend.habitat_info
  hab_sup_http_gateway_auth_token    = var.hab_sup_http_gateway_auth_token
  opensearch_listen_port             = var.opensearch_listen_port
  opensearch_private_ips             = var.existing_opensearch_private_ips
  proxy_listen_port                  = var.proxy_listen_port
  postgresql_private_ips             = var.existing_postgresql_private_ips
  postgresql_ssl_enable              = var.postgresql_ssl_enable
  private_ips                        = var.existing_chef_server_private_ips
  ssh_key_file                       = var.ssh_key_file
  ssh_user                           = var.ssh_user
  ssh_port                           = var.ssh_port
  ssh_user_sudo_password             = local.fe_sudo_password
  sudo_cmd                           = var.sudo_cmd
  teams_port                         = var.teams_port
  backup_config_s3                   = var.backup_config_s3
  backup_config_efs                  = var.backup_config_efs
  s3_endpoint                        = var.s3_endpoint
  bucket_name                        = var.bucket_name
  access_key                         = var.access_key
  secret_key                         = var.secret_key
  aws_region                         = var.region
  infra                              = var.infra
  nfs_mount_path                     = var.nfs_mount_path
  depends_on                         = [module.bootstrap_automate]
}

module "keystore_opensearch" {
  source                          = "./modules/keystoreopensearch"
  count                           = var.setup_managed_services ? 0 : 1
  opensearch_instance_count       = var.opensearch_instance_count
  opensearch_listen_port          = var.opensearch_listen_port
  opensearch_pkg_ident            = var.opensearch_pkg_ident
  private_ips                     = var.existing_opensearch_private_ips
  ssh_key_file                    = var.ssh_key_file
  ssh_user                        = var.ssh_user
  ssh_port                        = var.ssh_port
  ssh_user_sudo_password          = local.be_sudo_password
  sudo_cmd                        = var.sudo_cmd
  backup_config_s3                = var.backup_config_s3
  access_key                      = var.access_key
  secret_key                      = var.secret_key
  tmp_path                        = var.tmp_path
  depends_on                      = [module.bootstrap_automate]
}
