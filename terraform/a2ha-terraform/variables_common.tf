variable "automate_admin_email" {
  description = "Email address for the automate admin user account"
  default     = "admin@example.com"
}

variable "automate_admin_username" {
  description = "User name for the automate admin user account"
  default     = "admin"
}

variable "automate_admin_password" {
  description = "Password for the automate admin user account"
  default     = "c0d3c@n!"
}

variable "automate_config_file" {
  description = "Custom config file to use when setting up automate"
  default     = ""
}

variable "automate_dc_token" {
  description = "Token for the Automate Data Collector API"
  default     = "93a49a4f2482c64126f7b6015e6b0f30284287ee4054ff8807fb63d9cbd1c506"
}

variable "automate_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/automate"
  description = "The filesystem path to the Automate frontend archive share."
}

variable "artifact_info" {
  default     = ""
  description = "This variable is auto populated."
}

variable "automate_fqdn" {
  default     = ""
  description = "Automate FQDN variable. Leave as an empty string for AWS"
}

variable "automate_frontend_url" {
  default     = ""
  description = "Automate FQDN variable. Leave as an empty string for AWS"
}

variable "automate_instance_count" {
  default = 1
}

variable "automate_lb_fqdn" {
  default = ""
}

variable "automate_license" {
  default = "Contact Chef Sales at sales@chef.io to request a license."
}

variable "backend_aib_dest_file" {
  default     = ""
  description = "The full path of the airgap bundle destination file."
}

variable "backend_aib_local_file" {
  default     = ""
  description = "The filename of the airgap bundle to use from the 'terraform/transfer_files' directory."
}

variable "chef_server_instance_count" {
  default = 1
}

variable "curator_pkg_ident" {
  default = "chef/automate-ha-curator"
}

variable "curator_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "elasticsearch_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/elasticsearch"
  description = "The filesystem path to the Elasticsearch archive share."
}

variable "elasticsearch_https_user" {
  default = "automate_elasticsearch"
}
variable "opensearch_instance_count" {
  default = 3
}

variable "opensearch_listen_port" {
  default = 9200
}

variable "opensearch_username" {
  default = "admin"
}

variable "opensearch_user_password" {
  default = "admin"
}

variable "elasticsearch_pkg_ident" {
  default = "chef/automate-ha-elasticsearch"
}

variable "elasticsearch_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "elasticsidecar_pkg_ident" {
  default = "chef/automate-ha-elasticsidecar"
}

variable "elasticsidecar_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "frontend_aib_dest_file" {
  default     = ""
  description = "The full path of the airgap bundle destination file."
}

variable "frontend_aib_local_file" {
  default     = ""
  description = "The filename of the airgap bundle to use from the 'terraform/transfer_files' directory."
}

variable "habitat_info" {
  default     = ""
  description = "This variable is auto populated."
}

variable "habitat_uid_gid" {
  default     = ""
  description = "Set UID/GID for hab user"
}

variable "hab_sup_http_gateway_auth_token" {
  description = "Simple HTTP authentication Bearer token. Run 'make sup-keys' from the top-level project directory."
}

variable "hab_sup_ring_key" {
  description = "A symmetric pre-shared key used for wire encryption. Run 'make sup-keys' from the top-level project directory."
}

variable "hab_sup_run_args" {
  default = "--no-color --strategy none --peer-watch-file /etc/hab_peer_watch --permanent-peer --key /hab/sup/default/HttpGateway.key --certs /hab/sup/default/HttpGatewayChained.pem"
}

variable "journalbeat_pkg_ident" {
  default = "chef/automate-ha-journalbeat"
}

variable "kibana_https_user" {
  default = "admin"
}

variable "kibana_pkg_ident" {
  default = "chef/automate-ha-kibana"
}

variable "lb_access_logs" {
  default = false
}

variable "metricbeat_pkg_ident" {
  default = "chef/automate-ha-metricbeat"
}

variable "nfs_mount_path" {
  default     = "/mnt/automate_backups"
  description = "The NFS mount base path for backups and archives."
}

variable "opensearch_pkg_ident" {
  default = "chef/automate-ha-opensearch"
}

variable "pgleaderchk_listen_port" {
  default = 6432
}

variable "pgleaderchk_pkg_ident" {
  default = "chef/automate-ha-pgleaderchk"
}

variable "pgleaderchk_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "postgresql_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/postgresql"
  description = "The filesystem path to the PostgreSQL archive share."
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_listen_port" {
  default = 5432
}

variable "postgresql_pkg_ident" {
  default = "chef/automate-ha-postgresql"
}

variable "postgresql_pg_dump_enabled" {
  default     = true
  description = "Enable or disable PostgreSQL pg_dump for backups. possible values: [true, false]"
}

variable "postgresql_ssl_enable" {
  default     = true
  description = "Enable or disable SSL in PostgreSQL"
}

variable "postgresql_svc_load_args" {
  default = "--topology leader --strategy none"
}

variable "postgresql_wal_archive_enabled" {
  default     = false
  description = "(This is ALPHA and known to not work yet.) Enable or disable PostgreSQL WAL archive mode. possible values: [true, false]"
}

variable "proxy_listen_port" {
  default = 7432
}

variable "proxy_pkg_ident" {
  default = "chef/automate-ha-haproxy"
}

variable "proxy_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "rsync_files" {
  default     = []
  description = "An ordered array of SRC1,DST1,SRC2,DST2 file pairs. SRC is relative to the terraform/transfer_files directory"
}

variable "teams_port" {
  default = 10128
}

variable "tmp_path" {
  default = "/hab/var/automate-ha"
}

variable "sudo_password" {
  default = ""
}

variable "fe_sudo_password" {
  default = null
}

variable "be_sudo_password" {
  default = null
}

locals {
  fe_sudo_password = var.fe_sudo_password != null ? var.fe_sudo_password : var.sudo_password
  be_sudo_password = var.be_sudo_password != null ? var.be_sudo_password : var.sudo_password
}


variable "automate_root_ca" {
    default = ""
}
variable "automate_private_key" {
    default = ""
}
variable "automate_public_key" {
    default = ""
}
variable "chef_server_private_key" {
    default = ""
}
variable "chef_server_public_key" {
    default = ""
}

variable "postgresql_root_ca" {
    default = ""
}
variable "postgresql_private_key" {
    default = ""
}
variable "postgresql_public_key" {
    default = ""
}

variable "opensearch_root_ca" {
    default = ""
}
variable "opensearch_private_key" {
    default = ""
}
variable "opensearch_public_key" {
    default = ""
}
variable "opensearch_admin_dn" {
    default = ""
}
variable "opensearch_nodes_dn" {
    default = ""
}
variable "opensearch_admin_cert" {
    default = ""
}
variable "opensearch_admin_key" {
    default = ""
}
variable "automate_custom_certs_enabled" {
  default = false
}
variable "chef_server_custom_certs_enabled" {
  default = false
}
variable "postgresql_custom_certs_enabled" {
  default = false
}
variable "opensearch_custom_certs_enabled" {
  default = false
}
variable "tag_name" {
  default = "A2"
}
