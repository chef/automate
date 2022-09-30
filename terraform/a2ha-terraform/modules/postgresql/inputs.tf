variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "backend_aib_local_file" {
}

variable "backup_config_efs" {
  default = "false"
}

variable "habitat_info" {
}

variable "journalbeat_pkg_ident" {
}

variable "metricbeat_pkg_ident" {
}

variable "nfs_mount_path" {
  default = "/mnt/automate_backups"
}

variable "opensearch_listen_port" {
  default = 9200
}

variable "opensearch_private_ips" {
  default = []
}

variable "pgleaderchk_listen_port" {
  default = 6432
}

variable "pgleaderchk_pkg_ident" {
}

variable "pgleaderchk_svc_load_args" {
}

variable "postgresql_archive_disk_fs_path" {
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_listen_port" {
  default = 5432
}

variable "postgresql_pg_dump_enabled" {
}

variable "postgresql_pkg_ident" {
}

variable "postgresql_ssl_enable" {
}

variable "postgresql_svc_load_args" {
}

variable "postgresql_wal_archive_enabled" {
}

variable "private_ips" {
  default = []
}

variable "proxy_listen_port" {
  default = 7432
}

variable "proxy_pkg_ident" {
}

variable "proxy_svc_load_args" {
}

variable "public_ips" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_port" {
  default = 22
}

variable "ssh_user" {
  default = "centos"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/var/automate-ha"
}
