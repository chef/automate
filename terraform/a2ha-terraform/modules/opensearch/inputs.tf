variable "access_key" {
  default = ""
}

variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "backend_aib_local_file" {
}

variable "backup_config_efs" {
  default = "false"
}

variable "backup_config_s3" {
  default = "false"
}

variable "curator_pkg_ident" {
}

variable "habitat_info" {
}

variable "journalbeat_pkg_ident" {
  default = "chef/automate-ha-journalbeat"
}

variable "kibana_pkg_ident" {
  default = "chef/automate-ha-kibana"
}

variable "location" {
  default = ""
}

variable "metricbeat_pkg_ident" {
  default = "chef/automate-ha-metricbeat"
}

variable "nfs_mount_path" {
  default = "/mnt/automate_backups"
}

variable "opensearch_admin_cert" {
}

variable "opensearch_admin_dn" {
}

variable "opensearch_admin_key" {
}

variable "opensearch_certs_by_ip" {
  type = map(map(string))
  default = {}
}

variable "opensearch_custom_certs_enabled" {
  default = false
}

variable "opensearch_instance_count" {
  default = 1
}

variable "opensearch_listen_port" {
  default = 9200
}

variable "opensearch_nodes_dn" {
}

variable "opensearch_pkg_ident" {
  default = "chef/automate-ha-opensearch"
}

variable "opensearch_private_key" {
}

variable "opensearch_public_key" {
}

variable "opensearch_root_ca" {
}

variable "opensearch_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "opensearch_user_password" {
  default = "admin"
}

variable "opensearch_username" {
  default = "admin"
}

variable "opensearchsidecar_pkg_ident" {
}

variable "opensearchsidecar_svc_load_args" {
}

variable "private_ips" {
  default = []
}

variable "public_ips" {
  default = []
}

variable "s3_endpoint" {
  default = ""
}

variable "secret_key" {
  default = ""
}

variable "ssh_key_file" {
}

variable "ssh_port" {
  default = 22
}

variable "ssh_user" {
  default = "ubuntu"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/hab/var/automate-ha"
}
