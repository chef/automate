variable "airgap_info" {
}

variable "automate_admin_email" {
}

variable "automate_admin_password" {
}

variable "automate_admin_username" {
}

variable "automate_config" {
  default = ""
}

variable "automate_dc_token" {
}

variable "automate_fqdn" {
}

variable "automate_instance_count" {
  default = 1
}

variable "automate_license" {
  default = "Contact Chef Sales at sales@chef.io to request a license."
}

variable "automate_role" {
  description = "Set the type of server role. can be one of: bootstrap_automate, automate or chef_api"
}

variable "backend_aib_dest_file" {
}

variable "backend_aib_local_file" {
}

variable "cluster_id" {
  default = ""
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "elasticsearch_private_ips" {
  default = []
}

variable "frontend_aib_dest_file" {
}

variable "frontend_aib_local_file" {
}

variable "hab_sup_http_gateway_auth_token" {
}

variable "habitat_info" {
}

variable "postgresql_private_ips" {
  default = []
}

variable "postgresql_ssl_enable" {
}

variable "private_ips" {
  default = []
}

variable "proxy_listen_port" {
}

variable "public_ips" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_user" {
  default = "centos"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "teams_port" {
}

variable "tmp_path" {
  default = "/var/tmp"
}
