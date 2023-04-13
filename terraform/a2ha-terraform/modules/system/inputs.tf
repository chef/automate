variable "automate_archive_disk_fs_path" {
}

variable "instance_count" {
}

variable "opensearch_archive_disk_fs_path" {
}

variable "postgresql_archive_disk_fs_path" {
}

variable "private_ips" {
  default = []
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

variable "ssh_group_name" {
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/hab/var/automate-ha"
}
