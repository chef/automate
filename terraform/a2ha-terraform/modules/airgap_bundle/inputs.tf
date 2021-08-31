variable "archive_disk_info" {
}

variable "bundle_files" {
  default = []
  description = "Array of hashs for bundle files, hash should have a source and destination key"
}

variable "instance_count" {
}

variable "public_ips" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_user" {
}

variable "tmp_path" {
}
