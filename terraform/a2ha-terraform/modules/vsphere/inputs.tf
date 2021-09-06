variable "automate_instance_count" {
  default = 1
}

variable "automate_instance_cpus" {
  default = 2
}

variable "automate_instance_datadisk_gb" {
  default = 100
}

variable "automate_instance_ram_mb" {
  default     = 8192
  description = "RAM value in MB"
}

variable "chef_server_instance_count" {
  default = 1
}

variable "chef_server_instance_cpus" {
  default = 2
}

variable "chef_server_instance_datadisk_gb" {
  default = 20
}

variable "chef_server_instance_ram_mb" {
  default     = 4096
  description = "RAM value in MB"
}

variable "elasticsearch_instance_count" {
  default = 3
}

variable "elasticsearch_instance_cpus" {
  default = 2
}

variable "elasticsearch_instance_datadisk_gb" {
  default = 100
}

variable "elasticsearch_instance_ram_mb" {
  default     = 8192
  description = "RAM value in MB"
}

variable "nfs_mount_path" {
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_instance_cpus" {
  default = 2
}

variable "postgresql_instance_datadisk_gb" {
  default = 50
}

variable "postgresql_instance_ram_mb" {
  default     = 4096
  description = "RAM value in MB"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tag_contact" {
}

variable "tag_dept" {
}

variable "tag_name" {
  default = "A2"
}

variable "tag_project" {
}

variable "tmp_path" {
}

variable "vsphere_allow_unverified_ssl" {
  default = true
}

variable "vsphere_datacenter" {
  default = "Tierpoint"
}

variable "vsphere_datastore" {
  default = "FreeNAS03 iSCSI"
}

variable "vsphere_linux_datadisk_dev" {
  default     = "/dev/sdb"
  description = "Disk device which is meant to be formatted and mounted at /hab"
}

variable "vsphere_linux_lvm_allocate_pct" {
  default     = "80"
  description = "Percept of the LVM VG to allocate to the LV - 80% is recommended so theres room for snapshots"
}

variable "vsphere_linux_sshkeyfile" {
  default     = "~/.ssh/vagrant"
  description = "Path to the ssh private key file"
}

variable "vsphere_linux_sshuser" {
  default = "vagrant"
}

variable "vsphere_linux_template" {
  description = "RHEL7 or CentOS7 template"
  default     = "centos76-template"
}

variable "vsphere_network" {
  default = "VM Network"
}

variable "vsphere_password" {
}

variable "vsphere_resource_pool" {
  default = "a2-ha-backend"
}

variable "vsphere_server" {
}

variable "vsphere_user" {
}
