provider "vsphere" {
  user           = var.vsphere_user
  password       = var.vsphere_password
  vsphere_server = var.vsphere_server

  # If you have a self-signed cert
  allow_unverified_ssl = true
}

resource "random_id" "random" {
  byte_length = 4
}

data "vsphere_datacenter" "dc" {
  name = var.vsphere_datacenter
}

data "vsphere_datastore" "datastore" {
  name          = var.vsphere_datastore
  datacenter_id = data.vsphere_datacenter.dc.id
}

data "vsphere_resource_pool" "pool" {
  name          = var.vsphere_resource_pool
  datacenter_id = data.vsphere_datacenter.dc.id
}

data "vsphere_network" "network" {
  name          = var.vsphere_network
  datacenter_id = data.vsphere_datacenter.dc.id
}

data "vsphere_virtual_machine" "template" {
  name          = var.vsphere_linux_template
  datacenter_id = data.vsphere_datacenter.dc.id
}


locals {
# This script is meant to be run once at VM provision time only
  mount_data_disk = templatefile("${path.module}/mount_data_disk.tpl", {
    disk_dev                = var.vsphere_linux_datadisk_dev,
    lvm_volume_allocate_pct = var.vsphere_linux_lvm_allocate_pct,
    tmp_path                = var.tmp_path
  })
# This script is meant to be run once at VM provision time only
  mount_nfs_share = templatefile("${path.module}/mount_nfs_share.tpl", {
    mount_point = var.nfs_mount_path
  })
}

data "vsphere_custom_attribute" "contact" {
  name = "Contact"
}

data "vsphere_custom_attribute" "dept" {
  name = "Department"
}

data "vsphere_custom_attribute" "project" {
  name = "Project"
}

data "vsphere_custom_attribute" "clustername" {
  name = "ClusterName"
}

data "vsphere_custom_attribute" "clusterid" {
  name = "ClusterID"
}

resource "vsphere_virtual_machine" "chef_automate_postgresql" {
  count = var.postgresql_instance_count

  name = format(
    "${var.tag_name}_${random_id.random.hex}_backend_postgresql_%02d",
    count.index + 1,
  )
  resource_pool_id = data.vsphere_resource_pool.pool.id
  datastore_id     = data.vsphere_datastore.datastore.id
  custom_attributes = {
    data.vsphere_custom_attribute.contact.id     = var.tag_contact
    data.vsphere_custom_attribute.dept.id        = var.tag_dept
    data.vsphere_custom_attribute.project.id     = var.tag_project
    data.vsphere_custom_attribute.clustername.id = var.tag_name
    data.vsphere_custom_attribute.clusterid.id   = random_id.random.hex
  }

  num_cpus  = var.postgresql_instance_cpus
  memory    = var.postgresql_instance_ram_mb
  guest_id  = data.vsphere_virtual_machine.template.guest_id
  scsi_type = data.vsphere_virtual_machine.template.scsi_type

  connection {
    host        = self.default_ip_address
    type        = "ssh"
    user        = var.vsphere_linux_sshuser
    private_key = file(var.vsphere_linux_sshkeyfile)
    script_path = "${var.tmp_path}/tf_inline_script_vsphere.sh"
  }

  network_interface {
    network_id   = data.vsphere_network.network.id
    adapter_type = data.vsphere_virtual_machine.template.network_interface_types[0]
  }

  # OS disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk0"
    size             = data.vsphere_virtual_machine.template.disks[0].size
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 0
  }

  # Data disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk1"
    size             = var.postgresql_instance_datadisk_gb
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 1
  }

  clone {
    template_uuid = data.vsphere_virtual_machine.template.id
  }

  provisioner "file" {
    content     = local.mount_data_disk
    destination = "${var.tmp_path}/mount_data_disk"
  }

  provisioner "file" {
    content     = local.mount_nfs_share
    destination = "${var.tmp_path}/mount_nfs_share"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_data_disk",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs_share",
    ]
  }
}

resource "vsphere_virtual_machine" "chef_automate_elasticsearch" {
  count = var.elasticsearch_instance_count

  name = format(
    "${var.tag_name}_${random_id.random.hex}_backend_elasticsearch_%02d",
    count.index + 1,
  )
  resource_pool_id = data.vsphere_resource_pool.pool.id
  datastore_id     = data.vsphere_datastore.datastore.id
  custom_attributes = {
    data.vsphere_custom_attribute.contact.id     = var.tag_contact
    data.vsphere_custom_attribute.dept.id        = var.tag_dept
    data.vsphere_custom_attribute.project.id     = var.tag_project
    data.vsphere_custom_attribute.clustername.id = var.tag_name
    data.vsphere_custom_attribute.clusterid.id   = random_id.random.hex
  }

  num_cpus  = var.elasticsearch_instance_cpus
  memory    = var.elasticsearch_instance_ram_mb
  guest_id  = data.vsphere_virtual_machine.template.guest_id
  scsi_type = data.vsphere_virtual_machine.template.scsi_type

  connection {
    host        = self.default_ip_address
    type        = "ssh"
    user        = var.vsphere_linux_sshuser
    private_key = file(var.vsphere_linux_sshkeyfile)
    script_path = "${var.tmp_path}/tf_inline_script_vsphere.sh"
  }

  network_interface {
    network_id   = data.vsphere_network.network.id
    adapter_type = data.vsphere_virtual_machine.template.network_interface_types[0]
  }

  # OS disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk0"
    size             = data.vsphere_virtual_machine.template.disks[0].size
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 0
  }

  # Data disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk1"
    size             = var.elasticsearch_instance_datadisk_gb
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 1
  }

  clone {
    template_uuid = data.vsphere_virtual_machine.template.id
  }

  provisioner "file" {
    content     = local.mount_data_disk
    destination = "${var.tmp_path}/mount_data_disk"
  }

  provisioner "file" {
    content     = local.mount_nfs_share
    destination = "${var.tmp_path}/mount_nfs_share"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_data_disk",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs_share",
    ]
  }
}

resource "vsphere_virtual_machine" "chef_automate" {
  count = var.automate_instance_count

  name = format(
    "${var.tag_name}_${random_id.random.hex}_automate_%02d",
    count.index + 1,
  )
  resource_pool_id = data.vsphere_resource_pool.pool.id
  datastore_id     = data.vsphere_datastore.datastore.id
  custom_attributes = {
    data.vsphere_custom_attribute.contact.id     = var.tag_contact
    data.vsphere_custom_attribute.dept.id        = var.tag_dept
    data.vsphere_custom_attribute.project.id     = var.tag_project
    data.vsphere_custom_attribute.clustername.id = var.tag_name
    data.vsphere_custom_attribute.clusterid.id   = random_id.random.hex
  }

  num_cpus  = var.automate_instance_cpus
  memory    = var.automate_instance_ram_mb
  guest_id  = data.vsphere_virtual_machine.template.guest_id
  scsi_type = data.vsphere_virtual_machine.template.scsi_type

  connection {
    host        = self.default_ip_address
    type        = "ssh"
    user        = var.vsphere_linux_sshuser
    private_key = file(var.vsphere_linux_sshkeyfile)
    script_path = "${var.tmp_path}/tf_inline_script_vsphere.sh"
  }

  network_interface {
    network_id   = data.vsphere_network.network.id
    adapter_type = data.vsphere_virtual_machine.template.network_interface_types[0]
  }

  # OS disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk0"
    size             = data.vsphere_virtual_machine.template.disks[0].size
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 0
  }

  # Data disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk1"
    size             = var.automate_instance_datadisk_gb
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 1
  }

  clone {
    template_uuid = data.vsphere_virtual_machine.template.id
  }

  provisioner "file" {
    content     = local.mount_data_disk
    destination = "${var.tmp_path}/mount_data_disk"
  }

  provisioner "file" {
    content     = local.mount_nfs_share
    destination = "${var.tmp_path}/mount_nfs_share"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_data_disk",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs_share",
    ]
  }
}

resource "vsphere_virtual_machine" "chef_server" {
  count = var.chef_server_instance_count

  name = format(
    "${var.tag_name}_${random_id.random.hex}_chef_server_%02d",
    count.index + 1,
  )
  resource_pool_id = data.vsphere_resource_pool.pool.id
  datastore_id     = data.vsphere_datastore.datastore.id
  custom_attributes = {
    data.vsphere_custom_attribute.contact.id     = var.tag_contact
    data.vsphere_custom_attribute.dept.id        = var.tag_dept
    data.vsphere_custom_attribute.project.id     = var.tag_project
    data.vsphere_custom_attribute.clustername.id = var.tag_name
    data.vsphere_custom_attribute.clusterid.id   = random_id.random.hex
  }

  num_cpus  = var.chef_server_instance_cpus
  memory    = var.chef_server_instance_ram_mb
  guest_id  = data.vsphere_virtual_machine.template.guest_id
  scsi_type = data.vsphere_virtual_machine.template.scsi_type

  connection {
    host        = self.default_ip_address
    type        = "ssh"
    user        = var.vsphere_linux_sshuser
    private_key = file(var.vsphere_linux_sshkeyfile)
    script_path = "${var.tmp_path}/tf_inline_script_vsphere.sh"
  }

  network_interface {
    network_id   = data.vsphere_network.network.id
    adapter_type = data.vsphere_virtual_machine.template.network_interface_types[0]
  }

  # OS disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk0"
    size             = data.vsphere_virtual_machine.template.disks[0].size
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 0
  }

  # Data disk (note, don't change the label: https://www.terraform.io/docs/providers/vsphere/r/virtual_machine.html#label)
  disk {
    label            = "disk1"
    size             = var.chef_server_instance_datadisk_gb
    eagerly_scrub    = false
    thin_provisioned = true
    unit_number      = 1
  }

  clone {
    template_uuid = data.vsphere_virtual_machine.template.id
  }

  provisioner "file" {
    content     = local.mount_data_disk
    destination = "${var.tmp_path}/mount_data_disk"
  }

  provisioner "file" {
    content     = local.mount_nfs_share
    destination = "${var.tmp_path}/mount_nfs_share"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_data_disk",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs_share",
    ]
  }
}

