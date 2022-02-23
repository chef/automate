provider "aws" {
  region                  = var.aws_region
  profile                 = var.aws_profile
}

resource "aws_efs_file_system" "backups" {
  count = var.efs_creation == "true" ? 1 : 0
  creation_token = var.efs_creation == "true" ? "${var.tag_name}_${var.aws_cluster_id}_efsfs" : ""

  tags = merge(var.tags, map("Name", "${var.tag_name}_${var.aws_cluster_id}_efsfs"))
}

resource "aws_efs_mount_target" "backups" {
  count           = var.efs_creation == "true" ? 3 : 0 
  file_system_id  = aws_efs_file_system.backups[0].id
  subnet_id       = element(data.aws_subnet.default.*.id, count.index)
  security_groups = [aws_security_group.efs_mount[0].id]
}

locals {
  mount_nfs = templatefile("${path.module}/mount_nfs.tpl", {
    efs_mount_dns = var.efs_creation == "true" ? aws_efs_file_system.backups[0].dns_name : "",
    efs_region    = var.aws_region,
    mount_path    = var.nfs_mount_path
  })
}

resource "null_resource" "automate_efs" {
  count = var.efs_creation == "true" ? length(var.automate_private_ips) : 0

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.automate_private_ips[count.index]
  }

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }
}

resource "null_resource" "chef_server_efs" {
  count = var.efs_creation == "true" ? length(var.chef_server_private_ips) : 0
  

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.chef_server_private_ips[count.index]
  }

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }
}

resource "null_resource" "postgresql_efs" {
  count = var.efs_creation == "true" ? length(var.postgresql_private_ips) : 0
  

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.postgresql_private_ips[count.index]
  }

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }
}

resource "null_resource" "elasticsearch_efs" {
  count = var.efs_creation == "true" ? length(var.elasticsearch_private_ips) : 0

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.elasticsearch_private_ips[count.index]
  }

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }
}
