resource "aws_efs_file_system" "backups" {
  creation_token = "${var.tag_name}_${var.random_id}_efsfs"
  encrypted      = true

#  tags = merge(var.tags, map("Name", "${var.tag_name}_${var.random_id}_efsfs"))
}

resource "aws_efs_mount_target" "backups" {
  count           = 3
  file_system_id  = aws_efs_file_system.backups.id
  subnet_id       = element(var.subnet_id, count.index)
  security_groups = [var.mount_id]
}

locals {
  mount_nfs = templatefile("${path.module}/files/mount_nfs.tpl", {
    efs_mount_dns = aws_efs_file_system.backups.dns_name,
    efs_region    = var.aws_region,
    mount_path    = var.nfs_mount_path
  })
  
  ip_list = concat(var.automate_private_ips, var.chef_server_private_ips, var.postgresql_private_ips, var.elasticsearch_private_ips)

}

resource "null_resource" "mount_efs" {
#  for_each = toset(${local.ip_list})
  count = length(local.ip_list)
  connection {
    host        = "${local.ip_list[count.index]}"
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
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
