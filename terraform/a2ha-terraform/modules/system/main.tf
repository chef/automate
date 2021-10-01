locals {
  tunables_sh = templatefile("${path.module}/templates/tunables.sh.tpl", {
    tmp_path = var.tmp_path
  })
  archive_disk_info = templatefile("${path.module}/templates/archive_disk.info.tpl", {
    automate_archive_disk_fs_path      = var.automate_archive_disk_fs_path,
    elasticsearch_archive_disk_fs_path = var.elasticsearch_archive_disk_fs_path,
    postgresql_archive_disk_fs_path    = var.postgresql_archive_disk_fs_path
  })
}

resource "null_resource" "system_base_provisioning" {
  count = var.instance_count

  triggers = {
    template = local.tunables_sh
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system.sh"
  }

  provisioner "file" {
    content     = local.tunables_sh
    destination = "${var.tmp_path}/tunables.sh"
  }

  provisioner "file" {
    content     = local.archive_disk_info
    destination = "${var.tmp_path}/archive_disk.info"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/tunables.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/tunables.sh",
    ]
  }
}

