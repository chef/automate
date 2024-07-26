locals {
  configsync = templatefile("${path.module}/templates/postgres_configsync.sh.tpl", {
    tmp_path                    = var.tmp_path
  })
}

resource "null_resource" "postgresrestart" {
  count = var.postgresql_instance_count

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_postgres_configsync.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/postgres_configsync.sh"
    content     = local.configsync
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/postgres_configsync.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/postgres_configsync.sh",
    ]
  }
}