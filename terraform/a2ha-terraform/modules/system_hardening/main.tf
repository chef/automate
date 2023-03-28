locals {
  hardening_sh = templatefile("${path.module}/templates/hardening.sh.tpl", {
    ssh_user_sudo_password = var.ssh_user_sudo_password,
    ssh_user               = var.ssh_user,
    tmp_path               = var.tmp_path
  })
}

resource "null_resource" "system_hardening" {
  count = var.instance_count

  triggers = {
    template = local.hardening_sh
  }

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_hardening.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/hardening.sh"
    content     = local.hardening_sh
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/hardening.sh",
    ]
  }
}

