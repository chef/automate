resource "null_resource" "add_chef_server_fqdn_to_ui" {
  count = 1

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
  }

  provisioner "file" {
    destination = "${var.tmp_path}/create-api-token.sh"
    source = "${path.module}/files/create-api-token.sh"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/create-api-token.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/create-api-token.sh ${var.automate-fqdn} ${var.chef-server-fqdn}",
    ]
  }

}
