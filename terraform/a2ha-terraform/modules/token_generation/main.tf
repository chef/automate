resource "null_resource" "token" {
  count = 1

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
  }

  provisioner "file" {
    destination = "${var.tmp_path}/create-api-token.sh"
    content = "sudo chef-automate iam token create admin-token --admin > ${var.tmp_path}/token "
  }


  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/create-api-token.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/create-api-token.sh",
    ]
  }

  provisioner "local-exec" {
    command = "scp -o StrictHostKeyChecking=no -i ${var.ssh_key_file} ${var.ssh_user}@${var.private_ips[0]}:${var.tmp_path}/token token"
  }

}

resource "null_resource" "create_chef_server" {
  count = length(var.chef_ips)
  provisioner "local-exec" {
     command = "bash create-chef-server.sh  ${var.automate-fqdn} ${var.chef_ips[count.index]} ${count.index}   > out.txt "
  }
 depends_on = [null_resource.token]
}



