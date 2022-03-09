resource "null_resource" "token" {
  count = 1

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
  }

  provisioner "file" {
    destination = "${var.tmp_path}/create-api-token.sh"
    source = "${path.module}/create-api-token.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/create-chef-server.sh"
    source = "${path.module}/create-chef-server.sh"
  }
  

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/create-api-token.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/create-api-token.sh",
    ]
  }
  
}

resource "null_resource" "add_chef_server" {
  count = length(var.chef_ips)

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/create-chef-server.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/create-chef-server.sh ${var.automate-fqdn} ${var.chef_ips[count.index]} ${count.index} > out.txt",
    ]
  }

 depends_on = [null_resource.token]
}



