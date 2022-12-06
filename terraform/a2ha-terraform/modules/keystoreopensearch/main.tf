locals {
    keystore = templatefile("${path.module}/templates/opensearch-keystore.sh.tpl", {
    opensearch_pkg_ident            = var.opensearch_pkg_ident,
    backup_config_s3                = var.backup_config_s3
    access_key                      = var.access_key
    secret_key                      = var.secret_key
    listen_port                     = var.opensearch_listen_port
    tmp_path                        = var.tmp_path
  })
}


resource "null_resource" "opensearch_keystore" {
  count = var.backup_config_s3 == "true" ? var.opensearch_instance_count : 0

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_opensearch_keystore.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/opensearch-keystore.sh"
    content     = local.keystore
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/opensearch-keystore.sh",
      "sleep ${count.index*2}",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/opensearch-keystore.sh",
    ]
  }
}