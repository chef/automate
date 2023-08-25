locals {
    keystore = templatefile("${path.module}/templates/opensearch-keystore.sh.tpl", {
    opensearch_pkg_ident            = var.opensearch_pkg_ident,
    backup_config_s3                = var.backup_config_s3
    access_key                      = var.access_key
    secret_key                      = var.secret_key
    listen_port                     = var.opensearch_listen_port
    tmp_path                        = var.tmp_path
    location                        = var.location
    google_service_account_file     = var.google_service_account_file
  })
}

resource "null_resource" "copy_file" {
  triggers = {
    source_exists = fileexists("${var.google_service_account_file}")
  }

  provisioner "local-exec" {
    command = "echo GCP Service Account File exists"
    when    = triggers.source_exists
  }

  provisioner "file" {
    when        = triggers.source_exists
    source      = "${var.google_service_account_file}"
    destination = "${var.tmp_path}/${var.google_service_account_file}"
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = element(var.private_ips, count.index)
    port        = var.ssh_port
    script_path = "${var.tmp_path}/tf_inline_script_system_gcp.sh"
  }
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