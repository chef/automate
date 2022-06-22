locals {
  peer_ip_list = join("\n", var.peer_ips)
  habitat_info = templatefile("${path.module}/templates/habitat.info.tpl", {
    airgap_info    = var.airgap_info,
    instance_count = var.instance_count,
    peers          = local.peer_ip_list
  })
  install_hab = templatefile("${path.module}/templates/install_hab.sh.tpl", {
    aib_file                        = var.backend_aib_dest_file,
    hab_sup_http_gateway_auth_token = var.hab_sup_http_gateway_auth_token,
    hab_sup_http_gateway_ca_cert    = var.hab_sup_http_gateway_ca_cert,
    hab_sup_http_gateway_pub_cert   = var.hab_sup_http_gateway_pub_cert,
    hab_sup_http_gateway_priv_key   = var.hab_sup_http_gateway_priv_key,
    hab_sup_ring_key                = var.hab_sup_ring_key,
    install_hab_sh_args             = var.install_hab_sh_args,
    tmp_path                        = var.tmp_path
    habitat_uid_gid                 = var.habitat_uid_gid
  })
  hab_sup_service = templatefile("${path.module}/templates/hab-sup.service.tpl", {
    hab_sup_run_args = var.hab_sup_run_args,
    tmp_path         = var.tmp_path
  })
}

resource "null_resource" "habitat" {
  count = var.instance_count

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_habitat.sh"
  }

  triggers = {
    template = local.habitat_info
    peer_ips = local.peer_ip_list
  }

  provisioner "file" {
    destination = "${var.tmp_path}/hab-sup.service"
    content     = local.hab_sup_service
  }

  provisioner "file" {
    destination = "${var.tmp_path}/install_hab.sh"
    content     = local.install_hab
  }

  provisioner "file" {
    destination = "${var.tmp_path}/hab_peer_watch"
    content     = local.peer_ip_list
  }

  provisioner "file" {
    destination = "${var.tmp_path}/habitat.info"
    content     = local.habitat_info
  }

  provisioner "remote-exec" {
    inline = [
      "echo \"${var.airgap_info}\"",
      "chmod 0700 ${var.tmp_path}/install_hab.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/install_hab.sh",
    ]
  }
}
