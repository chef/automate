locals {
  curator_user_toml = templatefile("${path.module}/templates/curator_user.toml.tpl", {})
  provision =  templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file = var.backend_aib_dest_file,
    curator_pkg_ident     = var.curator_pkg_ident,
    curator_svc_binds     = var.curator_svc_binds,
    curator_svc_load_args = var.curator_svc_load_args,
    tmp_path              = var.tmp_path
  })
}

resource "null_resource" "curator" {
  count = var.instance_count

  triggers = {
    template      = local.curator_user_toml
    template_prov = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_curator.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/curator_user.toml"
    content     = local.curator_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/curator_provision.sh"
    content     = local.provision
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/curator_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/curator_provision.sh",
    ]
  }
}

