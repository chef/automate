locals {
  journalbeat_user_toml = templatefile("${path.module}/templates/journalbeat_user.toml.tpl", {
    journalbeat_tags = join(", ", formatlist("\"%s\"", var.journalbeat_tags)),
    elasticsearch_output_hosts = join(
      ", ",
      formatlist(
        "\"https://%s:%s\"",
        var.elasticsearch_private_ips,
        var.elasticsearch_listen_port,
      ),
    ),
    tmp_path = var.tmp_path
  })
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file     = var.backend_aib_dest_file,
    journalbeat_pkg_ident     = var.journalbeat_pkg_ident,
    journalbeat_svc_binds     = var.journalbeat_svc_binds,
    journalbeat_svc_load_args = var.journalbeat_svc_load_args,
    tmp_path                  = var.tmp_path
  })
}

resource "null_resource" "journalbeat" {
  count = var.instance_count

  triggers = {
    template      = local.journalbeat_user_toml
    template_prov = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_journalbeat.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/journalbeat_user.toml"
    content     = local.journalbeat_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/journalbeat_provision.sh"
    content     = local.provision
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/journalbeat_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/journalbeat_provision.sh",
    ]
  }
}

