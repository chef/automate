locals {
  metricbeat_user_toml = templatefile("${path.module}/templates/metricbeat_user.toml.tpl", {
    metricbeat_tags = join(", ", formatlist("\"%s\"", var.metricbeat_tags)),
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
    backend_aib_dest_file    = var.backend_aib_dest_file,
    metricbeat_pkg_ident     = var.metricbeat_pkg_ident,
    metricbeat_svc_binds     = var.metricbeat_svc_binds,
    metricbeat_svc_load_args = var.metricbeat_svc_load_args,
    tmp_path                 = var.tmp_path
  })
}

resource "null_resource" "metricbeat" {
  count = var.instance_count

  triggers = {
    template      = local.metricbeat_user_toml
    template_prov = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_metricbeat.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/metricbeat_user.toml"
    content     = local.metricbeat_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/metricbeat_provision.sh"
    content     = local.provision
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/metricbeat_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/metricbeat_provision.sh",
    ]
  }
}

