locals {
  kibana_user_toml = templatefile("${path.module}/templates/kibana_user.toml.tpl", {
    kibana_tags = join(", ", formatlist("\"%s\"", var.kibana_tags)),
    elasticsearch_output_hosts = join(
      ", ",
      formatlist("\"%s:%s\"", var.private_ips, var.elasticsearch_listen_port),
    ),
    nginx_elasticsearch_user = var.nginx_elasticsearch_user,
    nginx_elasticsearch_auth = var.nginx_elasticsearch_auth,
    tmp_path                 = var.tmp_path
  })
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file = var.backend_aib_dest_file,
    kibana_pkg_ident      = var.kibana_pkg_ident,
    kibana_svc_binds      = var.kibana_svc_binds,
    kibana_svc_load_args  = var.kibana_svc_load_args,
    tmp_path              = var.tmp_path
  })
}

resource "null_resource" "kibana" {
  count = var.instance_count

  triggers = {
    template      = local.kibana_user_toml
    template_prov = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_kibana.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/kibana_user.toml"
    content     = local.kibana_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/kibana_provision.sh"
    content     = local.provision
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/kibana_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/kibana_provision.sh",
    ]
  }
}
