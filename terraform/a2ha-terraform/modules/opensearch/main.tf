locals {
  opensearch_user_toml = [
    for n in range(var.opensearch_instance_count) : templatefile("${path.module}/templates/opensearch_user.toml.tpl", {
      listen_port     = var.opensearch_listen_port,
      minimum_masters = floor(var.opensearch_instance_count / 2 + 1),
      node_name       = var.private_ips[n],
      private_ip      = var.private_ips[n],
      private_ips     = join(", ", formatlist("\"%s\"", var.private_ips)),
      nodes_list      = join(", ", formatlist("\"%s\"", var.private_ips))
      tmp_path        = var.tmp_path
    })
  ]
  opensearchsidecar_user_toml = [
    for n in range(var.opensearch_instance_count) : templatefile("${path.module}/templates/opensearchsidecar_user.toml.tpl", {
      private_ip = var.private_ips[n]
    })
  ]
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file        = var.backend_aib_dest_file,
    opensearch_pkg_ident      = var.opensearch_pkg_ident,
    opensearch_svc_load_args  = var.opensearch_svc_load_args,
    opensearchsidecar_pkg_ident     = var.opensearchsidecar_pkg_ident,
    opensearchsidecar_svc_load_args = var.opensearchsidecar_svc_load_args,
    tmp_path                     = var.tmp_path
  })
}

resource "null_resource" "opensearch" {
  count = var.opensearch_instance_count

  triggers = {
    es_user_toml_sha = sha1(local.opensearch_user_toml[count.index])
    template = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_opensearch.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/opensearch-user.toml"
    content     = local.opensearch_user_toml[count.index]
  }

  provisioner "file" {
    destination = "${var.tmp_path}/es_provision.sh"
    content     = local.provision
  }

  provisioner "file" {
    destination = "${var.tmp_path}/opensearchsidecar.toml"
    content     = local.opensearchsidecar_user_toml[count.index]
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/es_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/es_provision.sh",
    ]
  }
}
