locals {
  elasticsearch_user_toml = [
    for n in range(var.elasticsearch_instance_count) : templatefile("${path.module}/templates/elasticsearch_user.toml.tpl", {
      listen_port     = var.elasticsearch_listen_port,
      minimum_masters = floor(var.elasticsearch_instance_count / 2 + 1),
      private_ip      = var.private_ips[n],
      private_ips     = join(", ", formatlist("\"%s\"", var.private_ips)),
      tmp_path        = var.tmp_path
    })
  ]
  elasticsidecar_user_toml = [
    for n in range(var.elasticsearch_instance_count) : templatefile("${path.module}/templates/elasticsidecar_user.toml.tpl", {
      private_ip = var.private_ips[n]
    })
  ]
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file        = var.backend_aib_dest_file,
    elasticsearch_pkg_ident      = var.elasticsearch_pkg_ident,
    elasticsearch_svc_load_args  = var.elasticsearch_svc_load_args,
    elasticsidecar_pkg_ident     = var.elasticsidecar_pkg_ident,
    elasticsidecar_svc_load_args = var.elasticsidecar_svc_load_args,
    tmp_path                     = var.tmp_path
  })
}

resource "null_resource" "elasticsearch" {
  count = var.elasticsearch_instance_count

  triggers = {
    es_user_toml_sha = sha1(local.elasticsearch_user_toml[count.index])
    template = local.provision
  }

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_elasticsearch.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/elasticsearch-user.toml"
    content     = local.elasticsearch_user_toml[count.index]
  }

  provisioner "file" {
    destination = "${var.tmp_path}/es_provision.sh"
    content     = local.provision
  }

  provisioner "file" {
    destination = "${var.tmp_path}/elasticsidecar.toml"
    content     = local.elasticsidecar_user_toml[count.index]
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

resource "null_resource" "backup_configuration" {
  count = var.backup_config_efs == "true" ? 1 : 0

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
    script_path = "${var.tmp_path}/tf_inline_script_system_elasticsearch.sh"
  }
  
  provisioner "file" {
    destination = "${var.tmp_path}/efs_backup.sh"
    source = "${path.module}/templates/efs_backup.sh"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/efs_backup.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/efs_backup.sh",
    ]
  }
  
  depends_on = [
    null_resource.elasticsearch
  ]
}