locals {
  opensearch_user_toml = [
    for n in range(var.opensearch_instance_count) : templatefile("${path.module}/templates/opensearch_user.toml.tpl", {
      listen_port                     = var.opensearch_listen_port,
      minimum_masters                 = floor(var.opensearch_instance_count / 2 + 1),
      node_name                       = var.private_ips[n],
      private_ip                      = var.private_ips[n],
      private_ips                     = join(", ", formatlist("\"%s\"", var.private_ips)),
      nodes_list                      = join(", ", formatlist("\"%s\"", var.private_ips))
      tmp_path                        = var.tmp_path
      backup_config                   = var.backup_config_s3 == "true" ? var.location == "s3" ? "s3" : "gcs" : var.backup_config_efs == "true" ? "efs" : ""
      protocol                        = length(split("://", var.s3_endpoint)) > 1 ? split("://", var.s3_endpoint)[0] : "https"
      endpoint                        = length(split("://", var.s3_endpoint)) > 1 ? split("://", var.s3_endpoint)[1] : length(split("://", var.s3_endpoint)) == 1 ? split("://", var.s3_endpoint)[0] : "s3.amazonaws.com"
      nfs_mount_path                  = var.nfs_mount_path
      opensearch_custom_certs_enabled = var.opensearch_custom_certs_enabled
      opensearch_root_ca              = var.opensearch_root_ca
      opensearch_admin_key            = var.opensearch_admin_key
      opensearch_admin_cert           = var.opensearch_admin_cert
      opensearch_admin_dn             = var.opensearch_admin_dn
      opensearch_public_key           = contains(keys(var.opensearch_certs_by_ip), var.private_ips[n]) ? var.opensearch_certs_by_ip[element(var.private_ips, n)].public_key : var.opensearch_public_key
      opensearch_private_key          = contains(keys(var.opensearch_certs_by_ip), var.private_ips[n]) ? var.opensearch_certs_by_ip[element(var.private_ips, n)].private_key : var.opensearch_private_key
      opensearch_nodes_dn             = contains(keys(var.opensearch_certs_by_ip), var.private_ips[n]) ? var.opensearch_certs_by_ip[element(var.private_ips, n)].nodes_dn : var.opensearch_nodes_dn
    })
  ]
  opensearchsidecar_user_toml = [
    for n in range(var.opensearch_instance_count) : templatefile("${path.module}/templates/opensearchsidecar_user.toml.tpl", {
      private_ip = var.private_ips[n]
    })
  ]
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file           = var.backend_aib_dest_file,
    opensearch_pkg_ident            = var.opensearch_pkg_ident,
    opensearch_svc_load_args        = var.opensearch_svc_load_args,
    opensearchsidecar_pkg_ident     = var.opensearchsidecar_pkg_ident,
    opensearchsidecar_svc_load_args = var.opensearchsidecar_svc_load_args,
    tmp_path                        = var.tmp_path
    backup_config_s3                = var.backup_config_s3
    access_key                      = var.access_key
    secret_key                      = var.secret_key
    listen_port                     = var.opensearch_listen_port
    nfs_mount_path                  = var.nfs_mount_path
    opensearch_username             = var.opensearch_username
    opensearch_user_password        = var.opensearch_user_password
  })

  efs_backup = templatefile("${path.module}/templates/efs_backup.sh.tpl", {
    nfs_mount_path        = var.nfs_mount_path
  })
}

resource "null_resource" "opensearch" {
  count = var.opensearch_instance_count

  triggers = {
    es_user_toml_sha = sha1(local.opensearch_user_toml[count.index])
    template         = local.provision
  }

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
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

resource "null_resource" "backup_configuration" {
  count = 0

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[0]
    script_path = "${var.tmp_path}/tf_inline_script_system_opensearch.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/efs_backup.sh"
    content      = local.efs_backup
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/efs_backup.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/efs_backup.sh",
    ]
  }

  depends_on = [
    null_resource.opensearch
  ]
}