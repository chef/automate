locals {
  automate_custom_config = "${var.tmp_path}/automate_custom_config.toml"

  connector_toml = templatefile("${path.module}/templates/connector.toml.tpl", {
    automate_fqdn                      = var.automate_fqdn,
    automate_admin_email               = var.automate_admin_email,
    automate_admin_username            = var.automate_admin_username,
    automate_admin_password            = var.automate_admin_password,
    automate_custom_config             = local.automate_custom_config,
    automate_dc_token                  = var.automate_dc_token,
    automate_role                      = var.automate_role,
    aws_region                         = var.aws_region,
    opensearch_ips                     = jsonencode(formatlist("%s", var.opensearch_private_ips)),
    opensearch_listen_port             = var.opensearch_listen_port,
    managed_opensearch_certificate     = var.managed_opensearch_certificate,
    managed_opensearch_domain_url      = var.managed_opensearch_domain_url,
    managed_opensearch_user_password   = var.managed_opensearch_user_password,
    managed_opensearch_username        = var.managed_opensearch_username,
    aws_os_snapshot_role_arn           = var.aws_os_snapshot_role_arn,
    os_snapshot_user_access_key_id     = var.os_snapshot_user_access_key_id,
    os_snapshot_user_access_key_secret = var.os_snapshot_user_access_key_secret,
    managed_rds_certificate            = var.managed_rds_certificate,
    managed_rds_dbuser_password        = var.managed_rds_dbuser_password,
    managed_rds_dbuser_username        = var.managed_rds_dbuser_username,
    managed_rds_instance_url           = var.managed_rds_instance_url,
    managed_rds_superuser_password     = var.managed_rds_superuser_password,
    managed_rds_superuser_username     = var.managed_rds_superuser_username,
    postgresql_ips                     = jsonencode(formatlist("%s", var.postgresql_private_ips)),
    postgresql_ssl_enable              = var.postgresql_ssl_enable ? "true" : "false",
    proxy_listen_port                  = var.proxy_listen_port,
    setup_managed_services             = var.setup_managed_services,
    teams_port                         = var.teams_port,
    tmp_path                           = var.tmp_path,
    backup_config_s3                   = var.backup_config_s3,
    backup_config_efs                  = var.backup_config_efs,
    nfs_mount_path                     = var.nfs_mount_path,
    s3_endpoint                        = var.s3_endpoint,
    bucket_name                        = var.bucket_name,
    access_key                         = var.access_key
    secret_key                         = var.secret_key
    infra                              = var.infra
  })

  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    admin_password                  = var.automate_admin_password,
    automate_custom_config          = local.automate_custom_config,
    automate_role                   = var.automate_role,
    backend_aib_file                = var.backend_aib_dest_file,
    frontend_aib_file               = var.frontend_aib_dest_file,
    hab_sup_http_gateway_auth_token = var.hab_sup_http_gateway_auth_token,
    ssh_user                        = var.ssh_user,
    ssh_port                        = var.ssh_port,
    tmp_path                        = var.tmp_path
  })
}

# special conditional resource if the server is a non-bootstrap
# the file resource is nice and will wait until the file appears
resource "null_resource" "automate_pre" {


  count = var.automate_role != "bootstrap_automate" ? var.automate_instance_count : 0

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = element(var.private_ips, count.index)
    port        = var.ssh_port
    script_path = "${var.tmp_path}/tf_inline_script_system_automate.sh"
  }

  provisioner "local-exec" {
    # https://github.com/hashicorp/terraform/issues/17101
    # Until Terraform supports explicit module inter-dependencies, we create an implicit
    # dependency by using outputs from the Habitat and Airgap modules.
    command = "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\""
  }

  provisioner "local-exec" {
    command = "while true; do sleep 5; test -f ${path.module}/../../bootstrap${var.cluster_id}.abb && break; done"
  }

  provisioner "file" {
    source      = "${path.module}/../../bootstrap${var.cluster_id}.abb"
    destination = "${var.tmp_path}/bootstrap.abb"
  }
}

resource "null_resource" "automate" {
  count = var.automate_instance_count

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = element(var.private_ips, count.index)
    port        = var.ssh_port
    script_path = "${var.tmp_path}/tf_inline_script_system_automate.sh"
  }

  triggers = {
    connector_toml_sha         = sha1(local.connector_toml)
    template                   = local.provision
    automate_custom_config_sha = sha1(var.automate_config)
  }

  provisioner "file" {
    content     = var.automate_config
    destination = local.automate_custom_config
  }

  provisioner "file" {
    content     = local.connector_toml
    destination = "${var.tmp_path}/connector.toml"
  }

  provisioner "file" {
    source      = "${path.module}/files/config.toml.erb"
    destination = "${var.tmp_path}/config.toml.erb"
  }

  provisioner "file" {
    content     = local.provision
    destination = "${var.tmp_path}/automate_provision.sh"
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/automate_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/automate_provision.sh",
    ]
  }

  depends_on = [null_resource.automate_pre]
}

# special conditional resource if the server is a bootstrap, run after the rest
resource "null_resource" "automate_post" {
  count = var.automate_role == "bootstrap_automate" ? 1 : 0

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = element(var.private_ips, count.index)
    port        = var.ssh_port
    script_path = "${var.tmp_path}/tf_inline_script_system_automate.sh"
  }

  provisioner "local-exec" {
    # https://github.com/hashicorp/terraform/issues/17101
    # Until Terraform supports explicit module inter-dependencies, we create an implicit
    # dependency by using outputs from the Habitat and Airgap modules.
    command = "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\""
  }

  provisioner "local-exec" {
    command = "scp -P ${var.ssh_port} -o StrictHostKeyChecking=no -i ${var.ssh_key_file} ${var.ssh_user}@${var.private_ips[0]}:${var.tmp_path}/bootstrap.abb bootstrap${var.cluster_id}.abb"
  }

  depends_on = [null_resource.automate]
}
