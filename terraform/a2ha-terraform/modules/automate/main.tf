locals {
  automate_custom_config = "${var.tmp_path}/automate_custom_config.toml"

  automate_connector_toml = [
    for n in range(var.automate_instance_count) : templatefile("${path.module}/templates/connector.toml.tpl", {
      automate_fqdn                      = length(trimspace(var.automate_fqdn)) > 0 ? var.automate_fqdn : var.automate_lb_fqdn,
      automate_admin_email               = var.automate_admin_email,
      automate_admin_username            = var.automate_admin_username,
      automate_admin_password            = var.automate_admin_password,
      automate_custom_config             = local.automate_custom_config,
      automate_dc_token                  = var.automate_dc_token,
      automate_role                      = var.automate_role,
      aws_region                         = var.aws_region,
      opensearch_ips                     = jsonencode(formatlist("%s", var.opensearch_private_ips)),
      opensearch_listen_port             = var.opensearch_listen_port,
      managed_opensearch_domain_name     = var.managed_opensearch_domain_name,
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
      setup_self_managed_services        = var.setup_self_managed_services,
      opensearch_root_cert               = var.opensearch_root_cert,
      postgresql_root_cert               = var.postgresql_root_cert,
      teams_port                         = var.teams_port,
      tmp_path                           = var.tmp_path,
      backup_config_s3                   = var.backup_config_s3,
      backup_config_efs                  = var.backup_config_efs,
      nfs_mount_path                     = var.nfs_mount_path,
      s3_endpoint                        = var.s3_endpoint,
      bucket_name                        = var.bucket_name,
      access_key                         = var.access_key
      google_service_account_file        = var.google_service_account_file
      secret_key                         = var.secret_key
      location                           = var.var.location 
      infra                              = var.infra
      automate_root_ca                   = var.automate_root_ca
      automate_public_key                = contains(keys(var.automate_certs_by_ip), var.private_ips[n]) ? var.automate_certs_by_ip[element(var.private_ips, n)].public_key : var.automate_public_key
      automate_private_key               = contains(keys(var.automate_certs_by_ip), var.private_ips[n]) ? var.automate_certs_by_ip[element(var.private_ips, n)].private_key : var.automate_private_key
      chef_server_public_key             = contains(keys(var.chef_server_certs_by_ip), var.private_ips[n]) ? var.chef_server_certs_by_ip[element(var.private_ips, n)].public_key : var.chef_server_public_key
      chef_server_private_key            = contains(keys(var.chef_server_certs_by_ip), var.private_ips[n]) ? var.chef_server_certs_by_ip[element(var.private_ips, n)].private_key : var.chef_server_private_key
      opensearch_root_ca                 = var.opensearch_root_ca
      postgresql_root_ca                 = var.postgresql_root_ca
      automate_custom_certs_enabled      = var.automate_custom_certs_enabled
      chef_server_custom_certs_enabled   = var.chef_server_custom_certs_enabled
      postgresql_custom_certs_enabled    = var.postgresql_custom_certs_enabled
      opensearch_custom_certs_enabled    = var.opensearch_custom_certs_enabled
    })
  ]

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
    nfs_mount_path                  = var.nfs_mount_path
  })
}

# Below code is for the gcp, it should only execute when it is object_storage and location = gcp
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
    connector_toml_sha         = sha1(local.automate_connector_toml[count.index])
    template                   = local.provision
    automate_custom_config_sha = sha1(var.automate_config)
  }

  provisioner "file" {
    content     = var.automate_config
    destination = local.automate_custom_config
  }

  provisioner "file" {
    content     = local.automate_connector_toml[count.index]
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
