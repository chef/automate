locals {
  automate_custom_config = "${var.tmp_path}/automate_custom_config.toml"

  connector_toml = templatefile("${path.module}/templates/connector.toml.tpl", {
    automate_fqdn             = var.automate_fqdn,
    automate_admin_email      = var.automate_admin_email,
    automate_admin_username   = var.automate_admin_username,
    automate_admin_password   = var.automate_admin_password,
    automate_custom_config    = local.automate_custom_config,
    automate_dc_token         = var.automate_dc_token,
    automate_role             = var.automate_role,
    elasticsearch_ips         = jsonencode(formatlist("%s", var.elasticsearch_private_ips)),
    elasticsearch_listen_port = var.elasticsearch_listen_port,
    postgresql_ips            = jsonencode(formatlist("%s", var.postgresql_private_ips)),
    postgresql_ssl_enable     = var.postgresql_ssl_enable ? "true" : "false",
    proxy_listen_port         = var.proxy_listen_port,
    teams_port                = var.teams_port,
    tmp_path                  = var.tmp_path,
  })

  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    admin_password                  = var.automate_admin_password,
    automate_custom_config          = local.automate_custom_config,
    automate_role                   = var.automate_role,
    backend_aib_file                = var.backend_aib_dest_file,
    frontend_aib_file               = var.frontend_aib_dest_file,
    hab_sup_http_gateway_auth_token = var.hab_sup_http_gateway_auth_token,
    ssh_user                        = var.ssh_user,
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
    script_path = "${var.tmp_path}/tf_inline_script_system_automate.sh"
  }

  triggers = {
    connector_toml_sha = sha1(local.connector_toml)
    template = local.provision
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
    script_path = "${var.tmp_path}/tf_inline_script_system_automate.sh"
  }

  provisioner "local-exec" {
    # https://github.com/hashicorp/terraform/issues/17101
    # Until Terraform supports explicit module inter-dependencies, we create an implicit
    # dependency by using outputs from the Habitat and Airgap modules.
    command = "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\""
  }

  provisioner "local-exec" {
    command = "scp -o StrictHostKeyChecking=no -i ${var.ssh_key_file} ${var.ssh_user}@${var.private_ips[0]}:${var.tmp_path}/bootstrap.abb bootstrap${var.cluster_id}.abb"
  }

  depends_on = [null_resource.automate]
}
