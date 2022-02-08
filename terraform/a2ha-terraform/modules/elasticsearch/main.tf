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

module "journalbeat" {
  airgap_info               = var.airgap_info
  backend_aib_dest_file     = var.backend_aib_dest_file
  elasticsearch_listen_port = var.elasticsearch_listen_port
  habitat_info              = var.habitat_info
  instance_count            = var.elasticsearch_instance_count
  journalbeat_pkg_ident     = var.journalbeat_pkg_ident
  journalbeat_svc_binds     = "--bind elasticsearch:automate-ha-elasticsearch.default"
  journalbeat_tags          = ["elasticsearch"]
  elasticsearch_private_ips = var.private_ips
  private_ips               = var.private_ips
  source                    = "../journalbeat"
  ssh_key_file              = var.ssh_key_file
  ssh_user                  = var.ssh_user
  ssh_user_sudo_password    = var.ssh_user_sudo_password
  sudo_cmd                  = var.sudo_cmd
}

module "metricbeat" {
  airgap_info               = var.airgap_info
  backend_aib_dest_file     = var.backend_aib_dest_file
  elasticsearch_listen_port = var.elasticsearch_listen_port
  habitat_info              = var.habitat_info
  instance_count            = var.elasticsearch_instance_count
  metricbeat_pkg_ident      = var.metricbeat_pkg_ident
  metricbeat_svc_binds      = "--bind elasticsearch:automate-ha-elasticsearch.default"
  metricbeat_tags           = ["elasticsearch"]
  elasticsearch_private_ips = var.private_ips
  private_ips               = var.private_ips
  source                    = "../metricbeat"
  ssh_key_file              = var.ssh_key_file
  ssh_user                  = var.ssh_user
  ssh_user_sudo_password    = var.ssh_user_sudo_password
  sudo_cmd                  = var.sudo_cmd
}

module "kibana" {
  airgap_info               = var.airgap_info
  backend_aib_dest_file     = var.backend_aib_dest_file
  elasticsearch_listen_port = var.elasticsearch_listen_port
  habitat_info              = var.habitat_info
  instance_count            = var.elasticsearch_instance_count
  kibana_pkg_ident          = var.kibana_pkg_ident
  kibana_svc_binds          = "--bind elasticsearch:automate-ha-elasticsearch.default"
  private_ips               = var.private_ips
  source                    = "../kibana"
  ssh_key_file              = var.ssh_key_file
  ssh_user                  = var.ssh_user
  ssh_user_sudo_password    = var.ssh_user_sudo_password
  sudo_cmd                  = var.sudo_cmd
}

module "curator" {
  airgap_info               = var.airgap_info
  backend_aib_dest_file     = var.backend_aib_dest_file
  elasticsearch_listen_port = var.elasticsearch_listen_port
  habitat_info              = var.habitat_info
  instance_count            = var.elasticsearch_instance_count
  curator_pkg_ident         = var.curator_pkg_ident
  curator_svc_binds         = "--bind elasticsearch:automate-ha-elasticsearch.default"
  private_ips               = var.private_ips
  source                    = "../curator"
  ssh_key_file              = var.ssh_key_file
  ssh_user                  = var.ssh_user
  ssh_user_sudo_password    = var.ssh_user_sudo_password
  sudo_cmd                  = var.sudo_cmd
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
