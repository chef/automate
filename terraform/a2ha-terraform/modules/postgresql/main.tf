locals {
  postgresql_user_toml = templatefile("${path.module}/templates/postgresql_user.toml.tpl", {
    listen_port                    = var.postgresql_listen_port,
    postgresql_pg_dump_enabled     = var.postgresql_pg_dump_enabled ? "true" : "false",
    postgresql_pg_dump_fs_path     = "${var.postgresql_archive_disk_fs_path}/pg_dump",
    postgresql_ssl_enable          = var.postgresql_ssl_enable ? "true" : "false",
    postgresql_wal_archive_enabled = var.postgresql_wal_archive_enabled ? "true" : "false",
    postgresql_wal_archive_fs_path = "${var.postgresql_archive_disk_fs_path}/archive",
    tmp_path                       = var.tmp_path
  })
  pgleaderchk_user_toml = templatefile("${path.module}/templates/pgleaderchk_user.toml.tpl", {
    listen_port = var.pgleaderchk_listen_port,
    tmp_path    = var.tmp_path
  })
  proxy_user_toml = templatefile("${path.module}/templates/proxy_user.toml.tpl", {
    listen_port = var.proxy_listen_port,
    tmp_path    = var.tmp_path
  })
  provision = templatefile("${path.module}/templates/provision.sh.tpl", {
    backend_aib_dest_file     = var.backend_aib_dest_file,
    journalbeat_pkg_ident     = var.journalbeat_pkg_ident,
    metricbeat_pkg_ident      = var.metricbeat_pkg_ident,
    postgresql_pkg_ident      = var.postgresql_pkg_ident,
    postgresql_svc_load_args  = var.postgresql_svc_load_args,
    pgleaderchk_pkg_ident     = var.pgleaderchk_pkg_ident,
    pgleaderchk_svc_load_args = var.pgleaderchk_svc_load_args,
    proxy_pkg_ident           = var.proxy_pkg_ident,
    proxy_svc_load_args       = var.proxy_svc_load_args,
    tmp_path                  = var.tmp_path
  })
  premount = templatefile("${path.module}/templates/pre_mount.tpl", {
    nfs_mount_path  = var.nfs_mount_path
  })
}

resource "null_resource" "postgresql" {
  count = var.postgresql_instance_count

  triggers = {
    template       = local.postgresql_user_toml
    template_pgl   = local.pgleaderchk_user_toml
    template_puser = local.proxy_user_toml
    template_prov  = local.provision
  }

  connection {
    user        = var.ssh_user
    port        = var.ssh_port
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
    script_path = "${var.tmp_path}/tf_inline_script_system_postgresql.sh"
  }

  provisioner "file" {
    destination = "${var.tmp_path}/postgresql-user.toml"
    content     = local.postgresql_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/pgleaderchk-user.toml"
    content     = local.pgleaderchk_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/proxy-user.toml"
    content     = local.proxy_user_toml
  }

  provisioner "file" {
    destination = "${var.tmp_path}/pg_provision.sh"
    content     = local.provision
  }

  provisioner "file" {
    destination = "${var.tmp_path}/pre_mount.sh"
    content     = local.premount
  }

  provisioner "remote-exec" {
    inline = [
      "chmod 0700 ${var.tmp_path}/pre_mount.sh",
      "${var.tmp_path}/pre_mount.sh",
    ]
  }

  provisioner "remote-exec" {
    inline = [
      # https://github.com/hashicorp/terraform/issues/17101
      # Until Terraform supports explicit module inter-dependencies, we create an implicit
      # dependency by using outputs from the Habitat and Airgap modules.
      "echo \"Airgap Info: ${var.airgap_info}\nHabitat Info: ${var.habitat_info}\"",
      "chmod 0700 ${var.tmp_path}/pg_provision.sh",
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S ${var.tmp_path}/pg_provision.sh",
    ]
  }
}

