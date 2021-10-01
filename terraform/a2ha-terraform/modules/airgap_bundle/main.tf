locals {
  checksum_info = [
    for bundle in var.bundle_files:
    format("%s %s",
      element(split(" ", file("transfer_files/${bundle.source}.md5")), 0),
      bundle.destination
    )
  ]
  rsync_files = [
    for bundle in var.bundle_files:
    format("%s,%s", bundle.source, bundle.destination)
  ]
  airgap_info = templatefile("${path.module}/templates/airgap.info.tpl", {
    archive_disk_info      = var.archive_disk_info,
    files                  = join(",", local.rsync_files),
    instance_count         = var.instance_count,
    tmp_path               = var.tmp_path,
    checksums              = join("\n", local.checksum_info)
  })
}

resource "null_resource" "rsync" {
  count = var.instance_count

  connection {
    user        = var.ssh_user
    private_key = file(var.ssh_key_file)
    host        = var.private_ips[count.index]
  }

  triggers = {
    template = local.airgap_info
    always_run = timestamp()
  }

  provisioner "file" {
    destination = "${var.tmp_path}/airgap.info"
    content     = local.airgap_info
  }

  provisioner "local-exec" {
    command = "${path.module}/files/rsync_wrapper.sh -k ${var.ssh_key_file} -u ${var.ssh_user} -i ${var.private_ips[count.index]} -l ${join(",", local.rsync_files)} -p ${path.module}"
  }
}
