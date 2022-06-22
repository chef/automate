output "airgap_info" {
  value = "Airgap IDs:${join(",", null_resource.rsync.*.id)}\nAirgap Info: ${local.airgap_info}"
}
