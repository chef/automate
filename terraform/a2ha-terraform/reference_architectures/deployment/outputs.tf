##############################################################################################
# NOTE: Do not remove any of these outputs as they are required by the test harness framework.
#

output "ssh_key_file" {
  value = var.ssh_key_file
}

output "ssh_user" {
  value = var.ssh_user
}

output "ssh_group_name" {
  value = var.ssh_group_name
}

output "ssh_port" {
  value = var.ssh_port
}

output "automate_private_ips" {
  value = formatlist("%s", var.automate_private_ips)
}

output "chef_server_private_ips" {
  value = formatlist("%s", var.chef_server_private_ips)
}

output "opensearch_private_ips" {
  value = formatlist("%s", var.opensearch_private_ips)
}

output "opensearch_public_ips" {
  value = formatlist("%s", var.opensearch_public_ips)
}

output "postgresql_private_ips" {
  value = formatlist("%s", var.postgresql_private_ips)
}

output "automate_ssh" {
  value = formatlist(
    "ssh -i %s -p %s %s@%s",
    var.ssh_key_file,
    var.ssh_port,
    var.ssh_user,
    var.automate_private_ips,
  )
}

output "chef_server_ssh" {
  value = formatlist(
    "ssh -i %s -p %s %s@%s",
    var.ssh_key_file,
    var.ssh_port,
    var.ssh_user,
    var.chef_server_private_ips,
  )
}

output "postgresql_ssh" {
  value = formatlist(
    "ssh -i %s -p %s %s@%s",
    var.ssh_key_file,
    var.ssh_port,
    var.ssh_user,
    var.postgresql_private_ips,
  )
}

output "opensearch_ssh" {
  value = formatlist(
    "ssh -i %s -p %s %s@%s",
    var.ssh_key_file,
    var.ssh_port,
    var.ssh_user,
    var.opensearch_private_ips,
  )
}

output "backup_config_efs" {
  value = var.backup_config_efs
}

output "backup_config_s3" {
  value = var.backup_config_s3
}
