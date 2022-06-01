##############################################################################################
# NOTE: Do not remove any of these outputs as they are required by the test harness framework.
#

output "ssh_key_file" {
  value = var.ssh_key_file
}

output "automate_private_ips" {
  value = formatlist("%s", module.aws.automate_private_ips)
}

output "chef_server_private_ips" {
  value = formatlist("%s", module.aws.chef_server_private_ips)
}

output "opensearch_private_ips" {
  value = formatlist("%s", module.aws.opensearch_private_ips)
}

output "aws_os_snapshot_role_arn" {
  value = module.aws.aws_os_snapshot_role_arn
}

output "os_snapshot_user_access_key_id" {
  value = module.aws.os_snapshot_user_access_key_id
}

output "os_snapshot_user_access_key_secret" {
  value = module.aws.os_snapshot_user_access_key_secret
}

output "postgresql_private_ips" {
  value = formatlist("%s", module.aws.postgresql_private_ips)
}
