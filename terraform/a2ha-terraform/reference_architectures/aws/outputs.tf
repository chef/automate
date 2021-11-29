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

output "elasticsearch_private_ips" {
  value = formatlist("%s", module.aws.elasticsearch_private_ips)
}

output "elasticsearch_public_ips" {
  value = formatlist("%s", module.aws.elasticsearch_public_ips)
}

output "postgresql_private_ips" {
  value = formatlist("%s", module.aws.postgresql_private_ips)
}
