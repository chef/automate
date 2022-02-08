##############################################################################################
# NOTE: Do not remove any of these outputs as they are required by the test harness framework.
#

output "ssh_key_file" {
  value = var.ssh_key_file
}

output "automate_frontend_urls" {
  value = "https://${var.automate_fqdn}"
}

output "automate_private_ips" {
  value = formatlist("%s", var.existing_automate_private_ips)
}

output "chef_server_private_ips" {
  value = formatlist("%s", var.existing_chef_server_private_ips)
}

output "elasticsearch_private_ips" {
  value = formatlist("%s", var.existing_elasticsearch_private_ips)
}

output "postgresql_private_ips" {
  value = formatlist("%s", var.existing_postgresql_private_ips)
}

output "elasticsearch_public_ips" {
  value = formatlist("%s", var.existing_elasticsearch_ips)
}

output "automate_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.existing_automate_private_ips,
  )
}

output "chef_server_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.existing_chef_server_private_ips,
  )
}

output "postgresql_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.existing_postgresql_private_ips,
  )
}

output "elasticsearch_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.existing_elasticsearch_ips,
  )
}

output "ops_dashboard_addresses" {
  value = formatlist("https://%s:5601/app/kibana#/dashboards?_g=()", var.existing_elasticsearch_ips)
}
