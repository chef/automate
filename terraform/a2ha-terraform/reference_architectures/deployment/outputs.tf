##############################################################################################
# NOTE: Do not remove any of these outputs as they are required by the test harness framework.
#

output "ssh_key_file" {
  value = var.ssh_key_file
}

output "automate_private_ips" {
  value = formatlist("%s", var.automate_private_ips)
}

output "chef_server_private_ips" {
  value = formatlist("%s", var.chef_server_private_ips)
}

output "elasticsearch_private_ips" {
  value = formatlist("%s", var.elasticsearch_private_ips)
}

output "elasticsearch_public_ips" {
  value = formatlist("%s", var.elasticsearch_public_ips)
}

output "postgresql_private_ips" {
  value = formatlist("%s", var.postgresql_private_ips)
}

output "automate_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.automate_private_ips,
  )
}

output "chef_server_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.chef_server_private_ips,
  )
}

output "postgresql_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.postgresql_private_ips,
  )
}

output "elasticsearch_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.ssh_key_file,
    var.ssh_user,
    var.elasticsearch_private_ips,
  )
}

output "ops_dashboard_addresses" {
  value = formatlist(
    "https://%s:5601/app/kibana#/dashboards?_g=()",
    var.elasticsearch_public_ips,
  )
}
