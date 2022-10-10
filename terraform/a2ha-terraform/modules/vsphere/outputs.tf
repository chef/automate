output "automate_frontend_urls" {
  value = "https://${element(
    vsphere_virtual_machine.chef_automate.*.default_ip_address,
    0,
  )}"
}

output "automate_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.vsphere_linux_sshkeyfile,
    var.vsphere_linux_sshuser,
    vsphere_virtual_machine.chef_automate.*.default_ip_address,
  )
}

output "chef_server_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.vsphere_linux_sshkeyfile,
    var.vsphere_linux_sshuser,
    vsphere_virtual_machine.chef_server.*.default_ip_address,
  )
}

output "postgresql_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.vsphere_linux_sshkeyfile,
    var.vsphere_linux_sshuser,
    vsphere_virtual_machine.chef_automate_postgresql.*.default_ip_address,
  )
}

output "opensearch_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.vsphere_linux_sshkeyfile,
    var.vsphere_linux_sshuser,
    vsphere_virtual_machine.chef_automate_opensearch.*.default_ip_address,
  )
}

output "automate_private_ips" {
  value = [vsphere_virtual_machine.chef_automate.*.default_ip_address]
}

output "chef_server_private_ips" {
  value = [vsphere_virtual_machine.chef_server.*.default_ip_address]
}

output "postgresql_private_ips" {
  value = [vsphere_virtual_machine.chef_automate_postgresql.*.default_ip_address]
}

output "opensearch_private_ips" {
  value = [vsphere_virtual_machine.chef_automate_opensearch.*.default_ip_address]
}

output "opensearch_public_ips" {
  value = [vsphere_virtual_machine.chef_automate_opensearch.*.default_ip_address]
}

output "ssh_user" {
  value = var.aws_ssh_user
}
