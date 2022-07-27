output "dashboard" {
  value = "${module.dashboard.fqdn}"
}

output "chef_server_performance_test_single_local_inplace_upgrade_fqdn" {
  value = "${module.chef_server_performance_test_single_local_inplace_upgrade.fqdn}"
}

output "chef_server_performance_test_single_local_inplace_upgrade_ssh_username" {
  value = "${module.chef_server_performance_test_single_local_inplace_upgrade.ssh_username}"
}
