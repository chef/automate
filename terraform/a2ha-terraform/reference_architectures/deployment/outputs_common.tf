output "automate_url" {
  value = "https://${length(var.automate_fqdn) > 0 ? var.automate_fqdn : var.automate_lb_fqdn}"
}

output "automate_admin_user" {
  value = var.automate_admin_username
}

output "automate_admin_password" {
  value = var.automate_admin_password
}

output "automate_data_collector_token" {
  value = var.automate_dc_token
}

