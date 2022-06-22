output "automate_url" {
  value = "https://${var.automate_fqdn}"
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

