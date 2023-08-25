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

output "to_check_consolidated_status" {
  value = "chef-automate status summary"
}

output "to_check_detail_status" {
  value = "chef-automate status"
}

output "to_post_deployment_verify" {
  value = "chef-automate verify"
}