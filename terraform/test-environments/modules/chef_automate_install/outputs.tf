output "chef_automate_admin_token" {
  value       = "${element(concat(data.http.chef_automate_admin_token.*.body, list("")), 0)}"
  description = "An A2 admin API token."
}

output "chef_automate_chef_server_admin_name" {
  value       = "${var.chef_server_admin_name}"
  description = "An A2 Chef Server admin user."
}

output "chef_automate_chef_server_admin_key" {
  value       = "${element(concat(data.http.chef_automate_chef_server_admin_key.*.body, list("")), 0)}"
  description = "An A2 Chef Server admin key."
}

output "chef_automate_chef_server_org" {
  value       = "${var.chef_server_org}"
  description = "An A2 Chef Server organization."
}
