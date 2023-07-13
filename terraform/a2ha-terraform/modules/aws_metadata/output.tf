output "json_data" {
  value = "${data.http.getEc2PrivateIP.response_body}/32"
}

output "bastion_role" {
  value = "${data.http.getBastionRole.status_code}/32"
}