output "json_data" {
  value = "${data.http.getBastionRole.status_code}" == 200 ? "" : "Failed to get IAM Role"
}
