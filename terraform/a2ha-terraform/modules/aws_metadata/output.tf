output "json_data" {
  value = "${data.http.getEc2PrivateIP.response_body}/32"
}
