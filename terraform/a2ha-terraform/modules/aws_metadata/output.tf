output "json_data" {
  value = "${data.http.getEc2PrivateIP.response_body}/32"
}

output "private_ip" {
  value = "${join(".", [for i, s in split(".",data.http.getEc2PrivateIP.response_body) : (
    i == 3 ? 0: s
  )])}/26"
}