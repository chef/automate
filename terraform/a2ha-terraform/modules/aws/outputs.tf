output "automate_frontend_urls" {
  value = "https://${aws_alb.automate_lb.dns_name}"
}

output "automate_fqdn" {
  value = var.automate_fqdn != "" ? var.automate_fqdn : aws_alb.automate_lb.dns_name
}

output "automate_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.aws_ssh_key_file,
    var.aws_ssh_user,
    aws_instance.chef_automate.*.private_ip,
  )
}

output "postgresql_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.aws_ssh_key_file,
    var.aws_ssh_user,
    aws_instance.chef_automate_postgresql.*.private_ip,
  )
}

output "elasticsearch_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.aws_ssh_key_file,
    var.aws_ssh_user,
    aws_instance.chef_automate_elasticsearch.*.private_ip,
  )
}

output "automate_private_ips" {
  value = aws_instance.chef_automate.*.private_ip
}

output "chef_server_private_ips" {
  value = aws_instance.chef_server.*.private_ip
}

output "postgresql_private_ips" {
  value = aws_instance.chef_automate_postgresql.*.private_ip
}

output "elasticsearch_public_ips" {
  value = aws_instance.chef_automate_elasticsearch.*.public_ip
}

output "elasticsearch_private_ips" {
  value = aws_instance.chef_automate_elasticsearch.*.private_ip
}

