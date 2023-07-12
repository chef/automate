output "automate_frontend_url" {
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

output "opensearch_ssh" {
  value = formatlist(
    "ssh -i %s %s@%s",
    var.aws_ssh_key_file,
    var.aws_ssh_user,
    aws_instance.chef_automate_opensearch.*.private_ip,
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

output "opensearch_private_ips" {
  value = aws_instance.chef_automate_opensearch.*.private_ip
}

output "aws_os_snapshot_role_arn" {
  value = var.aws_os_snapshot_role_arn != "" ? var.aws_os_snapshot_role_arn : aws_iam_role.pass_es_role[0].arn
}

output "os_snapshot_user_access_key_id" {
  value = var.os_snapshot_user_access_key_id != "" ? var.os_snapshot_user_access_key_id : aws_iam_access_key.snap_reg_user_key[0].id
}

output "os_snapshot_user_access_key_secret" {
  value = var.os_snapshot_user_access_key_id != "" ? var.os_snapshot_user_access_key_secret : aws_iam_access_key.snap_reg_user_key[0].secret
}

output "random_id" {
  value = random_id.random.hex
}

output "subnet_id" {
  value = var.private_custom_subnets
}

output "mount_id" {
  value = aws_security_group.efs_mount.id
}

output "tag_name" {
  value = var.tag_name
}

output "tags" {
  value = var.tags
}


output "private_subnets" {
  value = var.private_custom_subnets
}

output "base_linux_aws_security_group_id" {
  value = aws_security_group.base_linux.id
}

output "aws_cluster_id" {
  value = random_id.random.hex
}

output "ssh_user" {
  value = var.aws_ssh_user
}
output "ssh_port" {
  value = var.aws_ssh_port
}
