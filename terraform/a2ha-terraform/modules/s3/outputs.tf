output "instance_profile_name" {
  value = aws_iam_instance_profile.backup_instance_profile.name
}

output "bucket_name" {
  value = local.log_bucket
}