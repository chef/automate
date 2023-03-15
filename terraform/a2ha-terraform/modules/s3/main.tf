resource "aws_iam_instance_profile" "backup_instance_profile" {
  name = "${var.name}-${var.random_id}"
  role = aws_iam_role.backup_instance_role.name
}

resource "aws_iam_role" "backup_instance_role" {
  name               = "${var.name}-${var.random_id}"
  assume_role_policy = data.aws_iam_policy_document.backup_instance_role.json
}

data "aws_iam_policy_document" "backup_instance_role" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}


resource "aws_iam_role_policy_attachment" "ec2_full_access" {
  role       = aws_iam_role.backup_instance_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}

resource "aws_iam_role_policy_attachment" "provisioner_s3_full_access" {
  role       = aws_iam_role.backup_instance_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonS3FullAccess"
}

resource "aws_iam_role_policy_attachment" "AmazonAPIGatewayAdministrator" {
  role       = aws_iam_role.backup_instance_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonAPIGatewayAdministrator"
}

resource "aws_iam_role_policy_attachment" "AdministratorAccess" {
  role       = aws_iam_role.backup_instance_role.name
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}


locals {
  log_bucket = "${var.aws_s3_bucketName}"
}

# Creating s3 bucket using AWS-CLI (hab -> core/aws-cli)
resource "null_resource" "createS3bucket" {
  provisioner "local-exec" {
    command = "chmod 700 ${path.module}/templates/createS3Bucket.sh; ${path.module}/templates/createS3Bucket.sh ${var.aws_s3_bucketName} ${var.aws_region}"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "encryptS3bucket" {
  bucket = local.log_bucket

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm     = "aws:kms"
    }
    bucket_key_enabled = true
  }
}

resource "aws_s3_bucket_public_access_block" "blockPublicAccess" {
  bucket = local.log_bucket

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}