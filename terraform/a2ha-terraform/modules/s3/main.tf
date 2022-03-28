resource "aws_iam_instance_profile" "backup_instance_profile" {
  name = var.name
  role = aws_iam_role.backup_instance_role.name
}

resource "aws_iam_role" "backup_instance_role" {
  name               = var.name
  assume_role_policy = data.aws_iam_policy_document.backup_instance_role.json
  tags               = var.tags
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
  log_bucket = "${var.aws_s3_bucketName}-${var.random_id}"
}

resource "aws_s3_bucket" "createS3bucket" {
  bucket        = local.log_bucket
  force_destroy = true
}

resource "aws_s3_bucket_acl" "elb_bucket_acl" {
  bucket = local.log_bucket
  acl    = "private"
}