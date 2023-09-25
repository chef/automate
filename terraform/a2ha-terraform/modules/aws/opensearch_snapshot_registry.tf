resource "aws_iam_policy" "s3_access_policy" {
  count       =  var.aws_os_snapshot_role_arn != "" ? 0 : 1 
  name        = "automate-os-snapshot-access-policy-${random_id.random.hex}"
  path        = "/"
  description = "Policy to provide s3 access for AWS OS snapshots"
  policy      = <<POLICY
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:*",
                "s3-object-lambda:*"
            ],
            "Resource": "*"
        }
    ]
}
POLICY

  tags = merge(var.tags,
    tomap({"X-Application" = "automate-opensearch"})
  )
}

resource "aws_iam_role" "pass_es_role" {
  count               =  var.aws_os_snapshot_role_arn != "" ? 0 : 1
  name                = "automate-pass-es-role-${random_id.random.hex}"
  assume_role_policy  = <<TRUSTPOLICY
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Principal": {
              "Service": [
                "ec2.amazonaws.com",
                "opensearchservice.amazonaws.com"
              ]
            },
            "Action": "sts:AssumeRole"
        }
    ]
}
TRUSTPOLICY
  managed_policy_arns = [aws_iam_policy.s3_access_policy[0].arn]
  tags = merge(var.tags,
    tomap({"X-Application" = "automate-opensearch"})
  )

}

resource "aws_iam_policy" "pass_es_role_policy" {
  count       =  var.aws_os_snapshot_role_arn != "" ? 0 : 1
  name        = "pass-es-role-policy-${random_id.random.hex}"
  path        = "/"
  description = "Policy to pass ES access for Snapshot registry"
  policy      = <<POLICY
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": "iam:PassRole",
            "Resource": "arn:aws:iam::${local.account_id}:role/${aws_iam_role.pass_es_role[0].name}"
        },
        {
            "Effect": "Allow",
            "Action": "es:ESHttpPut",
            "Resource": "arn:aws:es:${var.aws_region}:${local.account_id}:domain/${var.managed_opensearch_domain_name}/*"
        }
    ]
}
POLICY
  tags = merge(var.tags,
    tomap({"X-Application" = "automate-opensearch"})
  )

}

resource "aws_iam_role_policy_attachment" "s3_policy_attachment" {
  count      =  var.aws_os_snapshot_role_arn != "" ? 0 : 1
  role       = aws_iam_role.pass_es_role[0].name
  policy_arn = aws_iam_policy.s3_access_policy[0].arn
}

resource "aws_iam_role_policy_attachment" "assume_role_policy_attachment" {
  count      =  var.aws_os_snapshot_role_arn != "" ? 0 : 1
  role       = aws_iam_role.pass_es_role[0].name
  policy_arn = aws_iam_policy.pass_es_role_policy[0].arn
}

resource "aws_iam_user" "snap_reg_user" {
  count  =  var.os_snapshot_user_access_key_id != "" ? 0 : 1
  name = "automate-os-snapshot-user-${random_id.random.hex}"
  tags = merge(var.tags,
    tomap({"X-Application" = "automate-opensearch"})
  )
}

resource "aws_iam_user_policy_attachment" "snap_reg_user_policy_attachment" {
  count      =  var.os_snapshot_user_access_key_id != "" ? 0 : 1
  user       = aws_iam_user.snap_reg_user[0].name
  policy_arn = aws_iam_policy.s3_access_policy[0].arn
}

resource "aws_iam_user_policy_attachment" "assume_role_policy_attachment" {
  count      =  var.os_snapshot_user_access_key_id != "" ? 0 : 1
  user       = aws_iam_user.snap_reg_user[0].name
  policy_arn = aws_iam_policy.pass_es_role_policy[0].arn
}

resource "aws_iam_access_key" "snap_reg_user_key" {
  count  =  var.os_snapshot_user_access_key_id != "" ? 0 : 1
  user   = aws_iam_user.snap_reg_user[0].name
}
