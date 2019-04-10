# 
# Common Data Source
# Data sources that should be shared across multiple scenario files should be placed here.
#

module "cd_infrastructure" {
  source      = "git@github.com:chef/es-terraform.git//modules/cd_common_infrastructure"
  environment = "${var.environment}"
}

provider "aws" {
  region  = "${module.cd_infrastructure.aws_region}"
  profile = "${module.cd_infrastructure.aws_profile}"
}

#
# Pull keys from citadel
#

data "aws_s3_bucket_object" "chef_user_key" {
  bucket = "chef-cd-citadel"
  key    = "cd-infrastructure-chef-user.pem"
}

data "aws_s3_bucket_object" "aws_private_key" {
  bucket = "chef-cd-citadel"
  key    = "cd-infrastructure-aws"
}

data "aws_s3_bucket_object" "pager_duty_token" {
  bucket = "chef-cd-citadel"
  key    = "pager_duty_token"
}

# Configure the PagerDuty provider
provider "pagerduty" {
  token = "${data.aws_s3_bucket_object.pager_duty_token.body}"
}

data "pagerduty_team" "release_engineering" {
  name = "Release Engineering"
}

data "pagerduty_user" "chef_ci_service_account" {
  email = "chef-ci@chef.io"
}

data "pagerduty_escalation_policy" "nonpaging" {
  name = "Non-paging Escalation Policy"
}

data "pagerduty_vendor" "cloudwatch" {
  name = "Cloudwatch"
}
