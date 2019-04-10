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

# environment specific infra
data "terraform_remote_state" "a2_infrastructure" {
  backend = "s3"

  config {
    bucket  = "chef-cd-terraform-state"
    key     = "a2/${var.environment}.tfstate"
    region  = "us-west-2"
    profile = "chef-cd"
  }
}
