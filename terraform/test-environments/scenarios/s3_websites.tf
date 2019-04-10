#########################################################################
# a2-{{channel}}.cd.chef.co
#########################################################################
# This S3-hosted website is a quick dashboard that can be used to quickly see the available instances.

module "dashboard" {
  source    = "git@github.com:chef/es-terraform.git//modules/cd_s3_website"
  subdomain = "a2-${var.dns_suffix}"

  # Metadata
  meta_title       = "Automate Release Dashboard"
  meta_description = "This is just a link to the channel-specific release dashboard site you are viewing right now...recursion FTW!"

  tag_contact = "${var.aws_tag_contact}"
}

resource "null_resource" "dashboard_deploy" {
  triggers = {
    always_deploy = "${uuid()}"
  }

  depends_on = ["module.dashboard"]

  provisioner "local-exec" {
    command = "cd ../ && NG_ENV=${var.channel} DNS_SUFFIX=${var.dns_suffix} bash scripts/deploy-dashboard.sh"
  }
}

#########################################################################
# ui-library.cd.chef.co
#########################################################################
# Use to host the chef-ui-library documentation

module "ui_library_website" {
  source    = "git@github.com:chef/es-terraform.git//modules/cd_s3_website"
  subdomain = "ui-library"

  # Metadata
  meta_title       = "Chef UI Library"
  meta_description = "Documentation site for Chef's shared UI and Pattern Library"

  tag_contact = "${var.aws_tag_contact}"
  create      = "${var.environment == "union" ? "true" : "false"}"
}

#########################################################################
# a2-code-coverage.cd.chef.co
#########################################################################
# Use to host code coverage reports for master

module "a2_code_coverage_website" {
  source    = "git@github.com:chef/es-terraform.git//modules/cd_s3_website"
  subdomain = "a2-code-coverage"

  # Metadata
  meta_title       = "A2 Code Coverage"
  meta_description = "Code Coverage reports for A2"

  tag_contact = "${var.aws_tag_contact}"
  create      = "${var.environment == "union" ? "true" : "false"}"
}
