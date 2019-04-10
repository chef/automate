module "cd_infrastructure" {
  source      = "git@github.com:chef/es-terraform.git//modules/cd_common_infrastructure"
  environment = "delivered"
}

provider "aws" {
  region  = "${module.cd_infrastructure.aws_region}"
  profile = "${module.cd_infrastructure.aws_profile}"
}

data "external" "latest_hab_pkg" {
  program = ["bash", "-c", "${path.module}/scripts/latest-hab-pkg.sh"]

  query = {
    channel = "${var.channel}"
  }
}

module "automate_www_site_hugo" {
  source    = "git@github.com:chef/es-terraform.git//modules/cd_hab_hugo_static_site"
  subdomain = "a2-docs-${var.dns_suffix}"

  pkg_ident   = "${data.external.latest_hab_pkg.result.pkg_ident}"
  fastly_fqdn = "${var.fastly_fqdn}"

  # AWS Tags
  tag_dept    = "CoreEng"
  tag_contact = "releng"
}
