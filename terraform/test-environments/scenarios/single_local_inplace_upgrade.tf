#
# A simple local install of A2 using the chef-automate CLI. It currently uses a pseudo-inplace upgrade.
#

module "single_local_inplace_upgrade" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_instance_v2"

  # DNS components ( a2-local-inplace-upgrade-{{channel}}.cd.chef.co )
  subdomain        = "a2-local-inplace-upgrade"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "Single Local (Inplace Upgrade)"
  meta_description = "A2 stack (using SAML) deployed locally as Habitat packages on a single host using the chef-automate CLI."
  meta_type        = "habitat"

  # AWS Instance Configuration
  vpc            = "${var.environment}"
  platform       = "ubuntu-16.04"
  key_name       = "cd-infrastructure"
  instance_type  = "m4.large"
  root_volume_gb = "200"

  # Required AWS Tags
  tag_dept        = "CoreEng"
  tag_contact     = "${var.aws_tag_contact}"
  tag_application = "a2"

  additional_tags = {
    X-Package-Type     = "habitat"
    X-Install-Utility  = "chef-automate-cli"
    X-Install-Strategy = "inplace-upgrade"
    X-Topology         = "single"
    X-Deployment-Type  = "local"
    X-Channel          = "${var.channel}"
    X-SAML             = "saml"
  }
}

module "single_local_inplace_upgrade_deploy" {
  source = "../modules/chef_automate_install"

  instance_id   = "${module.single_local_inplace_upgrade.instance_id}"
  instance_fqdn = "${module.single_local_inplace_upgrade.fqdn}"
  ssh_username  = "${module.single_local_inplace_upgrade.ssh_username}"

  journald_system_max_use = "${var.channel == "acceptance" ? "20G" : "6G"}"

  # Chef Baseline
  enable_monitoring = "true"
  chef_environment  = "${var.chef_environment}"

  # Automate Install
  channel         = "${var.channel}"
  deployment_type = "local"
  upgrade         = "true"

  # SAML
  saml = "true"
}
