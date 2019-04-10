#
# A simple local install of A2 using the chef-automate CLI. It is rebuilt everytime we run `terraform apply`.
# This uses a CIS level 1 v2.1.1 hardened RHEL 7 system.
#

module "single_hardened_local_fresh_install" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_instance_v2"

  # DNS components ( a2-hardened-local-fresh-install-{{channel}}.cd.chef.co )
  subdomain        = "a2-hardened-local-fresh-install"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "Hardened Single Local (Fresh Install)"
  meta_description = "A2 stack (using SAML) deployed locally as Habitat packages on a single host with hardened security using the chef-automate CLI."
  meta_type        = "habitat"

  # AWS Instance Configuration
  vpc            = "${var.environment}"
  platform       = "rhel-7-cis"
  key_name       = "cd-infrastructure"
  instance_type  = "m5.large"
  root_volume_gb = "50"
  always_rebuild = "true"

  # Required AWS Tags
  tag_dept        = "CoreEng"
  tag_contact     = "${var.aws_tag_contact}"
  tag_application = "a2"

  additional_tags = {
    X-Package-Type     = "habitat"
    X-Install-Utility  = "chef-automate-cli"
    X-Install-Strategy = "fresh-install"
    X-Topology         = "single"
    X-Deployment-Type  = "local"
    X-Channel          = "${var.channel}"
  }
}

module "single_hardened_local_fresh_install_deploy" {
  source = "../modules/chef_automate_install"

  instance_id   = "${module.single_hardened_local_fresh_install.instance_id}"
  instance_fqdn = "${module.single_hardened_local_fresh_install.fqdn}"
  ssh_username  = "${module.single_hardened_local_fresh_install.ssh_username}"

  journald_system_max_use = "${var.channel == "acceptance" ? "20G" : "6G"}"

  # Chef Baseline
  enable_email      = "false"
  enable_monitoring = "false"
  chef_environment  = "${var.chef_environment}"

  # Run Hardened Security Inspec Scan
  hardened_security = "true"

  # Automate Install
  channel         = "${var.channel}"
  deployment_type = "local"
}
