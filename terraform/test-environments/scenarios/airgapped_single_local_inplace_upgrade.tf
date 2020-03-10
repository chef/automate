#
# A simple local install of A2 using the chef-automate CLI. It currently uses a pseudo-inplace upgrade.
#

module "airgapped_single_local_inplace_upgrade" {
  source = "github.com/chef/es-terraform//modules/cd_instance_v2"

  # DNS components ( a2-airgapped-local-inplace-upgrade-{{channel}}.cd.chef.co )
  subdomain        = "a2-airgapped-local-inplace-upgrade"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "Airgapped Single Local (Inplace Upgrade)"
  meta_description = "A2 stack (using SAML) deployed locally from an airgap bundle on a single host using the chef-automate CLI."
  meta_type        = "habitat"

  # AWS Instance Configuration
  vpc            = "${var.environment}"
  platform       = "ubuntu-16.04"
  key_name       = "cd-infrastructure"
  instance_type  = "m5.large"
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

resource "null_resource" "airgapped_single_local_inplace_upgrade_upload_airgap_bundle" {
  triggers = {
    always_do = "${uuid()}"
  }

  depends_on = ["null_resource.create_airgap_bundle"]

  connection {
    type = "ssh"
    host = "${element(module.airgapped_single_local_inplace_upgrade.fqdn, count.index)}"
    user = "${module.airgapped_single_local_inplace_upgrade.ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  provisioner "file" {
    source      = "/tmp/chef-automate"
    destination = "/tmp/chef-automate"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod a+x /tmp/chef-automate",
      "sudo mv /tmp/chef-automate /usr/local/bin/chef-automate",
    ]
  }

  provisioner "file" {
    source      = "/tmp/automate.aib"
    destination = "/tmp/automate.aib"
  }
}

module "airgapped_single_local_inplace_upgrade_deploy" {
  source = "../modules/chef_automate_install"

  instance_id   = "${module.airgapped_single_local_inplace_upgrade.instance_id}"
  instance_fqdn = "${module.airgapped_single_local_inplace_upgrade.fqdn}"
  ssh_username  = "${module.airgapped_single_local_inplace_upgrade.ssh_username}"

  airgapped               = "${null_resource.airgapped_single_local_inplace_upgrade_upload_airgap_bundle.id != "" ? true : false}"
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
