#
# This is a simple InSpec target that can be hooked up to a Compliance Scanner.
#

module "inspec_target_rhel7" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_instance_v2"

  # DNS components ( inspec-target-rhel7-{{channel}}.cd.chef.co )
  subdomain        = "inspec-target-rhel7"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "RHEL7 InSpec Target"
  meta_description = "Long-running target for Compliance Scanning"
  meta_type        = "inspec"

  # AWS Instance Configuration
  vpc            = "${var.environment}"
  platform       = "rhel-7"
  key_name       = "cd-infrastructure"
  instance_type  = "t2.medium"
  root_volume_gb = "20"

  # Required AWS Tags
  tag_dept        = "CoreEng"
  tag_contact     = "${var.aws_tag_contact}"
  tag_application = "a2"
}

module "inspec_target_rhel7_cd_base" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_base"

  instance_id   = "${module.inspec_target_rhel7.instance_id}"
  instance_fqdn = "${module.inspec_target_rhel7.fqdn}"
  ssh_username  = "${module.inspec_target_rhel7.ssh_username}"

  enable_monitoring = "${var.enable_monitoring}"
  chef_environment  = "${var.chef_environment}"
}

#
# This is a temporary workaround to get a user on the box that the Scanner can
# work as. In the future this user should be a service-user managed by the
# cd_base module.
#

data "aws_s3_bucket_object" "inspec_scanner_pub_key" {
  bucket = "chef-cd-citadel"
  key    = "inspec-scanner_id_rsa.pub"
}

resource "null_resource" "install_service_user" {
  depends_on = ["module.inspec_target_rhel7"]

  triggers = {
    instance_id = "${element(module.inspec_target_rhel7.instance_id, 0)}"
  }

  connection {
    type = "ssh"
    host = "${element(module.inspec_target_rhel7.fqdn, 0)}"
    user = "${module.inspec_target_rhel7.ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  provisioner "remote-exec" {
    inline = [
      "sudo /usr/sbin/useradd -m inspec-scanner",
      "echo 'inspec-scanner ALL=(ALL) NOPASSWD:ALL' | sudo tee /etc/sudoers.d/inspec-scanner",
      "sudo mkdir -p /home/inspec-scanner/.ssh",
    ]
  }

  provisioner "file" {
    destination = "/tmp/inspec_scanner_pub_key"
    content     = "${data.aws_s3_bucket_object.inspec_scanner_pub_key.body}"
  }

  provisioner "remote-exec" {
    inline = [
      "sudo mv /tmp/inspec_scanner_pub_key /home/inspec-scanner/.ssh/authorized_keys",
      "sudo chown -R inspec-scanner:inspec-scanner /home/inspec-scanner/.ssh",
      "sudo chmod 0600 /home/inspec-scanner/.ssh/authorized_keys",
    ]
  }
}
