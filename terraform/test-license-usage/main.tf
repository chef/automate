resource "null_resource" "chef_automate_license_usage" {
  count = "${length(data.terraform_remote_state.a2_infrastructure.chef_server_performance_test_single_local_inplace_upgrade_fqdn)}"

  triggers = {
    always_do = "${uuid()}"
  }

  connection {
    type = "ssh"
    host = "${element(data.terraform_remote_state.a2_infrastructure.chef_server_performance_test_single_local_inplace_upgrade_fqdn, count.index)}"
    user = "${data.terraform_remote_state.a2_infrastructure.chef_server_performance_test_single_local_inplace_upgrade_ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo chef-automate license usage --result-json license-usage.json",
    ]
  }
}
