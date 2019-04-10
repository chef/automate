resource "null_resource" "create_airgap_bundle" {
  triggers = {
    always_do = "${uuid()}"
  }

  provisioner "local-exec" {
    command = "curl -o /tmp/chef-automate.zip https://packages.chef.io/files/${var.channel}/latest/chef-automate-cli/chef-automate_linux_amd64.zip"
  }

  provisioner "local-exec" {
    command = "unzip -o /tmp/chef-automate.zip -d /tmp"
  }

  provisioner "local-exec" {
    command = "chmod +x /tmp/chef-automate"
  }

  provisioner "local-exec" {
    command = "/tmp/chef-automate airgap bundle create /tmp/automate.aib --channel ${var.channel}"
  }
}
