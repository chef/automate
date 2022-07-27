data "aws_s3_bucket_object" "aws_private_key" {
  bucket = "chef-cd-citadel"
  key    = "cd-infrastructure-aws"
}

data "template_file" "chef_load_toml" {
  count = "${var.instance_count}"

  template = "${file("${path.module}/templates/chef_load.toml.tpl")}"

  vars {
    api_get_requests            = "${join(",", formatlist("\"%s\"",var.api_get_requests))}"
    automate_server_fqdn        = "${var.automate_server_fqdn}"
    automate_server_token       = "${var.automate_server_token}"
    enable_chef_server_load     = "${var.enable_chef_server_load}"
    chef_server_client_name     = "${var.chef_server_client_name}"
    chef_server_fqdn            = "${var.chef_server_fqdn}"
    chef_server_org             = "${var.chef_server_org}"
    run_list                    = "${join(",", formatlist("\"%s\"",var.run_list))}"
    node_name_prefix            = "chef-load-${count.index + 1}"
    num_nodes                   = "${var.chef_load_nodes}"
    num_actions                 = "${var.chef_load_actions}"
    interval                    = "${var.chef_load_interval}"
    ohai_json_path              = "/opt/chef_load_sample_data/example-ohai.json"
    compliance_status_json_path = "/opt/chef_load_sample_data/example-compliance-status.json"
    converge_status_json_path   = "/opt/chef_load_sample_data/example-converge-status.json"
  }
}

data "template_file" "applications_load_gen_toml" {
  count = "${var.instance_count}"

  template = "${file("${path.module}/templates/applications_load_gen.toml.tpl")}"

  vars {
    automate_server_fqdn  = "${var.automate_server_fqdn}"
    automate_server_token = "${var.automate_server_token}"
    num_svcs              = "${var.apps_load_svcs}"
  }
}

data "template_file" "hab_sup_exec_start_conf" {
  count = "${var.instance_count}"

  template = "${file("${path.module}/templates/hab-sup-exec-start.conf.tpl")}"

  vars {
    automate_server_fqdn  = "${var.automate_server_fqdn}"
    automate_server_token = "${var.automate_server_token}"
  }
}

module "chef_load_cd_base" {
  source = "github.com/chef/es-terraform//modules/cd_base"

  instance_count = "${var.instance_count}"
  instance_id    = "${var.instance_id}"
  instance_fqdn  = "${var.instance_fqdn}"
  ssh_username   = "${var.ssh_username}"

  enable_monitoring = "${var.enable_monitoring}"
  chef_environment  = "${var.chef_environment}"
}

resource "null_resource" "habitat_install" {
  count = "${var.instance_count}"

  depends_on = ["module.chef_load_cd_base"]

  triggers = {
    instance_id = "${element(var.instance_id, count.index)}"
  }

  connection {
    type = "ssh"
    host = "${element(var.instance_fqdn, count.index)}"
    user = "${var.ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  provisioner "file" {
    destination = "/tmp/limits.conf"

    content = <<EOF
[Service]
LimitNOFILE=infinity
EOF
  }

  provisioner "file" {
    content     = "${element(data.template_file.hab_sup_exec_start_conf.*.rendered, count.index)}"
    destination = "/tmp/hab-sup-exec-start.conf"
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo mkdir -p /etc/systemd/system/hab-supervisor.service.d",
      "sudo chown root:root /tmp/limits.conf",
      "sudo chown root:root /tmp/hab-sup-exec-start.conf",
      "sudo mv /tmp/limits.conf /etc/systemd/system/hab-supervisor.service.d/limits.conf",
      "sudo mv /tmp/hab-sup-exec-start.conf /etc/systemd/system/hab-supervisor.service.d/hab-sup-exec-start.conf",
      "sudo systemctl daemon-reload",
    ]
  }

  provisioner "habitat" {
    service_type = "systemd"
  }
}

resource "null_resource" "chef_load_deploy" {
  count = "${var.instance_count}"

  depends_on = ["null_resource.habitat_install"]

  triggers = {
    always_do = "${uuid()}"
  }

  connection {
    type = "ssh"
    host = "${element(var.instance_fqdn, count.index)}"
    user = "${var.ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  provisioner "file" {
    destination = "/tmp/group-node-names"

    content = <<EOF
#!/bin/sh

sed -i "/^node_name_prefix/ s/\".*chef-load-/\"$(date +%F)-chef-load-/" /hab/user/chef-load/config/user.toml
EOF
  }

  provisioner "file" {
    destination = "/tmp/chef-load_logrotate.conf"

    content = <<CONF
"/var/log/chef-load/chef-load.log" {
  daily
  rotate 7
  compress
  copytruncate
}
CONF
  }

  provisioner "file" {
    source      = "${path.module}/files/chef_load_sample_data"
    destination = "/tmp"
  }

  provisioner "file" {
    content     = "${element(data.template_file.chef_load_toml.*.rendered, count.index)}"
    destination = "/tmp/chef-load_user.toml"
  }

  provisioner "file" {
    content     = "${element(data.template_file.applications_load_gen_toml.*.rendered, count.index)}"
    destination = "/tmp/applications-load-gen_user.toml"
  }

  provisioner "file" {
    content     = "${var.chef_server_client_key}"
    destination = "/tmp/chef-server-client-key.txt"
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo rm -f /hab/sup/default/specs/* || true",
      "sudo mv /bin/hab /bin/hab.old",
      "sudo HAB_NONINTERACTIVE=true HAB_LICENSE=accept /bin/hab.old pkg install core/hab/${var.desired_hab_version} --binlink --force && sudo rm /bin/hab.old",
      "sudo hab license accept",
      "sudo HAB_NONINTERACTIVE=true hab pkg install core/hab-sup/${var.desired_hab_version}",
      "sudo systemctl restart hab-supervisor.service",
      "for _ in {1..5}; do if sudo hab sup status; then break; else echo 'waiting' && sleep 5; fi; done",
      "sudo mv /tmp/chef-load_logrotate.conf /etc/logrotate.d/chef-load",
      "sudo chown root:root /etc/logrotate.d/chef-load",
      "sudo rm -rf /opt/chef_load_sample_data",
      "sudo mv /tmp/chef_load_sample_data /opt",
      "sudo mv /tmp/chef-server-client-key.txt /opt",
      "sudo mkdir -p /hab/user/chef-load/config",
      "sudo mkdir -p /hab/user/applications-load-gen/config",
      "sudo mv /tmp/chef-load_user.toml /hab/user/chef-load/config/user.toml",
      "sudo mv /tmp/applications-load-gen_user.toml /hab/user/applications-load-gen/config/user.toml",
      "sudo mv /tmp/group-node-names /etc/cron.hourly/group-node-names",
      "sudo chown root:root /etc/cron.hourly/group-node-names",
      "sudo chmod a+x /etc/cron.hourly/group-node-names",
      "sudo /etc/cron.hourly/group-node-names",
      "sudo mkdir -p /var/log/chef-load",
      "sudo chown hab /var/log/chef-load",
      "sudo HAB_NONINTERACTIVE=true hab svc load -f chef/chef-load --channel ${var.chef_load_channel} --strategy at-once",
      "sudo HAB_NONINTERACTIVE=true hab svc load -f chef/applications-load-gen --channel dev --strategy at-once",
    ]
  }
}
