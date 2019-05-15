provider "aws" {
  region  = "us-west-2"
  profile = "chef-cd"
}

data "aws_s3_bucket_object" "aws_private_key" {
  bucket = "chef-cd-citadel"
  key    = "cd-infrastructure-aws"
}

data "aws_s3_bucket_object" "wilcard_chef_co_crt" {
  bucket = "chef-cd-citadel"
  key    = "wildcard.chef.co.crt"
}

data "aws_s3_bucket_object" "wilcard_chef_co_key" {
  bucket = "chef-cd-citadel"
  key    = "wildcard.chef.co.key"
}

data "aws_s3_bucket_object" "internal_license" {
  bucket = "chef-cd-citadel"
  key    = "a2-license"
}

data "template_file" "install_chef_automate_cli" {
  template = "${file("${path.module}/templates/install_chef_automate_cli.sh.tpl")}"

  vars {
    admin_password         = "${var.admin_password}"
    airgapped              = "${var.airgapped}"
    channel                = "${var.channel}"
    chef_server_admin_name = "${var.chef_server_admin_name}"
    chef_server_org        = "${var.chef_server_org}"
    create_admin_token     = "${var.create_admin_token}"
    enable_chef_server     = "${var.enable_chef_server}"
    iam_version            = "${var.iam_version}"
    enable_workflow        = "${var.enable_workflow}"
    hardened_security      = "${var.hardened_security}"
    upgrade                = "${var.upgrade}"
    workflow_enterprise    = "${var.workflow_enterprise}"
  }
}

module "chef_baseline" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_base"

  instance_id   = "${var.instance_id}"
  instance_fqdn = "${var.instance_fqdn}"
  ssh_username  = "${var.ssh_username}"

  enable_email      = "${var.enable_email}"
  enable_monitoring = "${var.enable_monitoring}"
  chef_environment  = "${var.chef_environment}"
}

resource "null_resource" "chef_automate_cli_deploy" {
  count = "${var.instance_count}"

  depends_on = ["module.chef_baseline"]

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
    destination = "/tmp/chef-automate_journald.conf"

    content = <<EOF
[Journal]
Storage=persistent
SystemMaxUse=${var.journald_system_max_use}
SystemMaxFileSize=1G
RateLimitBurst=0
RateLimitInterval=0
EOF
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo mkdir -p /etc/systemd/journald.conf.d",
      "sudo cp /tmp/chef-automate_journald.conf /etc/systemd/journald.conf.d/chef-automate_journald.conf",
      "sudo systemctl restart systemd-journald",
    ]
  }

  provisioner "file" {
    destination = "/tmp/airgapped-iptables.rules"

    content = <<EOF
*filter

# allow inbound traffic
:INPUT ACCEPT [4312:2537760]
:FORWARD ACCEPT [0:0]

# set DROP policy for all outbound traffic
:OUTPUT DROP [78:6704]

# allow outbound traffic for established connections
# this allows external systems like our CI infrastructure and developers' workstations
# to connect to the instance to provision it, troubleshoot it or perform acceptance testing
# while at the same time rejecting any unauthorized outbound traffic that is initiated by
# the instance
-A OUTPUT -m conntrack --ctstate ESTABLISHED -j ACCEPT

# allow outbound traffic to localhost
-A OUTPUT -o lo -j ACCEPT

# allow outbound traffic for DNS requests
# this is necessary for things like the sssd service to
# resolve Active Directory hostnames
-A OUTPUT -p udp -m udp --dport 53 -j ACCEPT

# allow outbound traffic to NTP servers
-A OUTPUT -p udp -m udp --dport 123 -j ACCEPT

# allow outbound traffic to Active Directory servers for SSH auth
-A OUTPUT -d 172.21.8.10/32 -p tcp -m tcp --dport 636 -j ACCEPT
-A OUTPUT -d 172.21.8.74/32 -p tcp -m tcp --dport 636 -j ACCEPT

# allow outbound traffic to redis servers for sensu monitoring
-A OUTPUT -d 172.21.240.208/32 -p tcp -m tcp --dport 6379 -j ACCEPT
-A OUTPUT -d 172.21.246.42/32 -p tcp -m tcp --dport 6379 -j ACCEPT
COMMIT
EOF
  }

  provisioner "file" {
    destination = "/tmp/iptablesload"

    content = <<EOF
#!/bin/sh
iptables-restore < /etc/airgapped-iptables.rules
exit 0
EOF
  }

  # enable connection tracking early in iptables so the remote-exec that sets OUTPUT policy to DROP
  # won't intermittently lose its SSH connection and cause terraform to wait indefinitely for a response
  provisioner "remote-exec" {
    inline = [
      "set -e",
      "${var.airgapped} && sudo iptables -A OUTPUT -m conntrack --ctstate ESTABLISHED -j ACCEPT",
      "exit 0",
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "${var.airgapped} && sudo mv /tmp/airgapped-iptables.rules /etc/airgapped-iptables.rules",
      "${var.airgapped} && sudo mv /tmp/iptablesload /etc/network/if-pre-up.d/iptablesload",
      "${var.airgapped} && sudo chmod a+x /etc/network/if-pre-up.d/iptablesload",
      "${var.airgapped} && sudo /etc/network/if-pre-up.d/iptablesload",
      "exit 0",
    ]
  }

  provisioner "file" {
    source      = "../../../inspec/a2-hardened-security"
    destination = "/tmp"
  }

  provisioner "file" {
    destination = "/tmp/install_chef_automate_cli.sh"
    content     = "${data.template_file.install_chef_automate_cli.rendered}"
  }

  provisioner "file" {
    destination = "/tmp/60-chef-automate.conf"

    content = <<EOF
# Kernel parameters required by A2 preflight check
vm.max_map_count=262144
vm.dirty_expire_centisecs=20000
EOF
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo mv /tmp/60-chef-automate.conf /etc/sysctl.d/60-chef-automate.conf",
      "sudo --login sysctl --system",
    ]
  }

  provisioner "file" {
    destination = "/tmp/chef-automate-config.toml"

    content = <<TOML
[global.v1]
  fqdn = "${var.alb_fqdn == "" ? element(var.instance_fqdn, count.index) : var.alb_fqdn}"

[deployment.v1]
  [deployment.v1.svc]
    channel = "${var.channel}"
    deployment_type = "${var.deployment_type}"
    upgrade_strategy = "${var.upgrade == "true" ? "at-once" : "none"}"
    manifest_cache_expiry = "0s"
    enable_chef_server = ${var.enable_chef_server}
    enable_workflow = ${var.enable_workflow}

[gateway.v1]
  [gateway.v1.sys]
    [gateway.v1.sys.service]
      trial_license_url = "https://licensing-${var.channel}.chef.io/create-trial"
      enable_apps_feature = ${var.enable_eas_dashboard}

[event_gateway.v1]
  [event_gateway.v1.sys]
    [event_gateway.v1.sys.service]
      enable_nats_feature = ${var.enable_eas_dashboard}

[event_service.v1]
  [event_service.v1.sys]
    [event_service.v1.sys.service]
      enable_nats_feature = ${var.enable_eas_dashboard}

[applications.v1]
  [applications.v1.sys]
    [applications.v1.sys.service]
      enable_nats_feature = ${var.enable_eas_dashboard}

[load_balancer.v1]
  [load_balancer.v1.sys]
    [[load_balancer.v1.sys.frontend_tls]]
      cert = """${join("\n", formatlist("%s", split("\n", data.aws_s3_bucket_object.wilcard_chef_co_crt.body)))}"""
      key = """${join("\n", formatlist("%s", split("\n", data.aws_s3_bucket_object.wilcard_chef_co_key.body)))}"""

[license_control.v1]
  [license_control.v1.svc]
    license = "${data.aws_s3_bucket_object.internal_license.body}"
  [license_control.v1.sys.telemetry]
    url = "https://telemetry-acceptance.chef.io"

[dex.v1]
  [dex.v1.sys]
    [dex.v1.sys.connectors.saml]
      ca_contents = """-----BEGIN CERTIFICATE-----
MIIDnjCCAoagAwIBAgIGAUtB26KcMA0GCSqGSIb3DQEBBQUAMIGPMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzENMAsGA1UECgwET2t0YTEU
MBIGA1UECwwLU1NPUHJvdmlkZXIxEDAOBgNVBAMMB2dldGNoZWYxHDAaBgkqhkiG9w0BCQEWDWlu
Zm9Ab2t0YS5jb20wHhcNMTUwMTMxMjExNzA4WhcNNDUwMTMxMjExODA4WjCBjzELMAkGA1UEBhMC
VVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lzY28xDTALBgNVBAoM
BE9rdGExFDASBgNVBAsMC1NTT1Byb3ZpZGVyMRAwDgYDVQQDDAdnZXRjaGVmMRwwGgYJKoZIhvcN
AQkBFg1pbmZvQG9rdGEuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvaLXQkwf
XxRu8NBruKXftYwVo9+WuH2iw/6cZB1u1sxbXHDlDxGPA5e9kecQNRB/LE/My7byr/gNakAsNIg3
nTINxBe8pwKCGrNghzrCEbBxA0iphk/mYcM7+pkSqNZpRGPBUn8AIgxtihfUz/f56v2YhA15huO8
k8fJoUyjwXu9/BGCkCP16ksJ50r9IHI+qabTq4c1lMOGxZGbZ7tQjbpKdiAPclgaTzSdQ/9lomnR
uCvrnVwciDp60tGuAATdt68Re5X/5uOizlNh6k9snUWH9TQmIdyYn5bNtDa+3STXj0mIMVaAfiqQ
5pyrWRRXXb4Uqx4/9lQM1/Lh/O8yeQIDAQABMA0GCSqGSIb3DQEBBQUAA4IBAQCgSlK+ZlmQsYtz
A30/rbU5ZlW8/FtgcH7FjrSfYmfxi79Wtff3mHYDZjpPQQsncGnf+9BxwOEoBXVOoqwd+OSeWIJa
pSRbDj8Iog7ldXRLo3/+PzRrnjhrP6xj8VwPDFpzdj6Hn/QBhk0qjXd6gV7mrrAJzss3XwHKWPoC
8m2vkhGDLhmQBKCz18cVn+Z4Xhs0s9l9iWG+Ic9NBZu1KwxXI1e7yR2+xZRDPnBggDa410uDkXSb
bDZqKKny7qHKs4bioZ/HtS9NfgFV+pz1GpI50nw6ojItCPhqhgaFwtvf2brq9BHSK/DUmA3vF7/d
XoB1V6vwQXRubclyH8Ei2+1j
-----END CERTIFICATE-----
"""
     sso_url = "https://chef.okta.com/app/chefsoftware_a2localfreshinstallunstable_1/exk1d6ztz4rioEOYA1d8/sso/saml"
     entity_issuer = "https://automate.chef.co/dex/callback"
     email_attr = "email"
     username_attr = "email"
     groups_attr = "groups"

[ingest]
  [ingest.v1]
    [ingest.v1.sys]
      [ingest.v1.sys.service]
        purge_converge_history_after_days = ${var.enable_cloudwatch_metrics == "true" ? 90 : 10}
        purge_actions_after_days = ${var.enable_cloudwatch_metrics == "true" ? 90 : 10}

[compliance]
  [compliance.v1]
    [compliance.v1.sys]
      [compliance.v1.sys.retention]
        compliance_report_days = ${var.enable_cloudwatch_metrics == "true" ? 90 : 10}

[data_lifecycle]
  [data_lifecycle.v1]
    [data_lifecycle.v1.sys]
      [data_lifecycle.v1.sys.service]
        daily_run_at = "17:45:00"

[elasticsearch]
  [elasticsearch.v1]
    [elasticsearch.v1.sys]
      [elasticsearch.v1.sys.runtime]
        heapsize = "${var.enable_cloudwatch_metrics == "true" ? "16g" : "1g"}"
TOML
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chmod +x /tmp/install_chef_automate_cli.sh",
      "sudo bash --login /tmp/install_chef_automate_cli.sh",
    ]
  }
}

data "http" "chef_automate_admin_token" {
  count = "${var.create_admin_token == "true" ? var.instance_count : 0}"

  depends_on = ["null_resource.chef_automate_cli_deploy"]

  url = "https://${element(var.instance_fqdn, count.index)}/admin-token.txt"
}

data "http" "chef_automate_chef_server_admin_key" {
  count = "${var.enable_chef_server == "true" ? var.instance_count : 0}"

  depends_on = ["null_resource.chef_automate_cli_deploy"]

  url = "https://${element(var.instance_fqdn, count.index)}/chef-server-admin-key.txt"
}

resource "null_resource" "chef_automate_metrics_prereqs" {
  count = "${var.enable_cloudwatch_metrics == "true" ? var.instance_count : 0}"

  depends_on = ["null_resource.chef_automate_cli_deploy"]

  triggers = {
    instance_id = "${element(var.instance_id, count.index)}"
  }

  connection {
    type = "ssh"
    host = "${element(var.instance_fqdn, count.index)}"
    user = "${var.ssh_username}"

    private_key = "${data.aws_s3_bucket_object.aws_private_key.body}"
  }

  # Install requirements for the metrics scripts we bring in later.
  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo apt-get update",
      "sudo apt install -y python-pip unzip sysstat libswitch-perl libdatetime-perl libunix-syslog-perl libio-all-lwp-perl libdigest-sha-perl",
      "sudo pip install awscli",
      "wget http://aws-cloudwatch.s3.amazonaws.com/downloads/CloudWatchMonitoringScripts-1.2.1.zip",
      "sudo unzip -o -d / CloudWatchMonitoringScripts-1.2.1.zip",
      "wget -O /tmp/jq https://github.com/stedolan/jq/releases/download/jq-1.5/jq-linux64",
      "sudo install /tmp/jq /usr/local/bin/",
    ]
  }
}

resource "null_resource" "chef_automate_metrics" {
  count = "${var.enable_cloudwatch_metrics == "true" ? var.instance_count : 0}"

  depends_on = ["null_resource.chef_automate_metrics_prereqs"]

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
    source      = "${path.module}/files/collect_metrics"
    destination = "/tmp"
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sed -i 's|--namespace A2_Performance_Test_dev|--namespace ${var.cloudwatch_namespace}|' /tmp/collect_metrics/*",
      "chmod a+x /tmp/collect_metrics/*",
      "sudo rm -rf /opt/collect_metrics",
      "sudo mv /tmp/collect_metrics /opt",
    ]
  }

  provisioner "file" {
    source      = "${path.module}/files/cloudwatch_cron"
    destination = "/tmp/cloudwatch_cron"
  }

  provisioner "remote-exec" {
    inline = [
      "set -e",
      "sudo cp /tmp/cloudwatch_cron /etc/cron.d/cloudwatch",
    ]
  }
}

data "aws_region" "current" {}

data "template_file" "performance_cloudwatch_dashboard" {
  count = "${var.enable_cloudwatch_metrics == "true" ? 1 : 0}"

  template = "${file("${path.module}/templates/performance_cloudwatch_dashboard.tpl")}"

  vars {
    cloudwatch_namespace = "${var.cloudwatch_namespace}"
    aws_region           = "${data.aws_region.current.name}"
  }
}

resource "aws_cloudwatch_dashboard" "performance_cloudwatch_dashboard" {
  count = "${var.enable_cloudwatch_metrics == "true" ? 1 : 0}"

  dashboard_name = "${var.cloudwatch_namespace}"

  dashboard_body = "${data.template_file.performance_cloudwatch_dashboard.rendered}"
}
