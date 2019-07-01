#
# Performance test a simple local install of A2 using the chef-automate CLI. It currently uses a pseudo-inplace upgrade.
#

module "performance_test_single_local_inplace_upgrade" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_instance_v2"

  # DNS components ( a2-perf-test-single-local-inplace-upgrade-{{channel}}.cd.chef.co )
  subdomain        = "a2-perf-test-single-local-inplace-upgrade"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "Performance Test Single Local (Inplace Upgrade)"
  meta_description = "Performance test A2 (inplace upgrade, using SAML). A2_Performance_Test_${var.dns_suffix} CloudWatch dashboard is available in the chef-cd AWS account. Contact #helpdesk for okta access."
  meta_type        = "habitat"

  # AWS Instance Configuration
  vpc            = "${var.environment}"
  platform       = "ubuntu-16.04"
  key_name       = "cd-infrastructure"
  instance_type  = "m5.2xlarge"
  root_volume_gb = "100"

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
  }
}

resource "aws_ebs_volume" "habitat_volume" {
  lifecycle {
    prevent_destroy = true

    ignore_changes = [
      "iops",
      "size",
      "type",
    ]
  }

  availability_zone = "${element(module.performance_test_single_local_inplace_upgrade.availability_zone, count.index)}"
  size              = "${4 * 1024}"
  type              = "gp2"

  tags {
    Name          = "${element(module.performance_test_single_local_inplace_upgrade.fqdn, count.index)}"
    X-Dept        = "CoreEng"
    X-Contact     = "${var.aws_tag_contact}"
    X-Application = "a2"
  }
}

module "attach_habitat_volume" {
  source = "git@github.com:chef/es-terraform.git//modules/attach_ebs_volume"

  actual_device_name = "/dev/nvme1n1"
  ec2_device_name    = "/dev/xvdh"
  instance_id        = "${module.performance_test_single_local_inplace_upgrade.instance_id}"
  instance_fqdn      = "${module.performance_test_single_local_inplace_upgrade.fqdn}"
  ssh_private_key    = "${data.aws_s3_bucket_object.aws_private_key.body}"
  ssh_username       = "${module.performance_test_single_local_inplace_upgrade.ssh_username}"
  volume_id          = "${aws_ebs_volume.habitat_volume.*.id}"
  volume_mount_point = "/hab"
}

module "performance_test_single_local_inplace_upgrade_deploy" {
  source = "../modules/chef_automate_install"

  instance_id   = "${module.attach_habitat_volume.instance_id}"
  instance_fqdn = "${module.performance_test_single_local_inplace_upgrade.fqdn}"
  ssh_username  = "${module.performance_test_single_local_inplace_upgrade.ssh_username}"

  journald_system_max_use = "${var.channel == "acceptance" ? "20G" : "6G"}"

  # Chef Baseline
  enable_monitoring = "false"
  chef_environment  = "${var.chef_environment}"

  # Automate Install
  channel              = "${var.channel}"
  deployment_type      = "local"
  enable_eas_dashboard = "true"
  upgrade              = "true"

  # SAML
  saml = "true"

  # Create admin-token
  create_admin_token = "true"

  # Setup CloudWatch
  enable_cloudwatch_metrics = "true"
  cloudwatch_namespace      = "A2_Performance_Test_${var.dns_suffix}"
}

#
# Create and configure Chef Load instances.
#

locals {
  performance_test_chef_load_count = 1
}

module "performance_test_chef_load" {
  source = "git@github.com:chef/es-terraform.git//modules/cd_instance_v2"

  instance_count = "${local.performance_test_chef_load_count}"

  # DNS components ( a2-perf-test-chef-load-{{channel}}.cd.chef.co )
  subdomain        = "a2-perf-test-chef-load"
  subdomain_suffix = "-${var.dns_suffix}"

  # Metadata
  meta_title       = "chef-load Instance"
  meta_description = "chef-load instance applying load to A2 for performance testing."
  meta_type        = "habitat"

  # AWS Instance Configuration
  vpc           = "${var.environment}"
  platform      = "ubuntu-16.04"
  key_name      = "cd-infrastructure"
  instance_type = "c5.large"

  # Required AWS Tags
  tag_dept        = "CoreEng"
  tag_contact     = "${var.aws_tag_contact}"
  tag_application = "a2"
}

module "performance_test_chef_load_deploy" {
  source = "../modules/chef_load_instance"

  instance_count = "${local.performance_test_chef_load_count}"
  instance_id    = "${module.performance_test_chef_load.instance_id}"
  instance_fqdn  = "${module.performance_test_chef_load.fqdn}"
  ssh_username   = "${module.performance_test_chef_load.ssh_username}"

  # Chef Baseline
  enable_monitoring = "false"
  chef_environment  = "${var.chef_environment}"

  # Chef Load Install
  automate_server_fqdn  = "${element(module.performance_test_single_local_inplace_upgrade.fqdn, 0)}"
  automate_server_token = "${module.performance_test_single_local_inplace_upgrade_deploy.chef_automate_admin_token}"
  chef_load_nodes       = "10000"
  chef_load_actions     = "1000"
  chef_load_interval    = "30"
}

# The following configuration provisions CloudWatch alarms that notify an AWS SNS topic which
# triggers/resolves incidents in a non-paging PagerDuty service named A2_Performance_${var.dns_suffix}.
# You must manually add a Slack channel integration to the PagerDuty service in order for
# triggered/resolved notifications to be forwarded to Slack.

resource "pagerduty_service" "a2_performance" {
  name                    = "A2_Performance_${var.dns_suffix}"
  auto_resolve_timeout    = "null"
  acknowledgement_timeout = 600

  escalation_policy = "${data.pagerduty_escalation_policy.nonpaging.id}"
  alert_creation    = "create_incidents"
}

resource "pagerduty_service_integration" "a2_performance_cloudwatch" {
  name    = "A2_Performance_${var.dns_suffix} Cloudwatch"
  service = "${pagerduty_service.a2_performance.id}"
  vendor  = "${data.pagerduty_vendor.cloudwatch.id}"
}

resource "aws_sns_topic" "a2_performance_notifier" {
  name = "A2_Performance_${var.dns_suffix}_Notifier"
}

resource "aws_sns_topic_subscription" "a2_performance_notifier_pagerduty_integration" {
  topic_arn              = "${aws_sns_topic.a2_performance_notifier.arn}"
  protocol               = "https"
  endpoint               = "https://events.pagerduty.com/integration/${pagerduty_service_integration.a2_performance_cloudwatch.integration_key}/enqueue"
  endpoint_auto_confirms = true
}

resource "aws_cloudwatch_metric_alarm" "a2_performance_2xxStatuses" {
  alarm_name          = "a2-performance-${var.dns_suffix}-2xxStatuses"
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = "2"
  metric_name         = "2xxStatuses"
  namespace           = "A2_Performance_Test_${var.dns_suffix}"
  period              = "300"
  statistic           = "Average"
  threshold           = "1500"
  alarm_description   = "This metric monitors A2_Performance_Test_${var.dns_suffix} 2xxStatuses"
  treat_missing_data  = "breaching"
  alarm_actions       = ["${aws_sns_topic.a2_performance_notifier.arn}"]
  ok_actions          = ["${aws_sns_topic.a2_performance_notifier.arn}"]
}

resource "aws_cloudwatch_metric_alarm" "a2_performance_5xxStatuses" {
  alarm_name          = "a2-performance-${var.dns_suffix}-5xxStatuses"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = "2"
  metric_name         = "5xxStatuses"
  namespace           = "A2_Performance_Test_${var.dns_suffix}"
  period              = "300"
  statistic           = "Average"
  threshold           = "10"
  alarm_description   = "This metric monitors A2_Performance_Test_${var.dns_suffix} 5xxStatuses"
  treat_missing_data  = "breaching"
  alarm_actions       = ["${aws_sns_topic.a2_performance_notifier.arn}"]
  ok_actions          = ["${aws_sns_topic.a2_performance_notifier.arn}"]
}
