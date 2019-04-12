variable "instance_count" {
  default     = 1
  description = "The number of instances to provision."
}

variable "instance_id" {
  type        = "list"
  description = "The AWS instance ID for the instance. Used to determine when re-provisioning is required."
}

variable "instance_fqdn" {
  type        = "list"
  description = "The FQDN for the instance. Typically an output of the cd_instance module."
}

variable "ssh_username" {
  type        = "string"
  description = "The SSH username for the instance. Typically an output of the cd_instance module."
}

variable "airgapped" {
  default     = "false"
  description = "Instance is airgapped."
}

variable "hardened_security" {
  default     = "false"
  description = "Instance has hardened security."
}

variable "enable_chef_server" {
  default     = "false"
  description = "Enables A2 Chef Server feature."
}

variable "chef_server_org" {
  default     = "test"
  description = "A2 Chef Server organization to create."
}

variable "chef_server_admin_name" {
  default     = "test-admin"
  description = "A2 Chef Server admin user to create."
}

variable "enable_cloudwatch_metrics" {
  default     = "false"
  description = "Collect CloudWatch metrics and create CloudWatch dashboard."
}

variable "cloudwatch_namespace" {
  default     = "A2_Performance_Test_dev"
  description = "CloudWatch namespace."
}

variable "create_admin_token" {
  default     = "false"
  description = "Create an API admin token."
}

variable "journald_system_max_use" {
  default     = "6G"
  description = "Controls how much disk space the journal may use up at most."
}

#
# Chef Baseline
#

variable "enable_email" {
  default     = "true"
  description = "Enable email on this instance."
}

variable "enable_monitoring" {
  default     = "true"
  description = "Enable Sensu monitoring on this instance."
}

variable "chef_environment" {
  default     = "_default"
  description = "The Chef Environment on chef-server.chef.co in which to put the instance."
}

#
# Deploy YAML Config
#

variable "upgrade" {
  default = "false"
}

variable "channel" {
  default = "dev"
}

variable "deployment_type" {
  default = "local"
}

variable "admin_password" {
  default = "chefautomate"
}

variable "alb_fqdn" {
  default = ""
}

variable "iam_version" {
  default     = "v1"
  description = "Enable major/minor versions of A2 IAM."
}

variable "enable_workflow" {
  default     = "false"
  description = "Enable A2 Workflow feature."
}

variable "workflow_enterprise" {
  default     = "demo"
  description = "A2 Workflow enterprise name."
}
