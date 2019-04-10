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
# Chef Load
#

variable "chef_load_channel" {
  default     = "stable"
  description = "The habitat builder channel to use when deploying chef-load."
}

variable "automate_server_fqdn" {
  description = "The A2 FQDN that chef-load will connect to."
}

variable "automate_server_token" {
  description = "The A2 API token chef-load will use when making API requests."
}

variable "enable_chef_server_load" {
  default     = "false"
  description = "Enable Chef Server load."
}

variable "chef_server_client_name" {
  default     = ""
  description = "The A2 Chef Server client name chef-load will use when making API requests."
}

variable "chef_server_client_key" {
  default     = "default_data"
  description = "The A2 Chef Server client key chef-load will use when making API requests."
}

variable "chef_server_fqdn" {
  default     = ""
  description = "The A2 Chef Server FQDN chef-load will use when making API requests."
}

variable "chef_server_org" {
  default     = ""
  description = "The A2 Chef Server organization chef-load will use when making API requests."
}

variable "run_list" {
  type        = "list"
  default     = []
  description = "The run list chef-load will use."
}

variable "api_get_requests" {
  type        = "list"
  default     = []
  description = "Additional Chef Server API GET requests that chef-load will make during each CCR."
}

variable "chef_load_instance_type" {
  default     = "c5.large"
  description = "The EC2 instance type for the chef-load instance."
}

variable "chef_load_interval" {
  default     = "30"
  description = "The interval between each node's Chef Client Run."
}

variable "chef_load_nodes" {
  default     = "10000"
  description = "The number of nodes that chef-load will simulate."
}

variable "chef_load_actions" {
  default     = "1000"
  description = "The number of Chef Actions that chef-load will simulate."
}
