#########################################################################
# Default
#########################################################################
variable "environment" {
  type        = "string"
  description = "Environment and associated workflow environment (dev, acceptance, union, rehearsal, or delivered)"
}

variable "dns_suffix" {
  type        = "string"
  description = "The suffix to append to all DNS names in this environment"
}

variable "enable_monitoring" {
  type        = "string"
  description = "Enable Sensu monitoring on this instance."
  default     = "true"
}

variable "channel" {
  type        = "string"
  description = "The Habitat channel from which to install the packages"
  default     = "dev"
}

variable "www_site_hugo_options" {
  type        = "string"
  description = "The options to pass into hugo when building the landing page"
  default     = ""
}

#########################################################################
# AWS
#########################################################################
variable "aws_tag_contact" {
  type        = "string"
  description = "Name of the user who own the instance (<FOO>@chef.io)"
  default     = "releng"
}

#########################################################################
# Chef
#########################################################################
variable "chef_environment" {
  type        = "string"
  description = "Chef environment new nodes will join"
}
