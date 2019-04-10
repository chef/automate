#########################################################################
# Default
#########################################################################

variable "channel" {
  type        = "string"
  description = "With which channel the website is associated"
}

variable "dns_suffix" {
  type        = "string"
  description = "The suffix to append to the internal cd.chef.co subdomain"
}

variable "fastly_fqdn" {
  type        = "string"
  default     = ""
  description = "The Fastly service FQDN (leave empty to disable)"
}
