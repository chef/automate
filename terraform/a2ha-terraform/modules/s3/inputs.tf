variable "aws_region" {
  default     = ""
  description = "The name of the selected AWS region / datacenter."
}

variable "aws_s3_bucketName" {
  default = "chef-automate-ha"
}

variable "destroy_bucket" {
  default = false
}

variable "name" {
  default = "chef-automate-ha"
}

variable "random_id" {
}

variable "tags" {
}