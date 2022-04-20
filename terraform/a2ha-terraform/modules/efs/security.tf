
locals {                                                            
  subnet_ids_string = join(",", var.private_subnets)
  subnet_ids_list = split(",", local.subnet_ids_string)             
} 

locals {                                                            
  private_custom_subnets_ids_string = join(",", var.private_custom_subnets)
  private_custom_subnets_ids_list = split(",", local.private_custom_subnets_ids_string)             
}

data "aws_subnet" "default" {                                  
  count = 3            
  id    = length(var.private_custom_subnets) > 0 ? local.private_custom_subnets_ids_list[count.index] : local.subnet_ids_list[count.index]
} 


data "aws_vpc" "default" {  
  id = var.aws_vpc_id
}


resource "aws_security_group" "efs_mount" {
  count = var.efs_creation == "true" ? 1 : 0
  
  name        = "efs_${var.aws_cluster_id}"
  description = "NFS for EFS"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, map("Name", "${var.tag_name}_${var.aws_cluster_id}_efs_security_group"))
}

//////////////////////////
// EFS rules
resource "aws_security_group_rule" "ingress_efs_nfs_2049" {
  count = var.efs_creation == "true" ? 1 : 0
  type                     = "ingress"
  from_port                = 2049
  to_port                  = 2049
  protocol                 = "tcp"
  cidr_blocks              = ["0.0.0.0/0"]
  security_group_id        = aws_security_group.efs_mount[0].id
}
