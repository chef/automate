
resource "random_id" "random" {
  byte_length = 4
}

data "aws_availability_zones" "available" {
}

data "aws_vpc" "default" {
  id = var.aws_vpc_id
}

locals {                                                            
  private_subnet_ids_string = join(",", var.private_custom_subnets)
  private_subnet_ids_list = split(",", local.private_subnet_ids_string)             
}

data "aws_subnet" "default" {                                  
  count = length(var.private_custom_subnets) > 0 ? 3 : 0            
  id    = local.private_subnet_ids_list[count.index]
}

locals {                                                            
  public_subnet_ids_string = join(",", var.public_custom_subnets)
  public_subnet_ids_list = split(",", local.public_subnet_ids_string)             
}

data "aws_subnet" "public" {                                  
  count = length(var.private_custom_subnets) > 0 ? 3 : 0            
  id    = local.public_subnet_ids_list[count.index]
}

data "aws_internet_gateway" "default" {
  filter {
    name   = "attachment.vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

resource "aws_subnet" "default" {
  count             = length(var.private_custom_subnets) > 0 ? 0 : 3
  vpc_id            = data.aws_vpc.default.id
  cidr_block        = cidrsubnet("${var.aws_cidr_block_addr}/18", 8, count.index + 1)
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_${data.aws_availability_zones.available.names[count.index]}_private"))
}

resource "aws_subnet" "public" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 3
  vpc_id                  = data.aws_vpc.default.id
  cidr_block              = cidrsubnet("${var.aws_cidr_block_addr}/18", 8, count.index + 4)
  availability_zone       = data.aws_availability_zones.available.names[count.index]
  map_public_ip_on_launch = true

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_${data.aws_availability_zones.available.names[count.index]}_public"))
}

resource "aws_eip" "eip1" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc              = true
  public_ipv4_pool = "amazon"

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_eip"))
}

resource "aws_eip" "eip2" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc              = true
  public_ipv4_pool = "amazon"

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_eip"))
}

resource "aws_eip" "eip3" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc              = true
  public_ipv4_pool = "amazon"

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_eip"))
}

resource "aws_nat_gateway" "nat1" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  allocation_id = aws_eip.eip1[0].id
  subnet_id     = length(var.public_custom_subnets) > 0 ? data.aws_subnet.public[0].id : aws_subnet.public[0].id

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_nat_gw"))

  depends_on = [data.aws_internet_gateway.default]
}

resource "aws_nat_gateway" "nat2" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  allocation_id = aws_eip.eip2[0].id
  subnet_id     = length(var.public_custom_subnets) > 0 ? data.aws_subnet.public[1].id : aws_subnet.public[1].id

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_nat_gw"))

  depends_on = [data.aws_internet_gateway.default]
}

resource "aws_nat_gateway" "nat3" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  allocation_id = aws_eip.eip3[0].id
  subnet_id     = length(var.public_custom_subnets) > 0 ? data.aws_subnet.public[2].id : aws_subnet.public[2].id

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_nat_gw"))

  depends_on = [data.aws_internet_gateway.default]
}

resource "aws_route_table" "route1" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc_id = data.aws_vpc.default.id
  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.nat1[0].id
  }

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_route_table"))

}

resource "aws_route_table" "route2" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc_id = data.aws_vpc.default.id
  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.nat2[0].id
  }

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_route_table"))

}

resource "aws_route_table" "route3" {
  count                   = length(var.public_custom_subnets) > 0 ? 0 : 1
  vpc_id = data.aws_vpc.default.id
  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.nat3[0].id
  }

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_route_table"))

}


resource "aws_route_table_association" "nat1" {
  count          = length(var.public_custom_subnets) > 0 ? 0 : 1
  subnet_id      = length(var.private_custom_subnets) > 0 ? data.aws_subnet.default[0].id : aws_subnet.default[0].id
  route_table_id = aws_route_table.route1[0].id
}

resource "aws_route_table_association" "nat2" {
  count          = length(var.public_custom_subnets) > 0 ? 0 : 1
  subnet_id      = length(var.private_custom_subnets) > 0 ? data.aws_subnet.default[1].id : aws_subnet.default[1].id
  route_table_id = aws_route_table.route2[0].id
}

resource "aws_route_table_association" "nat3" {
  count          = length(var.public_custom_subnets) > 0 ? 0 : 1
  subnet_id      = length(var.private_custom_subnets) > 0 ? data.aws_subnet.default[2].id : aws_subnet.default[2].id
  route_table_id = aws_route_table.route3[0].id
}

locals {
  ami = var.aws_ami_id
}

resource "aws_instance" "chef_automate_postgresql" {
  count = var.setup_managed_services ? 0 : var.postgresql_instance_count

  ami                         = local.ami
  instance_type               = var.postgresql_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = length(var.private_custom_subnets) > 0 ? element(data.aws_subnet.default.*.id, count.index) : element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = false
  ebs_optimized               = true

  connection {
    host        = coalesce(self.private_ip)
    port        = var.aws_ssh_port
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
  }

  root_block_device {
    delete_on_termination = var.delete_on_termination
    iops                  = var.postgresql_ebs_volume_type == "io1" ? var.postgresql_ebs_volume_iops : 0
    volume_size           = var.postgresql_ebs_volume_size
    volume_type           = var.postgresql_ebs_volume_type
    tags = merge(var.tags,map("Name",format("${var.tag_name}_${random_id.random.hex}_chef_automate_postgresql_%02d", count.index + 1)))
  }

  tags = merge(var.tags,
    map("Name",
      format(
        "${var.tag_name}_${random_id.random.hex}_chef_automate_postgresql_%02d",
        count.index + 1
      )
    )
  )
  lifecycle {
    ignore_changes = [
      tags,
      tags_all,
      root_block_device
    ]
  }
  metadata_options {
    http_endpoint          = "enabled"
    http_tokens            = "required"
    instance_metadata_tags = "enabled"
  }
  depends_on = [aws_route_table.route1,aws_route_table.route2,aws_route_table.route3]

}
resource "aws_instance" "chef_automate_opensearch" {
  count = var.setup_managed_services ? 0 : var.opensearch_instance_count

  ami                         = local.ami
  instance_type               = var.opensearch_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = length(var.private_custom_subnets) > 0 ? element(data.aws_subnet.default.*.id, count.index) : element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = false //Changes to false as Dashboards are no longer enabled
  ebs_optimized               = true
  iam_instance_profile        = var.aws_instance_profile_name

  root_block_device {
    delete_on_termination = var.delete_on_termination
    iops                  = var.opensearch_ebs_volume_type == "io1" ? var.opensearch_ebs_volume_iops : 0
    volume_size           = var.opensearch_ebs_volume_size
    volume_type           = var.opensearch_ebs_volume_type
    tags = merge(var.tags,map("Name",format("${var.tag_name}_${random_id.random.hex}_chef_automate_opensearch_%02d", count.index + 1)))
  }

  tags = merge(
    var.tags,
    map("Name",
      format("${var.tag_name}_${random_id.random.hex}_chef_automate_opensearch_%02d", count.index + 1)
    )
  )
  lifecycle {
    ignore_changes = [
      tags,
      tags_all,
      root_block_device    ]
  }
  metadata_options {
    http_endpoint          = "enabled"
    http_tokens            = "required"
    instance_metadata_tags = "enabled"
  }
  depends_on = [aws_route_table.route1,aws_route_table.route2,aws_route_table.route3]

}

resource "aws_instance" "chef_automate" {
  count = var.automate_instance_count

  ami                         = local.ami
  instance_type               = var.automate_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = length(var.private_custom_subnets) > 0 ? element(data.aws_subnet.default.*.id, count.index) : element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id, aws_security_group.chef_automate_ui.id]
  associate_public_ip_address = false
  ebs_optimized               = true
  iam_instance_profile        = var.aws_instance_profile_name

  root_block_device {
    delete_on_termination = var.delete_on_termination
    iops                  = var.automate_ebs_volume_type == "io1" ? var.automate_ebs_volume_iops : 0
    volume_size           = var.automate_ebs_volume_size
    volume_type           = var.automate_ebs_volume_type
    tags = merge(var.tags,map("Name",format("${var.tag_name}_${random_id.random.hex}_chef_automate_%02d", count.index + 1)))
  }

  tags = merge(
    var.tags,
    map("Name",
      format("${var.tag_name}_${random_id.random.hex}_chef_automate_%02d", count.index + 1)
    )
  )

  lifecycle {
    ignore_changes = [
      tags,
      tags_all,
      root_block_device
    ]
  }
  metadata_options {
    http_endpoint          = "enabled"
    http_tokens            = "required"
    instance_metadata_tags = "enabled"
  }
  depends_on = [aws_route_table.route1,aws_route_table.route2,aws_route_table.route3]
  
}

resource "aws_instance" "chef_server" {
  count = var.chef_server_instance_count


  ami                         = local.ami
  instance_type               = var.chef_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = length(var.private_custom_subnets) > 0 ? element(data.aws_subnet.default.*.id, count.index) : element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id, aws_security_group.chef_automate_ui.id]
  associate_public_ip_address = false
  ebs_optimized               = true
  iam_instance_profile        = var.aws_instance_profile_name

  root_block_device {
    delete_on_termination = var.delete_on_termination
    iops                  = var.chef_ebs_volume_type == "io1" ? var.chef_ebs_volume_iops : 0
    volume_size           = var.chef_ebs_volume_size
    volume_type           = var.chef_ebs_volume_type
    tags = merge(var.tags,map("Name",format("${var.tag_name}_${random_id.random.hex}_chef_server_%02d", count.index + 1)))
  }

  tags = merge(
    var.tags,
    map("Name",
      format("${var.tag_name}_${random_id.random.hex}_chef_server_%02d", count.index + 1)
    )
  )

  lifecycle {
    ignore_changes = [
      tags,
      tags_all,
      root_block_device
    ]
  }
  metadata_options {
    http_endpoint          = "enabled"
    http_tokens            = "required"
    instance_metadata_tags = "enabled"
  }
  depends_on = [aws_route_table.route1,aws_route_table.route2,aws_route_table.route3]

}
