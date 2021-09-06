provider "aws" {
  region                  = var.aws_region
  profile                 = var.aws_profile
  shared_credentials_file = "~/.aws/credentials"
}

resource "random_id" "random" {
  byte_length = 4
}

data "aws_ami" "image" {
  most_recent = true

  filter {
    name   = "name"
    values = [var.ami_filter_name]
  }

  filter {
    name   = "virtualization-type"
    values = [var.ami_filter_virt_type]
  }

  owners = [var.ami_filter_owner]
}

data "aws_availability_zones" "available" {
}

resource "aws_vpc" "default" {
  cidr_block = "10.1.0.0/16"

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_vpc"))

  enable_classiclink_dns_support = false
  enable_dns_hostnames           = true
}

resource "aws_internet_gateway" "default" {
  vpc_id = aws_vpc.default.id
}

resource "aws_route" "internet_access" {
  route_table_id         = aws_vpc.default.main_route_table_id
  destination_cidr_block = "0.0.0.0/0"
  gateway_id             = aws_internet_gateway.default.id
}

resource "aws_subnet" "default" {
  count                   = 3
  vpc_id                  = aws_vpc.default.id
  cidr_block              = cidrsubnet(aws_vpc.default.cidr_block, 8, count.index)
  availability_zone       = data.aws_availability_zones.available.names[count.index]
  map_public_ip_on_launch = true

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_${data.aws_availability_zones.available.names[count.index]}"))
}

resource "aws_efs_file_system" "backups" {
  creation_token = "${var.tag_name}_${random_id.random.hex}_efsfs"

  tags = merge(var.tags, map("Name", "${var.tag_name}_${random_id.random.hex}_efsfs"))
}

resource "aws_efs_mount_target" "backups" {
  count           = 3
  file_system_id  = aws_efs_file_system.backups.id
  subnet_id       = element(aws_subnet.default.*.id, count.index)
  security_groups = [aws_security_group.efs_mount.id]
}

locals {
  mount_nfs = templatefile("${path.module}/mount_nfs.tpl", {
    efs_mount_dns = aws_efs_file_system.backups.dns_name,
    efs_region    = var.aws_region,
    mount_path    = var.nfs_mount_path
  })

  ami = length(var.aws_ami_id) > 0 ? var.aws_ami_id : data.aws_ami.image.id
}

resource "aws_instance" "chef_automate_postgresql" {
  count = var.postgresql_instance_count

  ami                         = local.ami
  instance_type               = var.postgresql_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = true
  ebs_optimized               = true

  connection {
    host        = coalesce(self.public_ip, self.private_ip)
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
  }

  root_block_device {
    delete_on_termination = true
    iops                  = var.postgresql_ebs_volume_type == "io1" ? var.postgresql_ebs_volume_iops : 0
    volume_size           = var.postgresql_ebs_volume_size
    volume_type           = var.postgresql_ebs_volume_type
  }

  tags = merge(var.tags,
               map("Name",
                 format(
                   "${var.tag_name}_${random_id.random.hex}_chef_automate_postgresql_%02d",
                   count.index + 1
                 )
               )
         )

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }

  depends_on = [aws_efs_mount_target.backups]
}

resource "aws_instance" "chef_automate_elasticsearch" {
  count = var.elasticsearch_instance_count

  ami                         = local.ami
  instance_type               = var.elasticsearch_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = true
  ebs_optimized               = true

  connection {
    host        = coalesce(self.public_ip, self.private_ip)
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
  }

  root_block_device {
    delete_on_termination = true
    iops                  = var.elasticsearch_ebs_volume_type == "io1" ? var.elasticsearch_ebs_volume_iops : 0
    volume_size           = var.elasticsearch_ebs_volume_size
    volume_type           = var.elasticsearch_ebs_volume_type
  }

  tags  = merge(
            var.tags,
            map("Name",
                format("${var.tag_name}_${random_id.random.hex}_chef_automate_elasticsearch_%02d", count.index +1)
            )
          )

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }

  depends_on = [aws_efs_mount_target.backups]
}

resource "aws_instance" "chef_automate" {
  count = var.automate_instance_count

  connection {
    host        = coalesce(self.public_ip, self.private_ip)
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
  }

  ami                         = local.ami
  instance_type               = var.automate_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = true
  ebs_optimized               = true

  root_block_device {
    delete_on_termination = true
    iops                  = var.automate_ebs_volume_type == "io1" ? var.automate_ebs_volume_iops : 0
    volume_size           = var.automate_ebs_volume_size
    volume_type           = var.automate_ebs_volume_type
  }

  tags  = merge(
            var.tags,
            map("Name",
                format("${var.tag_name}_${random_id.random.hex}_chef_automate_%02d", count.index +1)
            )
          )

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }

  depends_on = [aws_efs_mount_target.backups]
}

resource "aws_instance" "chef_server" {
  count = var.chef_server_instance_count

  connection {
    host        = coalesce(self.public_ip, self.private_ip)
    type        = "ssh"
    user        = var.aws_ssh_user
    private_key = file(var.aws_ssh_key_file)
    script_path = "${var.tmp_path}/tf_inline_script_aws.sh"
  }

  ami                         = local.ami
  instance_type               = var.chef_server_instance_type
  key_name                    = var.aws_ssh_key_pair_name
  subnet_id                   = element(aws_subnet.default.*.id, count.index)
  vpc_security_group_ids      = [aws_security_group.base_linux.id, aws_security_group.habitat_supervisor.id, aws_security_group.chef_automate.id]
  associate_public_ip_address = true
  ebs_optimized               = true

  root_block_device {
    delete_on_termination = true
    iops                  = var.chef_ebs_volume_type == "io1" ? var.chef_ebs_volume_iops : 0
    volume_size           = var.chef_ebs_volume_size
    volume_type           = var.chef_ebs_volume_type
  }

  tags  = merge(
            var.tags,
            map("Name",
                format("${var.tag_name}_${random_id.random.hex}_chef_server_%02d", count.index +1)
            )
          )

  provisioner "file" {
    content     = local.mount_nfs
    destination = "${var.tmp_path}/mount_nfs"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.ssh_user_sudo_password}' | ${var.sudo_cmd} -S bash -ex ${var.tmp_path}/mount_nfs",
    ]
  }

  depends_on = [aws_efs_mount_target.backups]
}
