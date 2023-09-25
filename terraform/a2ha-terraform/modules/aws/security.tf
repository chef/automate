resource "aws_security_group" "base_linux" {
  name        = "base_linux_${random_id.random.hex}"
  description = "base security rules for all linux nodes"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_linux_security_group"}))
}

resource "aws_security_group" "habitat_supervisor" {
  name        = "habitat_supervisor_${random_id.random.hex}"
  description = "Security rules for the Habitat supervisor"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_habsup_security_group"}))
}

resource "aws_security_group" "chef_automate" {
  name        = "chef_automate_${random_id.random.hex}"
  description = "Chef Automate Server"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_automate_security_group"}))
}

resource "aws_security_group" "chef_automate_ui" {
  name        = "chef_automate_ui_${random_id.random.hex}"
  description = "Chef Automate Server protocol"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_automate_security_group"}))
}

resource "aws_security_group" "efs_mount" {
  name        = "efs_${random_id.random.hex}"
  description = "NFS for EFS"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_efs_security_group"}))
}

resource "aws_security_group" "load_balancer" {
  name        = "load_balancer_${random_id.random.hex}"
  description = "load_balancer rules for all linux nodes"
  vpc_id      = data.aws_vpc.default.id

  tags = merge(var.tags, tomap({"Name" = "${var.tag_name}_${random_id.random.hex}_linux_security_group"}))
}

//////////////////////////
// Base Linux Rules
resource "aws_security_group_rule" "ingress_allow_22_tcp_all" {
  type              = "ingress"
  from_port         = 22
  to_port           = 22
  protocol          = "tcp"
  cidr_blocks       = [var.json_data]
  security_group_id = aws_security_group.base_linux.id
}

/////////////////////////
// Habitat Supervisor Rules
# Allow Habitat Supervisor http communication tcp
resource "aws_security_group_rule" "ingress_allow_9631_tcp" {
  type                     = "ingress"
  from_port                = 9631
  to_port                  = 9631
  protocol                 = "tcp"
  security_group_id        = aws_security_group.habitat_supervisor.id
  source_security_group_id = aws_security_group.habitat_supervisor.id
}

# Allow Habitat Supervisor http communication udp
resource "aws_security_group_rule" "ingress_allow_9631_udp" {
  type                     = "ingress"
  from_port                = 9631
  to_port                  = 9631
  protocol                 = "udp"
  security_group_id        = aws_security_group.habitat_supervisor.id
  source_security_group_id = aws_security_group.habitat_supervisor.id
}

# Allow Habitat Supervisor ZeroMQ communication tcp
resource "aws_security_group_rule" "ingress_9638_tcp" {
  type                     = "ingress"
  from_port                = 9638
  to_port                  = 9638
  protocol                 = "tcp"
  security_group_id        = aws_security_group.habitat_supervisor.id
  source_security_group_id = aws_security_group.habitat_supervisor.id
}

# Allow Habitat Supervisor ZeroMQ communication udp
resource "aws_security_group_rule" "ingress_allow_9638_udp" {
  type                     = "ingress"
  from_port                = 9638
  to_port                  = 9638
  protocol                 = "udp"
  security_group_id        = aws_security_group.habitat_supervisor.id
  source_security_group_id = aws_security_group.habitat_supervisor.id
}

////////////////////////////////
// Chef Automate Rules
# HTTP (nginx)
resource "aws_security_group_rule" "ingress_chef_automate_allow_80_tcp" {
  type              = "ingress"
  from_port         = 80
  to_port           = 80
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.chef_automate_ui.id
}

# HTTPS (nginx)
resource "aws_security_group_rule" "ingress_chef_automate_allow_443_tcp" {
  type              = "ingress"
  from_port         = 443
  to_port           = 443
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.chef_automate_ui.id
}

# Allow Verify service
resource "aws_security_group_rule" "ingress_chef_automate_allow_7799_tcp" {
  type              = "ingress"
  from_port         = 7799
  to_port           = 7799
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.chef_automate.id
}

# Allow elasticsearch clients
resource "aws_security_group_rule" "ingress_chef_automate_allow_elasticsearch_tcp" {
  type                     = "ingress"
  from_port                = var.opensearch_listen_port
  to_port                  = var.opensearch_listen_port + 200
  protocol                 = "tcp"
  security_group_id        = aws_security_group.chef_automate.id
  source_security_group_id = aws_security_group.chef_automate.id
}

# Allow postgresql connections
resource "aws_security_group_rule" "ingress_chef_automate_allow_postgres_tcp" {
  type                     = "ingress"
  from_port                = var.postgresql_listen_port
  to_port                  = var.postgresql_listen_port
  protocol                 = "tcp"
  security_group_id        = aws_security_group.chef_automate.id
  source_security_group_id = aws_security_group.chef_automate.id
}

# Allow pgleaderchk connections
resource "aws_security_group_rule" "ingress_chef_automate_allow_pgleaderchk_tcp" {
  type                     = "ingress"
  from_port                = var.pgleaderchk_listen_port
  to_port                  = var.pgleaderchk_listen_port
  protocol                 = "tcp"
  security_group_id        = aws_security_group.chef_automate.id
  source_security_group_id = aws_security_group.chef_automate.id
}

# Allow proxy connections
resource "aws_security_group_rule" "ingress_chef_automate_allow_proxy_tcp" {
  type                     = "ingress"
  from_port                = var.proxy_listen_port
  to_port                  = var.proxy_listen_port
  protocol                 = "tcp"
  security_group_id        = aws_security_group.chef_automate.id
  source_security_group_id = aws_security_group.chef_automate.id
}

# Allow custom SSH connections
resource "aws_security_group_rule" "ingress_chef_automate_allow_custom_ssh" {
  type                     = "ingress"
  from_port                = var.aws_ssh_port
  to_port                  = var.aws_ssh_port
  protocol                 = "tcp"
  cidr_blocks              = ["0.0.0.0/0"]
  security_group_id        = aws_security_group.chef_automate.id
}

//////////////////////////
//  Load balancer security group Rules
resource "aws_security_group_rule" "ingress_allow_80_tcp_all" {
  type              = "ingress"
  from_port         = 80
  to_port           = 80
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.load_balancer.id
}

resource "aws_security_group_rule" "ingress_allow_443_tcp_all" {
  type              = "ingress"
  from_port         = 443
  to_port           = 443
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.load_balancer.id
}

resource "aws_security_group_rule" "load_balancer_egress_allow_0-65535_all" {
  type              = "egress"
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.load_balancer.id
}

# Egress : base_linux security group
resource "aws_security_group_rule" "linux_egress_allow_0-65535_all" {
  type              = "egress"
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_80_tcp_all" {
  type              = "egress"
  from_port         = 80
  to_port           = 80
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.chef_automate_ui.id
}

resource "aws_security_group_rule" "egress_allow_443_tcp_all" {
  type              = "egress"
  from_port         = 443
  to_port           = 443
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.chef_automate_ui.id
}

resource "aws_security_group_rule" "egress_allow_6432_tcp_all" {
  type              = "egress"
  from_port         = 6432
  to_port           = 6432
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_9200_tcp_all" {
  type              = "egress"
  from_port         = 9200
  to_port           = 9200
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_9631_tcp_all" {
  type              = "egress"
  from_port         = 9631
  to_port           = 9631
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_9300_tcp_all" {
  type              = "egress"
  from_port         = 9300
  to_port           = 9300
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_9638_tcp_all" {
  type              = "egress"
  from_port         = 9638
  to_port           = 9638
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_9638_udp_all" {
  type              = "egress"
  from_port         = 9638
  to_port           = 9638
  protocol          = "udp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_7432_tcp_all" {
  type              = "egress"
  from_port         = 7432
  to_port           = 7432
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_5432_tcp_all" {
  type              = "egress"
  from_port         = 5432
  to_port           = 5432
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_efs_nfs_2049" {
  type                     = "egress"
  from_port                = 2049
  to_port                  = 2049
  protocol                 = "tcp"
  cidr_blocks              = ["0.0.0.0/0"]
  security_group_id        = aws_security_group.base_linux.id
}

resource "aws_security_group_rule" "egress_allow_22_tcp_all" {
  type              = "egress"
  from_port         = 22
  to_port           = 22
  protocol          = "tcp"
  cidr_blocks       = [var.json_data]
  security_group_id = aws_security_group.base_linux.id
}
//////////////////////////
// EFS rules
resource "aws_security_group_rule" "ingress_efs_nfs_2049" {
  type                     = "ingress"
  from_port                = 2049
  to_port                  = 2049
  protocol                 = "tcp"
  security_group_id        = aws_security_group.efs_mount.id
  source_security_group_id = aws_security_group.base_linux.id
}
