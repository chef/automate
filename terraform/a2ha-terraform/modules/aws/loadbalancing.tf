
/////////////////////////
// Automate Load Balancing
resource "aws_alb" "automate_lb" {
  name               = "${var.tag_name}-${random_id.random.hex}-automate-lb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.load_balancer.id]
  subnets            = aws_subnet.public.*.id
  tags               = var.tags
}

resource "aws_alb_target_group" "automate_tg" {
  name                          = "${var.tag_name}-${random_id.random.hex}-automate-tg"
  port                          = 443
  protocol                      = "HTTPS"
  vpc_id                        = data.aws_vpc.default.id
  load_balancing_algorithm_type = "least_outstanding_requests"
}

resource "aws_alb_target_group_attachment" "automate_tg_attachment" {
  count            = var.automate_instance_count
  target_group_arn = aws_alb_target_group.automate_tg.arn
  target_id        = element(aws_instance.chef_automate.*.id, count.index)
  port             = 443
}

resource "aws_alb_listener" "automate_lb_listener_443" {
  load_balancer_arn = aws_alb.automate_lb.arn
  port              = "443"
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-2017-01"
  certificate_arn   = var.automate_lb_certificate_arn

  default_action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.automate_tg.arn
  }
}

resource "aws_alb_listener" "automate_lb_listener_80" {
  load_balancer_arn = aws_alb.automate_lb.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port        = "443"
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

/////////////////////////
// Chef Server
resource "aws_alb" "chef_server_lb" {
  name               = "${var.tag_name}-${random_id.random.hex}-chef-server-lb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.load_balancer.id]
  subnets            = aws_subnet.public.*.id
  tags               = var.tags
}

resource "aws_alb_target_group" "chef_server_tg" {
  name     = "${var.tag_name}-${random_id.random.hex}-chef-server-tg"
  port     = 443
  protocol = "HTTPS"
  vpc_id   = data.aws_vpc.default.id

  health_check {
    path                = "/_status"
    protocol            = "HTTPS"
    healthy_threshold   = 5
    unhealthy_threshold = 2
  }
}

resource "aws_alb_target_group_attachment" "chef_server_tg_attachment" {
  count            = var.chef_server_instance_count
  target_group_arn = aws_alb_target_group.chef_server_tg.arn
  target_id        = element(aws_instance.chef_server.*.id, count.index)
}

resource "aws_alb_listener" "chef_server_lb_listener_443" {
  load_balancer_arn = aws_alb.chef_server_lb.arn
  port              = "443"
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-2017-01"
  certificate_arn   = var.chef_server_lb_certificate_arn

  default_action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.chef_server_tg.arn
  }
}

resource "aws_alb_listener" "chef_server_lb_listener_80" {
  load_balancer_arn = aws_alb.chef_server_lb.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port        = "443"
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}
