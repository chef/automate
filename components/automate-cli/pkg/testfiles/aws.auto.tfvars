# AWS
################################################################################
aws_profile = "default"
aws_region  = "us-west-1"
aws_vpc_id  = "vpc-6fafdtj09"
aws_cidr_block_addr  = "172.31.0.0"
private_custom_subnets = []
public_custom_subnets  = []
aws_tags    = {
        X-Contact = ""
	X-Dept = ""
	X-Project = ""
	X-Customer = ""
	X-Production = ""
}

aws_ssh_key_pair_name = "abc"


 backup_config_s3 = "true"

aws_s3_bucketName = "bucket"

setup_managed_services = false
managed_opensearch_domain_name = " "
managed_opensearch_domain_url = " "
managed_opensearch_username = " "
managed_opensearch_user_password = " "
managed_opensearch_certificate = <<-EOT

EOT
aws_os_snapshot_role_arn = " "
os_snapshot_user_access_key_id = " "
os_snapshot_user_access_key_secret = " "
managed_rds_instance_url = " "
managed_rds_superuser_username = " "
managed_rds_superuser_password = " "
managed_rds_dbuser_username = " "
managed_rds_dbuser_password = " "
managed_rds_certificate = <<-EOT

EOT
aws_ami_id = "ami-061534684251"
lb_access_logs = "false"
automate_lb_certificate_arn = "arn:aws:acm:us-west-1:11275563:certificate/1a0995"
chef_server_lb_certificate_arn = "arn:aws:acm:us-west-1:112563:certificate/1a0a3d6995"

delete_on_termination = "true"

automate_server_instance_type = "t3.medium"

automate_ebs_volume_iops = "100"

automate_ebs_volume_size = "50"

automate_ebs_volume_type = "gp3"

chef_server_instance_type = "t3.medium"

chef_ebs_volume_size = "50"

chef_ebs_volume_type = "gp3"

opensearch_server_instance_type = "m5.large"

opensearch_ebs_volume_iops = "100"

opensearch_ebs_volume_size = "50"

opensearch_ebs_volume_type = "gp3"

postgresql_server_instance_type = "t3.medium"

postgresql_ebs_volume_iops = "100"

postgresql_ebs_volume_size = "50"

postgresql_ebs_volume_type = "gp3"




# Common
################################################################################
automate_config_file = "/hab/a2_deploy_workspace/configs/automate.toml"
automate_instance_count = 1
chef_server_instance_count = 1
opensearch_instance_count = 3

automate_root_ca = <<-EOT

EOT

opensearch_root_ca = <<-EOT

EOT

postgresql_root_ca = <<-EOT

EOT

automate_private_key = <<-EOT

EOT

chef_server_private_key = <<-EOT

EOT

opensearch_admin_key = <<-EOT

EOT

opensearch_private_key = <<-EOT

EOT

postgresql_private_key = <<-EOT

EOT

automate_public_key = <<-EOT

EOT

chef_server_public_key = <<-EOT

EOT

opensearch_public_key = <<-EOT

EOT

opensearch_admin_cert = <<-EOT

EOT

postgresql_public_key = <<-EOT

EOT

automate_custom_certs_enabled = false
chef_server_custom_certs_enabled = false
postgresql_custom_certs_enabled = false
opensearch_custom_certs_enabled = false

opensearch_admin_dn = ""
opensearch_nodes_dn = ""

automate_certs_by_ip = {}
chef_server_certs_by_ip = {}
postgresql_certs_by_ip = {}
opensearch_certs_by_ip = {}

nfs_mount_path = "/mnt/automate_backups"
postgresql_instance_count = 3
postgresql_archive_disk_fs_path = "/mnt/automate_backups/postgresql"

habitat_uid_gid = ""
ssh_user = "ubuntu"
ssh_port = "22"
ssh_key_file = "/home/ubuntu/abc.pem"

tag_project = ""