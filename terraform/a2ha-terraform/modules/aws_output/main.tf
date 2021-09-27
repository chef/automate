locals {
  output_tfvars = templatefile("${path.module}/files/output.tfvars.tpl",{
      automate_public_ips       = join(", ", formatlist("\"%s\"", var.automate_public_ips)),
      automate_private_ips      = join(", ", formatlist("\"%s\"", var.automate_private_ips)),
      chef_server_public_ips    = join(", ", formatlist("\"%s\"", var.chef_server_public_ips)),
      chef_server_private_ips   = join(", ", formatlist("\"%s\"", var.chef_server_private_ips)),
      elasticsearch_public_ips  = join(", ", formatlist("\"%s\"", var.elasticsearch_public_ips)),
      elasticsearch_private_ips = join(", ", formatlist("\"%s\"", var.elasticsearch_private_ips)),
      postgresql_private_ips    = join(", ", formatlist("\"%s\"", var.postgresql_private_ips)),
      postgresql_public_ips     = join(", ", formatlist("\"%s\"", var.postgresql_public_ips)),
      automate_fqdn             = var.automate_fqdn
      automate_frontend_urls    = var.automate_frontend_urls
    })
}

#This will create auto.tfvars with using aws's content. Because dusing deployment time we will need some aws info like public and private ips.
resource "local_file" "output" {
    filename = "${path.root}/reference_architectures/deployment/output.auto.tfvars"
    content  = local.output_tfvars
}

#This will convert tfvars to auto.tfvars because after the provisioning cluter, deployment will be started so at that time we will need current tfvars file. So if we convert it into auto.tfvars then we don't need to load this file. It will be automatically loaded.
#For starting deployment process after provisioning, we have to set deployment flag in tfvars and tf_arch file.
resource "null_resource" "output" {
    count = 1
    
    provisioner "local-exec" {
        command = "mv ${path.root}/terraform.tfvars ${path.root}/aws.auto.tfvars;sed  -i 's/aws/deployment/' ${path.root}/.tf_arch;sed  -i 's/architecture \"aws\"/architecture \"deployment\"/' ${path.root}/../a2ha.rb"
    }

    depends_on = [local_file.output]
}

#This resource will execute bash script to set up the destroy directory. Because we are maintaining single parent dir for terraform apply for provision and deployment. So just after provison, deploy command will be executed and for deployment diff terraform script will be executed. So we have to maintain destroy directory for aws resource deletion in future if needed.
resource "null_resource" "bash" {
    count = 1
    
    provisioner "local-exec" {
        command = "cp /hab/a2_deploy_workspace/terraform/reference_architectures/aws/variables.tf /hab/a2_deploy_workspace/terraform/destroy/aws;cp /hab/a2_deploy_workspace/terraform/a2ha_habitat.auto.tfvars /hab/a2_deploy_workspace/terraform/destroy/aws/;cp /hab/a2_deploy_workspace/terraform/aws.auto.tfvars /hab/a2_deploy_workspace/terraform/destroy/aws;cp /hab/a2_deploy_workspace/terraform/variables_common.tf /hab/a2_deploy_workspace/terraform/destroy/aws"
    }

    depends_on = [null_resource.output]
}