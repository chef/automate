locals {
  output_tfvars = templatefile("${path.module}/files/output.tfvars.tpl",{
      automate_private_ips      = join(", ", formatlist("\"%s\"", var.automate_private_ips)),
      chef_server_private_ips   = join(", ", formatlist("\"%s\"", var.chef_server_private_ips)),
      elasticsearch_public_ips  = join(", ", formatlist("\"%s\"", var.elasticsearch_public_ips)),
      elasticsearch_private_ips = join(", ", formatlist("\"%s\"", var.elasticsearch_private_ips)),
      postgresql_private_ips    = join(", ", formatlist("\"%s\"", var.postgresql_private_ips)),
      automate_fqdn             = var.automate_fqdn
      automate_frontend_urls    = var.automate_frontend_urls
      bucket_name               = var.bucket_name
    })

    copy_terraform_files_for_destroy = [
    "/hab/a2_deploy_workspace/terraform/reference_architectures/aws/variables.tf",
    "/hab/a2_deploy_workspace/terraform/a2ha_habitat.auto.tfvars",
    "/hab/a2_deploy_workspace/terraform/aws.auto.tfvars",
    "/hab/a2_deploy_workspace/terraform/variables_common.tf"
    ]

    destination_path = "/hab/a2_deploy_workspace/terraform/destroy/aws"
 
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

resource "null_resource" "setup_destroy_directory" {
  for_each = toset(local.copy_terraform_files_for_destroy)

  provisioner "local-exec" {
    command = "cp ${each.value} ${local.destination_path}"
  }

  depends_on = [null_resource.output]

}
