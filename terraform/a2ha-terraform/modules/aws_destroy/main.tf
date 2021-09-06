#This resource will execute bash script to set up the destroy directory. Because we are maintaining single parent dir for terraform apply for provision and deployment. So just after provison, deploy command will be executed and for deployment diff terraform script will be executed. So we have to maintain destroy directory for aws resource deletion in future if needed.
resource "null_resource" "bash" {
    count = 1
    
    provisioner "local-exec" {
        command = "cp ${path.root}/reference_architectures/aws/variables.tf ${path.root}/destroy/aws;cp ${path.root}/a2ha_habitat.auto.tfvars ${path.root}/destroy/aws/;cp ${path.root}/aws.auto.tfvars ${path.root}/destroy/aws;cp ${path.root}/variables_common.tf ${path.root}/destroy/aws;cp -r ${path.root}/.terraform/plugins/ ${path.root}/destroy/aws/.terraform/"
    }
}
