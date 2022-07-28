# HOW TO DESTROY INFRA

# SCENARIO 1

If you are running chef-automate provision-infra and in between anything occure and provision get stopped then run below command to destroy that half part to start provision again.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate;cd $i;done`

# SCENARIO 2

If you have successfully done provision-infra and after that if you want to destroy infra then run below command in below order.

## SCENARIO 2.1 - S3 Bucket

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;cp -r ../../.tf_arch .;cp -r ../../../a2ha.rb ..;terraform apply -var="destroy_bucket=true";cd $i;done`(This line will destroy storage bucket while clean up. Use this only if you want remove storage bucket)

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`

## SCENARIO 2.2 - EFS Bucket

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done`

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`

# SCENARIO 3

If you've completed `chef-automate deploy` and want to destroy deploy part then run below command. Remember destroy of deployment will not remove any kind of configuration from remote server but it will do terraform taint so that next time all the configuration things will be started again.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done`

# SCENARIO 4

If you have finished deployment and want destroy all the instances of whole infra then run below commnd.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;cp -r ../../.tf_arch .;cp -r ../../../a2ha.rb ..;terraform apply -var="destroy_bucket=true";cd $i;done`(This line will destroy storage bucket while clean up. Use this only if you want remove storage bucket)

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`
