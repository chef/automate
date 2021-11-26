# HOW TO DESTROY INFRA

# SCENARIO 1

If you are running chef-automate provision-infra and in between anything occure and provision get stopped then run below command to destroy that half part to start provision again.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate;cd $i;done`

# SCENARIO 2

If you have successfully done provision-infra and after that if you want to destroy infra then run below command in below order.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`
 
`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done` 

# SCENARIO 3 

If you've completed `chef-automate deploy` and want to destroy deploy part then run below command. Remember destroy of deployment will not remove any kind of configuration from remote server but it will do terraform taint so that next time all the configuration things will be started again.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done`

# SCENARIO 4

If you have finished deployment and want destroy all the instances of whole infra then run below commnd.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`
 
`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done` 
