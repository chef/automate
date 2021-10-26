# HOW TO DESTROY INFRA

# SCENARIO 1

If you are running chef-automate provision-infra and in between anything occure and provision get stopped then run below command to destroy that half part to start provision again.

`terraform -chdir="/hab/a2_deploy_workspace/terraform/" destroy -state="/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate"`


terraform -chdir="./hab/a2_deploy_workspace/terraform/destroy/aws/" destroy
terraform -chdir="./hab/a2_deploy_workspace/terraform/destroy/aws/" init

terraform -chdir="/hab/a2_deploy_workspace/terraform/" destroy -state="/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate"


# SCENARIO 2

If you have successfully done provision-infra and after that if you want to destroy infra then run below command in below order.

`terraform -chdir="/hab/a2_deploy_workspace/terraform/destroy/aws/" init`
 
`terraform -chdir="/hab/a2_deploy_workspace/terraform/destroy/aws/" destroy` 

# SCENARIO 3 

If you've completed `chef-automate deploy` and want to destroy deploy part then run below command. Remember destroy of deployment willnot remove any kind of configuration from remote server but it will do terraform taint so that next time all the configuration things will start again.

`terraform -chdir="/hab/a2_deploy_workspace/terraform/" destroy`

# SCENARIO 4

If you have finished deployment and want destroy all the instances f whole infra then run below commnd.

`terraform -chdir="/hab/a2_deploy_workspace/terraform/destroy/aws/" init`
 
`terraform -chdir="/hab/a2_deploy_workspace/terraform/destroy/aws/" destroy`
