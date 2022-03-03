+++
title = "Destroying Chef Automate HA Infrastructure"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Destroying Chef Automate HA Infrastructure"
    parent = "automate/install"
    identifier = "automate/install/ha_destroy.md Destroying Chef Automate HA Infrastructure"
    weight = 370
+++

Follow any of the step to destroy terraform or infrastructure created while deploying Chef Automate High Availability (HA) using AWS or Bare-metal model.

- If the *provision-infra* command fails, execute the following command to clear the space utilized by this command before it fails, execute the command:

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate;cd $i;done`

- If the *provision-infra* command ran successfully and if you want to clear the space utilized by this command, execute these commands in the order listed:

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.

- If you have deployed the Chef Automate HA successffuly and wanted to destroy the deployment part alone, execute the command:

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done`

{{< note >}}

The deployment destroy does not remove any remote server configuration made, however it taints the terraform and thus you need to redo the configurations.

{{ < /note >}}

- If you have deployed the Chef Automate HA successfully and wanted to destroy the entire infrastructure instances, execute these commands:

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.
