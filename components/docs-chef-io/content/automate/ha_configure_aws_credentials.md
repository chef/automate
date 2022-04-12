+++
title = "Configuration of AWS Credential on Bastion Host"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuration of AWS Credential on Bastion Host"
    parent = "automate/deploy_high_availability/aws_deployment"
    identifier = "automate/deploy_high_availability/aws_deployment/ha_configure_aws_credentials.md Configuration of AWS Credential on Bastion Host"
    weight = 220
+++

You need to set up the AWS credentials on the bastion host to trigger the Amazon Web Services (AWS) deployment.

Follow these steps to do so:

1. Navigate to the **AWS console**.

1. Select the user profile created and note the user's access key and secret key.

1. SSH into the bastion host.

1. Create a directory, `.aws` in the *root* folder.

1. Execute the command `touch ~/.aws/credentials`.

1. Create a file `credentials` in the /root/.aws directory. For example, `vi credentials`.

1. Add the access key ID and secret key to the *credentials* file:

   - aws_access_key_id=access key id of the IAM user
   - aws_secret_access_key=secret access key of the IAM user.

{{< figure src="/images/automate/ha_aws_credentials.png" alt="AWS Credentials">}}
