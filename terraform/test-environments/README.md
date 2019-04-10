# Automate 2.X - Terraform

This directory contains all the Terraform code required to spin up the chef-server.chef.co environment.

## Installation Scenarios

In the `scenarios` folder you will find the terraform code for standing up all our various scenarios. A short description about each of the files is available within that file.

### vars Folder

There is a `tfvars` file in the vars folder for each Installation Channel that we currently support. The values in those channels inform the Terraform scripts where to stand up the infrastructure in the `chef-cd` AWS environment, and what Habitat Channel to use to install Automate 2.X.

## Adding a new Installation Scenario to Acceptance

1. Duplicate an existing scenario that is similar to the one you desire. For example, if you wanted to add a
   `omnibus-standalone-upgrade-from-current`, you could start with the `omnibus-standalone-inplace-upgrade`
   scenario file.
2. Decide if you want this scenario to be "long lived" (i.e. should the environment continue running after the
   tests have been run against it?). If this scenario can be reaped at the end of the Functional phase, add the
   automate server and job runner module definitions to the `cleanup` task in the Makefile.
3. Update the `outputs.tf` to export the FQDN for the new automate server.

## Standing up a development "acceptance" environment (for Chef Software employees only)

You can use this environment to test changes to the Terraform code and the chef-server-deploy cookbook.

1. Download and configure the [okta_aws utility](https://github.com/chef/okta_aws) - you'll use this to get credentials to the appropriate AWS account.
2. Run `okta_aws chef-cd` to authenticate against the chef-cd AWS account.
3. Allocate a `TF_ENVIRONMENT_INDEX` for yourself - reach out in the #acc-support channel to determine which number you should use.
3. Run `make plan` in the `terraform/` directory to ensure that the plan will execute accordingly.
4. Run `make apply` to stand up your environment.
5. When complete, run `make destroy` to tear down your environment. **There is no automatic reaping of these environments.**
