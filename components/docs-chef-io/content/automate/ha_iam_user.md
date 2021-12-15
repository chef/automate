+++
title = "HA IAM User"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA IAM User"
    parent = "automate/install"
    identifier = "automate/install/ha_iam_user.md HA IAM User"
    weight = 270
+++

### IAM User

Note: you will by default login as the ubuntu user, but can switch to root access, if needed, using the sudo command.

In order to run the terraform scripts, you need an IAM user with following permissions:

- AdministratorAccess
- AmazonAPIGatewayAdministrator
- AmazonS3FullAccess

- full access is required for s3 backup


These permissions can either be directly added to the user or can be added via IAM Group. Ensure you have the access key id and secret access key for the user. Else, regenerate a new access key and keep it handy.

Once we have an AWS account, we’ll need to create an IAM (Identity and Access Management) user to programmatically interact with it and to configure our AWS CLI (command-line interface).

Amazon IAM enables you to manage users and user permissions in AWS. You can create one or more IAM users in your AWS account. You might create an IAM user for someone who needs access to your AWS console, or when you have a new application that needs to make API calls to AWS. This is to add an extra layer of security to your AWS account.

AWS offers Identity and Access Management (IAM) so that its customers can control who or what can see and use their services and resources. AWS admins can create users and groups and grant them access to resources through policies. Policy-based access control enables granular allow/deny decisions that come from a centralized control point.

Create User

First, log in to your AWS Console and select IAM from the list of services.
Select Users.
Select Add User.
Enter a User name and check Programmatic access, then select Next: Permissions.
This account will be used by our AWS CLI and Serverless Stack Framework (SST). They’ll be connecting to the AWS API directly and will not be using the Management Console.
Select Attach existing policies directly.
Set the user permissions. Click Attach existing policies directly and then filter the policies by keyword: IAM. For this user, select IAMFullAccess from the list of available policies. The IAMFullAccess policy enables this user to create and manage user permissions in AWS.
Search for AdministratorAccess and select the policy, then select Next: Tags.
Click Next: Review.
Select Create user.
Select Show to reveal Secret access key.
Take a note of the Access key ID and Secret access key. 
Now let’s configure our AWS CLI so we can deploy our applications from our command line.

Refer: https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html
