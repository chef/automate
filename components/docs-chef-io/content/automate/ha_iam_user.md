+++
title = "IAM Users"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "IAM Users"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_iam_user.md HA IAM Users"
    weight = 220
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

AWS Identity and Access Management (IAM) is a web service that helps you securely control access to AWS resources. You can use IAM to control who is authenticated (signed in) and authorized (has permissions) to use resources.

To run the terraform scripts, you need an IAM user with the following permissions:

- AdministratorAccess
- AmazonAPIGatewayAdministrator
- AmazonS3FullAccess
- IAMFullAccess

These permissions can be directly added to the user or via IAM Group.

{{< note >}}

Keep access key ID and secret access key handy. Refer [access key](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) page for detailed information on regenerating access key ID and secret access key.

{{< /note >}}

Once we have an AWS account, we'll need to create an IAM user to programmatically interact with it and configure the AWS CLI (command-line interface). Amazon IAM enables you to manage users and user permissions in AWS. You can create one or more IAM users in your AWS account. You might create an IAM user for someone who needs access to your AWS console or when you have a new application that needs to make API calls to AWS. This is to add an extra layer of security to your AWS account.

## Creating an IAM User

1.  Navigate to your AWS account.
1.  Select **IAM** from the list of services from the AWS console. The **IAM dashboard** screen appears.
    {{< figure src="/images/automate/ha_aws_iam.png" alt="AWS IAM Dashboard">}}
1.  Select **Users** from the **Access management** menu on the left.
1.  Select **Create Users**. The **Specify user details** screen appears.
1.  Enter the user name for the new user and other necessary details.
    {{< figure src="/images/automate/ha_aws_iam_user.png" alt="AWS IAM User Creation">}}
1.  Check the **Access key - Programmatic access** option under the **Select AWS access type** section.
    This is the sign-in name for AWS. If you want to add multiple users, choose to Add another user for each additional user and specify their user names. You can add up to 10 users at one time.
    This account will be used by your AWS CLI and will be connecting to the AWS API directly by not using the Management Console.
    {{< figure src="/images/automate/ha_aws_iam_paccess.png" alt="AWS IAM User - Programmetic Access">}}
1.  Select **Next: Permissions**.
1.  Select **Attach existing policies directly**.
1.  Filter the policies by keyword, `IAM`. For this user, select `IAMFullAccess` from the list of available policies.\
    The `IAMFullAccess` policy enables this user to create and manage user permissions in AWS.
    {{< figure src="/images/automate/ha_aws_iam_policy.png" alt="AWS IAM User Policy">}}
1.  Set the user permissions.
1.  Search for **AdministratorAccess** and select the policy.
1.  Search for **AmazonAPIGatewayAdministrator** and select the policy.
1.  Search for **AmazonS3FullAccess** and select the policy.
1.  Select **Next: Tags**.
1.  Provide key name and value as tagging for the user been created.
1.  Select **Next: Review**.\
    {{< figure src="/images/automate/ha_aws_iam_user_review.png" alt="AWS IAM User Review with permissions">}}
1.  Select **Create user**.
1.  After user is created go to the **Security Credentials** tab
    {{< figure src="/images/automate/ha_aws_iam_security_cred.png" alt="AWS IAM User - Security Credentials">}}
1.  Select **Create Access key**\
    {{< figure src="/images/automate/ha_aws_iam_create_key.png" alt="AWS IAM User - Create Access Key">}}
1.  Select **other** on the list\
    {{< figure src="/images/automate/ha_aws_iam_key_type.png" alt="AWS IAM User - Access Key Type">}}
1.  Select **show** to reveal the secret access key.
1.  Download and save the **Secret access key**.
    {{< figure src="/images/automate/ha_aws_iam_user_created.png" alt="AWS IAM User Created with Access Key">}}
1.  Take a note of the Access key ID and Secret access key.

Now, let's configure the AWS CLI to deploy our applications from the command line.
Refer [Creating an IAM User](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html) page for creating an IAM user through CLI and API methods.
