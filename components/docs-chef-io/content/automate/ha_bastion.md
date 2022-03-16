+++
title = "Bastion Host"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Bastion Host"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_bastion.md Bastion Host"
    weight = 40
+++

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer. All other services are limited to reduce the threat to the computer.

Its purpose is to provide access to a private network from an external network, such as the Internet or outside of a firewall and involves access from untrusted networks or computers. These computers are also equipped with special networking interfaces to withstand high-bandwidth attacks through the internet.

Bastion servers are instances that reside within your public subnet and are accessed using SSH. The purpose of a bastion host is to restrict access to a private network from an external network. Once remote connectivity establishes with the bastion host, it allows you to use SSH to log in to other instances (within private subnets) deeper within your network.

The bastion hosts provide secure access to Linux instances located in the private and public subnets.

## Bastion Host for Chef Automate High Availability (HA)

The Virtual machine is required for either of the Chef Automate HA deployment types to trigger the deployment, which is actually a bastion host. This section explains the bastion host requirements and configurations for the two deployment modes of the Chef Automate HA.

### Download and Install the Chef Automate Utility

Both deployment models require installing and configuring the Chef Automate High Availability (HA) on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.

- Enter the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**.

  The command downloads the Chef Automate utility package in .zip format and installs the utility by providing the execute permission to the Chef Automate file.

The installation of the Chef Automate utility completes, and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}

### Bastion Host Requirements for On-Premise Deployment

#### Prerequisites

- Bastion Server/host IP address
- Instance type: 2 vCPU
- Operating System: Ubuntu 20.04
- Memory: Minimum of 4GB
- Hard Disk Space - 100 GB
- Ports to be publicly accessible: 22 and 9631

### Bastion Host Requirements for AWS (Amazon Web Services) Deployment

#### Prerequisites

- [AWS Credential configured on your bastion host](( {{< relref "#Configuring Bastion for AWS Deployment Type" >}} )).
- Create the certificate for the DNS
- Operating System (OS): Bastion host with Ubuntu 20.04 or centOs-7 or RHEL-7
- AWS instance type: *t2.medium*
- Memory: Minimum of 4GB
- Hard Disk Space - 100 GB
- SSH: VPC to Port 22, publicly accessible
- [Setup Virtual Private Cloud (VPC) in AWS](( {{< relref "ha_common.md#VPC Setup.md" >}}))

#### Configuring Bastion for AWS Deployment Type

Follow these steps to add a bastion host to your Linux environment on AWS cloud, which connects to other internal network hosts:

1. Sign in to your AWS account. If you don't have one, [sign up at amazon](https://aws.amazon.com).
1. Navigate to the **AWS Management Console**.

{{< figure src="/images/automate/ha_aws_console.png" alt="Amazon Management Console">}}

1. Select an **AWS Region** from the top toolbar.
1. Select *EC2* from the **Services** menu on the left.
1. Select the **Launch Instances** button, and perform the following steps:

   - Select *Linux-based Amazon Machine Image (AMI)*. For example, `centos`.

   {{< figure src="/images/automate/ha_aws_ami.png" alt="Amazon Machine Image">}}

   - Select the `t2.medium` instance type. Ensure vCPUs is `1`, Memory (GiB) is `4`, and Instance Storage (GB) is `EBS only`.
   - Select the **Next: Configure Instance Details** button.
   - Modify VPC and Subnet values as required.
   - Ensure you have selected `1` in the **Number of instances** field, and make any required changes.
   - Select **Next: Add Storage** button.
   - Enter `100` GB of storage in the **Size (GiB)** field.
   - Select **Next: Add Tags** button.
   - Enter the key and value for the tag in the **Key** and **Value** fields. *Optional*
   - Select **Next: Configure Security Group** button.
   - Select **Create a new security group** or select an **Existing security group** option.
   - Based on your selection, select needed security groups for your EC2 instance, or add the rule by providing required details. You could also change or update the security groups in the future.
   - Ensure **Type** is `SSH`, **Protocol** is `TCP`, and **Port Range** is `22` to create rules and connections.
   - Open port `9631` by adding TCP rule.

Or launch an EC2 instance, which is available and already defined.

1. Select **Review and Launch** button.
1. Review all the details and click the Launch button. The AWS console prompts you to either create an existing SSH key pair, or use a previously established pair.

   - If you choose to create a new key pair, specify a **Key pair name** and select **Download Key Pair** (private key file, .pem). Store the key file in a secure and accessible location.

   {{< figure src="/images/automate/ha_aws_keypair.png" alt="AWS EC2 Launch Instances">}}

   - Else, select an existing key pair.

1. Select the **Launch Instances**. The AWS console confirms the launch of your host.

{{< figure src="/images/automate/ha_aws_launch_status.png" alt="AWS EC2 Launch Status">}}

You can test the connectivity to the bastion server by navigating to the AWS Console under the **Instances** > **EC2** option and view the new bastion server running.

Refer detailed information on deploying Linux bastion hosts to manage your [AWS Cloud deployments remotely](https://aws.amazon.com/quickstart/architecture/linux-bastion/) page.

#### Configuration of AWS Credential on Bastion Host

You need to setup the AWS credentials on the bastion host to trigger the Amazon Web Services (AWS) deployment.

Follow these steps to do so:

1. Navigate to the AWS console.

1. Select the user profile created and make a note of the access key and secret key of the user.

1. SSH into the bastion host.

1. Create a directory, `.aws` in *root* folder.

1. Type the command, `touch ~/.aws/credentials`.

1. Create a file `credentials` in the /root/.aws directory. For example, `vi credentials`.

1. Add the access key ID and secret key to the *credentials* file:

   - aws_access_key_id=access key id of the IAM user
   - aws_secret_access_key=secret access key of the IAM user.

{{< figure src="/images/automate/ha_aws_credentials.png" alt="AWS Credentials">}}
