+++
title = "Prerequisites for Chef Automate HA Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Prerequisites for Chef Automate HA Deployment"
    parent = "automate/install"
    identifier = "automate/install/ha_aws_prereq.md Prerequisites for Chef Automate HA Deployment"
    weight = 220
+++

## Pre-requisites

### Download and Install Chef Automate Utility

Both types of deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**. The command downloads the Chef Automate utility installer in .zip format.
- Type the  command,  and press **Enter**. The command installs the utility and provides the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}

### Building an AWS bastion host

Follow these steps to add a bastion host to your Linux environment on AWS cloud, which connects to other internal network hosts:

1. Sign into your AWS account. If you don't have one, sign up at https://aws.amazon.com.
1. Navigate to the AWS Management Console.

{{< figure src="/images/automate/ha_aws_console.png" alt="Amazon Management Console">}}

1. Select an AWS Region from the top toolbar.
1. Select EC2 under Services menu on the left.
1. Click the Launch Instances button, and perform the following steps:
   1. Select linux based Amazon Machine Image (AMI). For example, `centos`.

   {{< figure src="/images/automate/ha_aws_ami.png" alt="Amazon Machine Image">}}

   1. Select the *t2.medium* instance type. Ensure vCPUs is 1, Memory (GiB) is 4 and Instance Storage (GB) is EBS only.
   1. Click the Next: Configure Instance Details button.
   1. Modify VPC and Subnet values as required.
   1. Ensure you have selected 1 in Number of instances field, and make any other required changes.
   1. Click Next: Add Storage button.
   1. Specify 100 GB of storage in Size (GiB) field.
   1. Click Next: Add Tags button.
   1. Specify the key and value for the tag in Key and Value fields. This step is optional.
   1. Click Next: Configure Security Group button.
   1. Select Create a new security group or Select an existing security group option.
   1. Based on your selection, select needed security groups for your EC2 instance, or add the rule by providing required details. You could change or update the security groups in the future if you want.
   1. Ensure Type is SSH, Protocol is TCP, and Port Range is 22 to create rules and connections.
   <!-- u must have private key -->
   1. Open port 9631 by adding TCP rule.

Or, launch an EC2 instance, which was previously defined.

1. Click Review and Launch button.
1. Review all the details and click the Launch button. The AWS console prompts you to either create an existing SSH key pair, or use a pair you have previously established.
   - If you choose to create a new key pair, specify a Key pair name and click Download Key Pair (private key file, .pem). Store the key file in a secure and accessible location.

    {{< figure src="/images/automate/ha_aws_keypair.png" alt="AWS EC2 Launch Instances">}}

   - Else, select an existing key pair.
1. Click Launch Instances. The AWS console confirms the launch of your host.

 {{< figure src="/images/automate/ha_aws_launch_status.png" alt="AWS EC2 Launch Status">}}

You can test the connectivity to the bastion server by navigating to the AWS Console under Instances > EC2 option and view the fresh bastion server running.

For detailed information to deploy Linux bastion hosts to manage your AWS Cloud deployments remotely, see https://aws.amazon.com/quickstart/architecture/linux-bastion/.
