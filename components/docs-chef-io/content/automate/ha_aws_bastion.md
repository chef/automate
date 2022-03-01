+++
title = "Configuring Bastion for AWS Deployment Type"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuring Bastion for AWS Deployment Type"
    parent = "automate/install"
    identifier = "automate/install/ha_aws_bastion.md Configuring Bastion for AWS Deployment Type"
    weight = 300
+++

Follow these steps to add a bastion host to your Linux environment on AWS cloud, which connects to other internal network hosts:

1. Sign into your AWS account. If you don't have one, [sign up at amazon](https://aws.amazon.com).
1. Navigate to the AWS Management Console.

{{< figure src="/images/automate/ha_aws_console.png" alt="Amazon Management Console">}}

1. Select an AWS Region from the top toolbar.
1. Select EC2 under the *Services* menu on the left.
1. Click the Launch Instances button, and perform the following steps:
   - Select Linux-based Amazon Machine Image (AMI). For example, `centos`.

   {{< figure src="/images/automate/ha_aws_ami.png" alt="Amazon Machine Image">}}

   - Select the *t2.medium* instance type. Ensure vCPUs is 1, Memory (GiB) is 4, and Instance Storage (GB) is EBS only.
   - Click the Next: Configure Instance Details button.
   - Modify VPC and Subnet values as required.
   - Ensure you have selected 1 in the *Number of instances* field, and make any other required changes.
   - Click Next: Add Storage button.
   - Specify 100 GB of storage in *Size (GiB)* field.
   - Click *Next: Add Tags* button.
   - Specify the key and value for the tag in the *Key* and *Value* fields. This step is optional.
   - Click *Next: Configure Security Group* button.
   - Select Create a new security group or Select an existing security group option.
   - Based on your selection, select needed security groups for your EC2 instance, or add the rule by providing required details. You could change or update the security groups in the future if you want.
   - Ensure Type is SSH, Protocol is TCP, and Port Range is 22 to create rules and connections.
   <!-- u must have private key -->
   - Open port 9631 by adding TCP rule.

Or, launch an EC2 instance, which is available and already defined.

1. Click Review and Launch button.
1. Review all the details and click the Launch button. The AWS console prompts you to either create an existing SSH key pair, or use a pair you have previously established.
   - If you choose to create a new key pair, specify a Key pair name and click Download Key Pair (private key file, .pem). Store the key file in a secure and accessible location.

    {{< figure src="/images/automate/ha_aws_keypair.png" alt="AWS EC2 Launch Instances">}}

   - Else, select an existing key pair.
1. Click Launch Instances. The AWS console confirms the launch of your host.

 {{< figure src="/images/automate/ha_aws_launch_status.png" alt="AWS EC2 Launch Status">}}

You can test the connectivity to the bastion server by navigating to the AWS Console under the *Instances* > *EC2* option and view the fresh bastion server running.

Refer detailed information on deploying Linux bastion hosts to manage your [AWS Cloud deployments remotely](https://aws.amazon.com/quickstart/architecture/linux-bastion/) page.
