+++
title = "Configuring Bastion for AWS Deployment Type"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuring Bastion for AWS Deployment Type"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_aws_bastion.md Configuring Bastion for AWS Deployment Type"
    weight = 210
+++

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
   - Modify **VPC and Subnet values** as required.
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

{{< figure src="/images/automate/ha_aws_launchinstances.png" alt="AWS EC2 Instances">}}

1. Select **Review and Launch** button.
1. Review all the details and select the **Launch** button. The AWS console prompts you to either create an existing SSH key pair, or use a previously established pair.

   - If you choose to create a new key pair, specify a **Key pair name** and select **Download Key Pair** (private key file, .pem). Store the key file in a secure and accessible location.

   {{< figure src="/images/automate/ha_aws_keypair.png" alt="AWS EC2 Launch Instances">}}

   - Else, select an existing key pair.

1. Select the **Launch Instances**. The AWS console confirms the launch of your host.

{{< figure src="/images/automate/ha_aws_launch_status.png" alt="AWS EC2 Launch Status">}}

You can test the connectivity to the bastion server by navigating to the AWS Console under the **Instances** > **EC2** option and view the new bastion server running.

Refer detailed information on deploying Linux bastion hosts to manage your [AWS Cloud deployments remotely](https://aws.amazon.com/quickstart/architecture/linux-bastion/) page.
