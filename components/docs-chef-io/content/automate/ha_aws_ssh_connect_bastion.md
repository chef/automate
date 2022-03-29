+++
title = "Establishing SSH Connection with Bastion Host"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Establishing SSH Connection with Bastion Host"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_aws_ssh_connect_bastion.md Establishing SSH Connection with Bastion Host"
    weight = 220
+++

This page explains the procedure to establish the connection between your bastion host and an AWS account. A key pair consists of a public key that AWS stores and a private key file you store. Together, they allow you to connect to your instance securely.

1. Navigate to the AWS Management Console.
1. Select **Instances** > **EC2** option from the left menu.
1. Search your instance and select the corresponding Instance ID. The **AWS Console** displays the **Instance Summary** screen.
1. Select **Connect**. The AWS console displays a list of methods to connect to your instance. Here, we have used the Mac system.
1. Open an **SSH client**.
1. Locate your private key file and navigate to that directory.
1. Execute the `chmod 400 key.pem` command (`key.pem` is the name of the key pair file name) to ensure your key is not publicly viewable.

![AWS EC2 Launch Status](/images/automate/ha_aws_connect.png)

{{< figure src="/images/automate/ha_aws_launch_status.png" alt="AWS EC2 Launch Status">}}

1. Connect to your instance using its public DNS. For example, `ssh -i "doc-bastion.pem" ubuntu@ec2-3-24-212-25.ap-southeast-2.compute.amazonaws.com`.
1. Select `yes` when the terminal prompts you to connect.

{{< figure src="/images/automate/ha_aws_ssh_connection.png" alt="AWS SSH Connection Details">}}

This completes the SSH connection to the AWS EC2 instance.

By default, you can log on as a Ubuntu user. You can switch to *root* access using the `sudo` command.

Refer [Mounting the file system on the EC2 instance and testing](https://docs.aws.amazon.com/efs/latest/ug/wt1-test.html) for detailed information.
