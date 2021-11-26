+++
title = "HA Bastion Setup"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Bastion Setup"
    parent = "automate/install"
    identifier = "automate/install/ha_bastion_setup.md HA Bastion Setup"
    weight = 240
+++

## Bastion Host

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer, and all other services are limited to reduce the threat to the computer.

Its purpose is to provide access to a private network from an external network, such as the Internet or outside of a firewall and involves access from untrusted networks or computers. These computers are also equipped with special networking interfaces to withstand high-bandwidth attacks through the internet.

## Bastion Host Setup

Bastion servers are instances that resides within your public subnet and accessed using SSH. The purpose of a bastion host is to restrict access to a private network from an external network. Once remote connectivity establishes with the bastion host, it allows you to use SSH to login to other instances (within private subnets) deeper within your network.

The bastion hosts provide secure access to Linux instances located in the private and public subnets.

## Bastion Host for Chef Automate High Availability (HA)

Virtual machine is required for either of the Chef Automate HA deployment types to trigger the deployment, which is actually a bastion host. This page explains the bastion host requirements and configurations for the two deployment modes of the Chef Automate HA.

### Bastion Server Setup for bare infra??

#### Prerequisites

- Bastion Server/host IP address.
- Memory: Minimum of 100GB
- Hard Disk Space - 4 GB

### Bastion Server Setup for AWS (Amazon Web Services) Deployment

#### Prerequisites

- Configure the AWS Credential on the bastion host. [Bastion Setup]({{< relref "ha_bastion_setup" >}})
- Create the certificate for the DNS
- Operating System (OS): Bastion host with Ubuntu 20.04 or centOs-7 or RHEL-7
- AWS instance type: *t2.medium*
- Memory: Minimum of 100GB
- SSH: VPC to Port 22, publicly accessible
- Setup Virtual Private Cloud (VPC) in AWS 

#### Building an AWS bastion host

Follow these steps to add a bastion host to your Linux environment on AWS cloud, which connects to other internal network hosts:

1. Sign into your AWS account. If you don't have one, sign up at https://aws.amazon.com.
1. Navigate to the AWS Management Console.

![Amazon Management Console](/images/automate/ha-aws-console.png)

1. Select an AWS Region from the top toolbar.
1. Select EC2 under Services menu on the left.
1. Click the Launch Instances button, and perform the following steps:
   1. Select linux based Amazon Machine Image (AMI).

   ![Amazon Machine Image](/images/automate/ha-aws-ami.png)

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
   1. Open port 9631 by adding TCP rule.

Or, launch an EC2 instance, which was previously defined.

1. Click Review and Launch button.
1. Review all the details and click the Launch button. The AWS console prompts you to either create an existing SSH key pair, or use a pair you have previously established. 
   - If you choose to create a new key pair, specify a Key pair name and click Download Key Pair (private key file, .pem). Store the key file in a secure and accessible location.

   ![AWS EC2 Launch Instances](/images/automate/ha-aws-keypair.png)

   - Else, select an existing key pair. 
1. Click Launch Instances. The AWS console confirms the launch of your host.

 ![AWS EC2 Launch Status](/images/automate/ha-aws-launch-status.png)

You can test the connectivity to the bastion server by navigating to the AWS Console under Instances > EC2 option and view the fresh bastion server running.

For detailed information to deploy Linux bastion hosts to manage your AWS Cloud deployments remotely, see https://aws.amazon.com/quickstart/architecture/linux-bastion/.

##### Connect to your Instance

A key pair consists of a public key that AWS stores, and a private key file that you store. Together, they allow you to connect to your instance securely.

1. Navigate to the AWS Management Console.
1. Select Instances > EC2 option from the left menu.
1. Search your instance and click the corresponding Instance ID. The AWS Console displays the Instance Summary screen.
1. Click Connect. The AWS console displays various methods to connect to your instance. Here, we have used Mac system.
1. Open an SSH client.
1. Locate your private key file and navigate to that directory. 
1. Run this command, `chmod 400 key.pem` (key.pem is the name of the key pair file name), to ensure your key is not publicly viewable.

![AWS EC2 Launch Status](/images/automate/ha-aws-connect.png)

1. Connect to your instance using its public DNS. For example, `ssh -i "doc-bastion.pem" ubuntu@ec2-3-24-212-25.ap-southeast-2.compute.amazonaws.com`.
1. Type `yes` when terminal prompts with you for connecting.

![AWS EC2 Launch Status](/images/automate/ha-aws-ssh-connection.png)

This completes the SSH connection to the AWS EC2 instance.

### IAM User

Note: you will by default login as the ubuntu user, but can switch to root access, if needed, using the sudo command.

In order to run the terraform scripts, you need an IAM user with following permissions:

- AdministratorAccess
- AmazonAPIGatewayAdministrator
- AmazonS3FullAccess

- full access is required for s3 backup


These permissions can either be directly added to the user or can be added via IAM Group. Ensure you have the access key id and secret access key for the user. Else, regenerate a new access key and keep it handy.

### AWS Credentials

In order to trigger the deployment, you need to setup the AWS credentials on the bastion host. Follow these steps to do so:

1. Create an user in AWS.

1.  Download the access key and secret key of the user.

1. SSH into the bastion host and create a directory ".aws" in /root.

1. Create a file "credentials" in the /root/.aws directory.

1. touch ~/.aws/credentials

6. Add the access key ID and secret key to the credentials file:

   - aws_access_key_id=access key id of the IAM user
   - aws_secret_access_key=secret access key of the IAM user.


Harden the OS, which basically refers to increasing the security which has been provided by the OS.
Specify appropriate security groups or create a security group for the bastion host.
This will open up the port 22 which is usually used with SSH.
Select a source, which is done to ensure that relevant people (who have access to add their IPs) have access to the Bastion host.
The security groups of the current instances have to be changed to make sure that inbound SSH (if any) can be accessed through the Bastion Host's IP address only.
The local ~/.ssh/config file has to be edited to reflect the bastion host name, username, and a 'Yes' value for the ForwardAgent field. This is used to set up the SSH forwarding via the local machine to the bastion host so that the file used to access the EC2 instance is made available only when the user tries to connect to one of the servers.
Username refers to the person who has the rights to login to the server. Hostname refers to the IP address of the bastion host.
This makes sure that the user can SSH into the Bastion server by just typing 'ssh bastion' from the command line interface.
Bastion Host needs to be accessed with the help of SSH. into an existing server instance, and this way, a much tighter security would be built into the servers, by making these servers accessible only through a Bastion host.
Copy any available VPC ID from aws web portal
Navigate to the Subnet option from the Virtual Private Cloud menu of the AWS console.
Note down the unique Ipv4 CIDR field in the subnet section. We need to derive a value for 'aws_cidr_block_addr'.

Follow the below approach to do so

https://www.calculator.net/ip-subnet-calculator.html?cclass=a&csubnet=20&cip=172.31.0.0&ctype=ipv4&printit=0&x=82&y=36
https://ccna-200-301.online/network-documentation/


Subnet field: iI your vpc IPv4 CIDR block is 172.31.0.0/16 then just add plus 2 to make 18 because we have set /20 in our system.

IP Address: Write down 172.31.0.0 from 172.31.0.0/16 from IPv4 CIDR block and press calculate.

From the results, note the available Network Address field as a value for 'aws_cidr_block_addr= ' field.

Specify CIDR


#### VPC Setup

- VPC Services - of all regions - region in which you are supposed to create the infrastructure. Select the available VPC or create a new one.Which will have CIDR block and Ips available. AWS account has limit of 20 VPCs per region.  no 2 VPCs can communicate directly
- Port 22 is mandatory? Once the basing system is ready, trigger the deployment. These are all AWS specific reqs - port 22 is a std port for SSh

#### Certificates

Certificate is optional. Default certificate of Postgres n es. Required for load balancer. Arn link of the aws certificate customer to provide .. put the certificate in the certificate manager. Customer can give theirs. They can provide to the installation team

- Key Pairs

As a bastion server, it should have the public access with a public IPv4 address.