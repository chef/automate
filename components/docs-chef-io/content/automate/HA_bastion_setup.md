+++
title = "Chef Automate HA - Bastion Setup"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - Bastion Setup"
    parent = "automate/High_Availability"
    identifier = "automate/reference/ha_bastion_setup.md HA Bastion Setup"
    weight = 40
+++

## Bastion Host

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer, and all other services are limited to reduce the threat to the computer.
  
Its purpose is to provide access to a private network from an external network, such as the Internet or outside of a firewall and involves access from untrusted networks or computers. These computers are also equipped with special networking interfaces to withstand high-bandwidth attacks through the internet.

## Bastion Host Setup

Bastion servers are instances that resides within your public subnet and accessed using SSH. The purpose of a bastion host is to restrict access to a private network from an external network. Once remote connectivity establishes with the bastion host, it allows you to use SSH to login to other instances (within private subnets) deeper within your network.

Virtual machine is required for either of the Chef Automate High Availability (HA) deployment types, which is actually a bastion host.

### Bastion Server Setup for bare infra??

### Bastion Server Setup for AWS Deployment

#### Prerequisites

- Need to configure the AWS Credential on the bastion host. [Bastion Setup]({{< relref "ha_bastion_setup" >}})
- We need to create the certificate for the DNS.
- Operating System (OS): Bastion host with Ubuntu 20.04 or centOs-7 or RHEL-7
- AWS instance type: *t2.medium* 
- Memory: Minimum of 100GB 
- SSH: VPC to Port 22, publicly accessible
-
- VPC Services – of all regions – region in which you are supposed to create the infrastructure. Select the available VPC or create a new one.Which will have CIDR block and Ips available. AWS account has limit of 20 VPCs per region.  no 2 VPCs can communicate directly
- Port 22 is mandatory? Once the basing system is ready, trigger the deployment. These are all AWS specific reqs – port 22 is a std port for SSh

- Certificates
- Key Pairs

The Chef Automate High Availability (HA) on AWS cloud at the customer venue requires you to setup bastion host to trigger the deployment.

As a bastion server, it should have the public access with a public IPv4 address.

Building an AWS bastion host

To build a bastion host to connect to other internal network hosts using AWS (Amazon Web Services), follow these steps:

- Navigate to the AWS Console and choose EC2 under All Services.
- Click the blue Launch Instance button.
- Select the *t2.medium* instance type.
- select needed security groups for your EC2 instance. Of course, you could change or update the security groups in the future if you want.

- Click Review and Launch at the bottom of the page
- Choose to Create a new security group and give it an appropriate name like ssh-from-Internet.
- Create SSH rules that only allow connections for your team.
- Click Review and LaunchWhen when all your teams IP addresses are added. You will be asked to either create an existing SSH key pair, or use a pair you have previously established. If you choose to create a new key pair, give it a Key pair name, and then click Download Key Pair (private key file, .pem).
- Store the key file in a secure and accessible location.
- Click Launch Instances. The AWS console confirms the launch of your host.

Managing SSH keys

A key pair consists of a public key that AWS stores, and a private key file that you store. Together, they allow you to connect to your instance securely.

For windows systems, the private key file is required to obtain the password used to log into your instance. For Linux systems, the private key file allows you to securely SSH into your instance.

Configuring hosts for logging

Back at the AWS Console under EC2 → Instances you should now see your fresh bastion server running.
If you click into the white space under Name, you will see a little pencil icon appear. Click the pencil and spcify your host a more appropriate name, and then click the checkmark icon.
From the buttons near the top of the screen, click Connect, and you will be shown a help window with instructions on using SSH to connect to your new bastion server
SSH into the private instance
open the  Terminal command line and go to the directory where our private key file - the one with .pem extension
Change the permission of the key file, chmod 400 
Specify the command copied form AWS console to connect to the instance:
Note: you will by default login as the ubuntu user, but can switch to root access, if needed, using the sudo command.



### IAM User 

In order to run the terraform scripts, you need an IAM user with following permissions:

- AdministratorAccess 
- AmazonAPIGatewayAdministrator 
- AmazonS3FullAccess 

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


Steps to create a bastion host for a specific AWS infrastructure
Following are the steps to create a bastion host:

Sign into your AWS account.
Create an EC2 instance or launch an EC2 instance which was previously defined.
Harden the OS, which basically refers to increasing the security which has been provided by the OS.
Specify appropriate security groups or create a security group for the bastion host.
This will open up the port 22 which is usually used with SSH.
Select a source, which is done to ensure that relevant people (who have access to add their IPs) have access to the Bastion host.
The security groups of the current instances have to be changed to make sure that inbound SSH (if any) can be accessed through the Bastion Host’s IP address only.
The local ~/.ssh/config file has to be edited to reflect the bastion host name, username, and a ‘Yes’ value for the ForwardAgent field. This is used to set up the SSH forwarding via the local machine to the bastion host so that the file used to access the EC2 instance is made available only when the user tries to connect to one of the servers.
Username refers to the person who has the rights to login to the server. Hostname refers to the IP address of the bastion host.
This makes sure that the user can SSH into the Bastion server by just typing ‘ssh bastion’ from the command line interface.
Bastion Host needs to be accessed with the help of SSH. into an existing server instance, and this way, a much tighter security would be built into the servers, by making these servers accessible only through a Bastion host.
Copy any available VPC ID from aws web portal 
Navigate to the Subnet option from the Virtual Private Cloud menu of the AWS console.
Note down the unique Ipv4 CIDR field in the subnet section. We need to derive a value for ‘aws_cidr_block_addr’. 

Follow the below approach to do so

https://www.calculator.net/ip-subnet-calculator.html?cclass=a&csubnet=20&cip=172.31.0.0&ctype=ipv4&printit=0&x=82&y=36 
https://ccna-200-301.online/network-documentation/


Subnet field: iI your vpc IPv4 CIDR block is 172.31.0.0/16 then just add plus 2 to make 18 because we have set /20 in our system. 

IP Address: Write down 172.31.0.0 from 172.31.0.0/16 from IPv4 CIDR block and press calculate. 

From the results, note the available Network Address field as a value for ‘aws_cidr_block_addr= ‘ field.

Specify CIDR


