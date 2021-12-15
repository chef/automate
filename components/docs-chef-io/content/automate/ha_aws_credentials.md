+++
title = "HA AWS Credentials"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA AWS Credentials"
    parent = "automate/install"
    identifier = "automate/install/ha_aws_credentials.md HA AWS Credentials"
    weight = 250
+++

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

Arn link in config.toml… default certificate we can use - certificate manager — 