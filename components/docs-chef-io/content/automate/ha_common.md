+++
title = "Reference Topics"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Reference Topics"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_common.md Reference Topics"
    weight = 240
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

This page details the topics that are common to Chef Automate High Availability (HA) deployment models and aid you in deploying it in your network infrastructure.

## IAM Users

AWS Identity and Access Management (IAM) is a web service that helps you securely control access to AWS resources. You use IAM to control who is authenticated (signed in) and authorized (has permissions) to use resources.

To run the terraform scripts, you need an IAM user with the following permissions:

- AdministratorAccess
- AmazonAPIGatewayAdministrator
- AmazonS3FullAccess

These permissions can either be directly added to the user or via IAM Group.

{{< note >}}

Keep access key ID and secret access key handy. Refer [access key](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) page for detailed information on regenerating access key ID and secret access key.

{{< /note >}}

Once we have an AWS account, we'll need to create an IAM user to programmatically interact with it and to configure our AWS CLI (command-line interface). Amazon IAM enables you to manage users and user permissions in AWS. You can create one or more IAM users in your AWS account. You might create an IAM user for someone who needs access to your AWS console, or when you have a new application that needs to make API calls to AWS. This is to add an extra layer of security to your AWS account.

### Creating an IAM User

1. Navigate to your AWS account.
1. Select *IAM* from the list of services from the AWS console. The *IAM dashboard* screen appears.

    {{< figure src="/images/automate/ha_aws_iam.png" alt="AWS IAM Dashboard">}}

1. Select *Users* from the *Access management* menu on the left.
1. Select *Add Users*. The *Set User Details* screen appears.
1. Type the user name for the new user and other necessary details.

    {{< figure src="/images/automate/ha_aws_iam_user.png" alt="AWS IAM User Creation">}}

1. Check the *Access key - Programmatic access* box under *Select AWS access type* section.

    {{< figure src="/images/automate/ha_aws_iam_paccess.png" alt="AWS IAM User - Programmetic Access">}}

    This is the sign-in name for AWS. If you want to add multiple users, choose to Add another user for each additional user and type their user names. You can add up to 10 users at one time.

    This account will be used by your AWS CLI and will be connecting to the AWS API directly by not using the Management Console.

1. Select *Next: Permissions*.
1. Select *Attach existing policies directly*. Filter the policies by keyword: IAM. For this user, select IAMFullAccess from the list of available policies. The IAMFullAccess policy enables this user to create and manage user permissions in AWS.

    {{< figure src="/images/automate/ha_aws_iam_policy.png" alt="AWS IAM User Policy">}}

1. Set the user permissions.
1. Search for **AdministratorAccess* and select the policy.
1. Select *Next: Tags*.
1. Provide key name and value as tagging for the user been created.
1. Select *Next: Review*.

    {{< figure src="/images/automate/ha_aws_iam_user_review.png" alt="AWS IAM User Review with permissions">}}

1. Select *Create user*.
1. Select *show* to reveal the secret access key.
1. Download and save the *Secret access key*.

    {{< figure src="/images/automate/ha_aws_iam_user_created.png" alt="AWS IAM User Created with Access Key">}}

1. Take a note of the Access key ID and Secret access key.

Now let's configure our AWS CLI to deploy our applications from our command line.

Refer [Creating an IAM User](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html) page for creating an IAM user through CLI and API methods.

## Certificates

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity and reliability of a website or Web application is established.

Certificates should be rotated periodically, to ensure optimal security.

### How is Certificate Rotation Helpful?

Certificate rotation means the replacement of existing certificates with new ones when any certificate expires or based on your organization policy. A new CA authority is substituted for the old requiring a replacement of root certificate for the cluster.

The certificate rotation is also required when key for a node, client, or CA is compromised. Then, you need to modify the contents of a certificate, for example, to add another DNS name or the IP address of a load balancer through which a node can be reached. In this case, you  would need to rotate only the node certificates.

#### How to Rotate the Certificates?

You can generate the required certificates on your own or you can use the existing certificates of your organization. Ensure you execute all the below commands from the `cd /hab/a2_deploy_workspace` path.

Follow these steps to rotate your certificates that are to be used in Chef Automate High Availability (HA):

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.
1. Type the command, `./scripts/credentials set ssl --rotate all` and press **Enter**. This command rotates all the certificates of your organization.

    {{< note >}}

    When you run this command first time, a series of certificates are created and saved in `/hab/a2_deploy_workspace/certs` location. You need to identify the appropriate certificates. For example, to rotate certificates for PostgreSQL, use certificate values into *pg_ssl_private.key*,  *pg_ssl_public.pem*, and *ca_root.pem*. Likewise, to rotate certificates for ElasticSearch, use certificate values into *ca_root.pem*, *es_admin_ssl_private.key*, *es_admin_ssl_public.pem*, *es_ssl_private.key*, *es_ssl_public.pem*, *kibana_ssl_private.key*, *kibana_ssl_public.pem*.

    {{< /note >}}

1. For rotating the PostgreSQL certificates, type the command `./scripts/credentials set ssl --pg-ssl` and press **Enter**. .

1. For rotating the Elasticsearch certificates, type the command, `./scripts/credentials set ssl --es-ssl` and press **Enter**.

    <!-- 4. Copy your *x.509 SSL certs* into the appropriate files in `certs/` folder. -->

    <!-- - Place your root certificate into `ca_root.pem file`. -->

    <!-- - Place your intermediate CA into the `pem` file. -->

1. If your organization issues certificate from an intermediate CA, then place the respective certificate after the server certificate as per order listed. For example, in `certs/pg_ssl_public.pem`, paste it as them as listed:

    - Server Certificate
    - Intermediate CA Certificate 1
    - Intermediate CA Certificate n

1. Type the command, `./scripts/credentials set ssl` (with the appropriate options) and press **Enter**. This command deploys the nodes.

1. Type the command, `./scripts/credentials set ssl  --help` and press **Enter**. This command provides you information and list of commands related to certificate rotation.

1. For rotating the PostgreSQL credentials, type the command `./scripts/credentials set postgresql --auto` and press **Enter**. .

1. For rotating the Elasticsearch credentials, type the command, `./scripts/credentials set elasticsearch --auto` and press **Enter**.

### What are Self Signed Certificates?

A self signed certificate is a digital certificate that is not signed by a publicly trusted certificate authority (CA). They are created, issued, and signed by the company or developer who is responsible for the website or software being signed. The private key used in such certificate is not validated by a third party and is generally used in low-risk internal networks or in the software development phase. In addition, unlike CA-issued certificates, self-signed certificates cannot be revoked.

#### Certificate Creation

You can create a self-signed key and certificate pair with OpenSSL utility, a command line tool for creating and managing OpenSSL certificates, keys, and other files.

Follow these steps to create a self-sign certificate:

1. Install an *openssl* utility.

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

1. Type the command, `./scripts/credentials set ssl --rotate-all`. This command creates a skeleton of certificates.

1. Copy the below *bash script* to a new file:

    ```bash
    # !/bin/bash

    echo extendedKeyUsage = clientAuth, serverAuth > server_cert_ext.cnf

    echo extendedKeyUsage = clientAuth, serverAuth > client_cert_ext.cnf

    openssl genrsa -out ca_root.key 2048

    openssl req -x509 -new -key ca_root.key -sha256 -out ca_root.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'

    openssl genrsa -out admin-pkcs12.key 2048

    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "admin-pkcs12.key" -topk8 -out "es_admin_ssl_private.key" -nocrypt

    openssl req -new -key es_admin_ssl_private.key -out admin.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'

    openssl x509 -extfile <(printf "subjectAltName=DNS:chefadmin") -req -in admin.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_admin_ssl_public.pem -sha256 -extfile server_cert_ext.cnf

    openssl genrsa -out ssl-pkcs12.key 2048

    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out  es_ssl_private.key -nocrypt

    openssl req -new -key es_ssl_private.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode'

    openssl x509 -extfile <(printf "subjectAltName=DNS:chefnode") -req -in ssl.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_ssl_public.pem -sha256 -extfile client_cert_ext.cnf

    cp ca_root.pem /hab/a2_deploy_workspace/certs/ca_root.pem

    cp es_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/es_admin_ssl_public.pem

    cp es_admin_ssl_private.key /hab/a2_deploy_workspace/certs/es_admin_ssl_private.key

    cp es_ssl_public.pem /hab/a2_deploy_workspace/certs/es_ssl_public.pem

    cp es_ssl_private.key /hab/a2_deploy_workspace/certs/es_ssl_private.key

    cp es_admin_ssl_private.key /hab/a2_deploy_workspace/certs/kibana_ssl_private.key

    cp es_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/kibana_ssl_public.pem

    cp es_ssl_private.key /hab/a2_deploy_workspace/certs/pg_ssl_private.key

    cp es_ssl_public.pem /hab/a2_deploy_workspace/certs/pg_ssl_public.pem
    ```

1. Navigate to your bastion host.

1. Execute the new file that has the copied bash script. The script generates the certificates at `/hab/a2_deploy_worspace/certs` directory. For example, `bash cert.sh`, where *cert.sh is the name of the newly created bash script file.

1. Again, navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

1. Execute following commands in the same order as listed to apply the generated certificates:

    - ./scripts/credentials set ssl --es-ssl
    - ./scripts/credentials set ssl --pg-ssl
    - ./scripts/credentials set ssl --kibana-ssl

    Once the certificates are applied successfully, the following confirmation message appears as shown in the screen:

    ```bash
        STDERR:
        EXIT_STATUS: 0
        I, [2021-12-20T16:44:20.979703 #411121] INFO - - : STDOUT: >> Setting new configuration version 1640018660 for automate-ha-postgresql.default
        Creating service configuration
        Applying via peer 127.0.0.1:9632
        * Applied configuration

        STDERR:
        EXIT_STATUS: 0
        I, [2021-12-20T16:44:20.979815 #411121] INFO - - : * SSL Certificates Rotated *
    ```

1. Navigate to the Chef Automate and Chef Server instances and check the Chef Service health status. If the service is down or critical, then  wait for three to four minutes for the instances to be up.

## AWS Certificate Manager

This section explains how to use the **AWS Certificate Manager (ACM)** console to generate a public ACM certificate to create a Load Balancer (LB).

You can either generate a new public certificate or copy an existing Amazon Resource Name (ARN) value of the certificate of your selected region in the AWS console.

{{< figure src="/images/automate/ha_aws_cert_mngr_arn.png" alt="AWS ACM ARN Value">}}

### Requesting a Public Certificate using AWS console

Follow these steps if you want to request an ACM public certificate:

1. Sign in to the **AWS Management Console**.

1. Open the [ACM console](https://console.aws.amazon.com/acm/home).

1. Select your region from the top-right corner of the console.

1. Select **Request a certificate**.

    {{< figure src="/images/automate/ha_aws_cert_mngr_console.png" alt="AWS ACM">}}

1. Select the **Request a public certificate** option from the **Certificate type** page, and select **Next**.

1. In the **Domain names** section, enter your domain name. You can use a fully qualified domain name (FQDN), such as `www.example.com`, or a bare domain name such as `example.com`.

    Before ACM issues a certificate, it validates that you own or control the domain names in your certificate request. You can use either email validation or DNS validation.

1. In the **Select validation method** section, select either *DNS validation* or *Email validation*.

1. From the **Tags** page, you can optionally tag your certificate. Tags are key-value pairs that serve as metadata for identifying and organizing AWS resources.

1. Select **Request**. The console returns you to your certificate list after processing the request, where your new certificate displays with status, *Pending validation*.

You can also request a public certificate using the Command Line Interface (CLI). For detailed information on requesting these certificates, refer  [AWS documentation on requesting a public certificate](https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html).

### Providing Certificate ARN Value in `a2ha.rb` Configuration file

Follow these steps if you want to use the AWS certificates in your network infrastructure:

1. Log in as a Sudo user by using `su-` command.

1. Navigate to the `hab` workspace directory.

1. Open the `a2ha.rb` file in any editor of your choice.

    {{< figure src="/images/automate/ha_load_balancer.png" alt="AWS Certificate ARN Value">}}

1. Copy and paste the certificate ARN from AWS console to the `chef_server_lb_certificate_arn` and `automate_server_lb_certificate_arn` fields in the `a2ha.rb` file. For example:

    ```ruby
    automate_lb_certificate_arn = "areas:am:a-southeast-2:112758395563:certificate/9b9fcc04-6513-4ac5-9332-26a59a6ce4e"
    chef_server_lb_certificate_arn = "areas:am:a-southeast-2:112758395563:certificate/9b9fcc04-6513-4ac5-9932-262a59a6ce4e"
    ```

1. Save and close the file.

## Destroying Chef Automate HA Infrastructure

Follow any of the step to destroy terraform or infrastructure created while deploying Chef Automate High Availability (HA) using AWS or Bare-metal model.

- If the *provision-infra* command fails, execute the following command to clear the space utilized by this command before it fails, execute the command:

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate;cd $i;done`

- If the *provision-infra* command ran successfully and if you want to clear the space utilized by this command, execute these commands in the order listed:

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.

    - If you have deployed the Chef Automate HA successfully and wanted to destroy the deployment part alone, execute the command:

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done`

    {{< note spaces=4 >}}

    The deployment destroy does not remove any remote server configuration made, however it taints the terraform and thus you need to redo the configurations.

    {{< /note >}}

    - If you have deployed the Chef Automate HA successfully and wanted to destroy the entire infrastructure instances, execute these commands:

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.

## Validation Commands

This section elaborates the validation procedure that checks the firewall rules and ports before Chef Automate High Availability (HA) backend cluster deployment in your network infrastructure.

Follow these steps to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in air-gapped environment (means no access to the internet):

1. Download hab, *hab-x86_64-linux.tar.gz* by executing the command, `sudo wget https://packages.chef.io/files/stable/habitat/latest/hab-x86_64-linux.tar.gz`.

1. Install hab package in your internet environment by executing the following commands that generate *netcate package*:

    ```bash

    sudo tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
    export HAB_LICENSE=accept-no-persist
    hab pkg install core/netcat -bf
    ls -dtr1 /hab/cache/artifacts/core-netcat-*
    ```

1. Provide the path of the `config.toml` file, `hab-utitlity` and `netcate` package in the command, ./chef-automate validate-ha-infrastructure */path/to/config.toml* */path/to/hab.tar.gz* */path/to/netcat.hart*  as parameters.

    ```bash
    ./chef-automate validate-ha-infrastructure /root/config.toml /root/hab-x86_64-linux.tar.gz /hab/cache/artifact/core-netcat-<version>.hart
    ```

This command show the status of the set firewall rules and the ports configured.

### Validation Procedure for Airgap Environment

You need to execute the following command to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in your network environment which has access to the internet:

`./chef-automate validate-ha-infrastructure /path/to/config.toml`

where you need to provide the path of the *config.toml* file in */path/to/config.toml* in the above command.

This command show the status of the set firewall rules and the ports configured.

## Understanding VPC

Amazon VPC, a virtual network dedicated to your AWS account, enables you to launch AWS resources into a virtual network. This virtual network resembles a traditional network that you had to operate in your own data center, with the benefits of using the scalable infrastructure of AWS.

Amazon VPC is the networking layer for Amazon EC2. Amazon Elastic Compute Cloud (Amazon EC2) provides scalable computing capacity in the Amazon Web Services (AWS) Cloud. Using Amazon EC2 eliminates your need to invest in hardware upfront, so you can develop and deploy applications faster. You can use Amazon EC2 to launch as many or as few virtual servers as you need, configure security and networking, and manage storage. Amazon EC2 enables you to scale up or down to handle changes in requirements or spikes in popularity, reducing your need to forecast traffic.

VPC creates an isolated virtual network environment in the AWS cloud, dedicated to your AWS account. Other AWS resources and services operate inside VPC networks to provide cloud services. AWS VPC looks familiar to anyone running a physical Data Center (DC). A VPC behaves like a traditional TCP/IP network that can be expanded and scaled as needed. However, the DC components you are used to dealing with---such as routers, switches, and VLANS---do not explicitly exist in a VPC. They have been abstracted and re-engineered into cloud software.

All VPCs are created and exist in one--and only one--AWS region. AWS regions are geographic locations where Amazon clusters its cloud data centers. The advantage of regionalisation is that a regional VPC provides network services originating from that geographical area. If you need to provide closer access for customers in another region, you can set up another VPC in that region. This aligns nicely with the theory of AWS cloud computing, where IT applications and resources are delivered through the internet on-demand and with pay-as-you-go pricing. Limiting VPC configurations to specific regions allows you to selectively provide network services where they are needed, as they are needed.

Each Amazon account can host multiple VPCs. Because VPCs are isolated, you can duplicate private subnets among VPCs the same way you could use the same subnet in two different physical data centers. You can also add public IP addresses that can be used to reach VPC-launched instances from the internet.

You can modify or use that VPC for your cloud configurations, or you can build a new VPC and support services from scratch. However, no VPCs can communicate directly.

### VPC Setup

You can either create a new VPC or use an existing available one in the region where you are setting up the Chef Automate HA infrastructure.

#### VPC Limit

The default limit to create a VPC in a region is *5*. However, if the VPCs used in the respective region are exhausted, you can increase the limit in your AWS account. Chef Automate HA on AWS deployment creates two VPCs, one for the bastion host and another for the rest of the node in a cluster.

{{< note >}}

You require a minimum of three node clusters for ElaticSearcg and Postgres-sql instances.

{{< /note >}}

AWS limits the size of each VPC; a user cannot change the size once the VPC has been created. Amazon VPC also sets a limit of 200 subnets per VPC, each of which can support a minimum of 14 IP addresses. AWS places further limitations per account / per region, including limiting the number of VPCs to five, the number of Elastic IP addresses to five, the number of Internet gateways per VPC to one, the number of virtual private gateways to five, and the number of customer gateways to 50.

VPC IP address ranges are defined using Classless interdomain routing (CIDR) IPv4 and IPv6 blocks. You can add primary and secondary CIDR blocks to your VPC, if the secondary CIDR block comes from the same address range as the primary block.

#### Copying an Existing VPC

1. Navigate to the *AWS Management Console*.

1. Select your *AWS Region* from the top toolbar.

1. From the navigation pane, select *VPC Dashboard* in the upper-left corner.

1. Select *Your VPCs*.

1. Copy any available *VPC ID* from the *Your VPCs* screen.

{{< figure src="/images/automate/ha_aws_vpc_existing.png" alt="Using Existing VPC">}}

##### Adding a Private Subnet to the Available VPC

1. From the navigation pane, select *Subnets*.

1. Search your *VPC ID* in the *Subnets* screen.

1. Copy the corresponding *IPv4 CIDR* value.

{{< figure src="/images/automate/ha_aws_vpc_existing_subnet.png" alt="Using Existing VPC Subnet Value">}}

#### Creating a VPC

1. Navigate to the *AWS Management Console*.

1. Select your *AWS Region* from the top toolbar.

1. From the navigation pane, select *VPC Dashboard* in the upper-left corner.

1. Select *Your VPCs*.

1. Select *Create VPC* from the left.

1. For *IPv4 CIDR* block, enter the CIDR block for the VPC. We recommend that you use a CIDR block from the private (non-publicly routable) IP address ranges. For example, 10.0.0.0/16. For more information, refer [VPC and Subnet Sizing for IPv4 page](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#vpc-sizing-ipv4).

1. For *IPv6 CIDR* block, keep No IPv6 CIDR Block.

1. For VPC name, enter a *tag name*.

1. Select *Create VPC*. After the VPC is created, choose OK.

{{< figure src="/images/automate/ha_aws_vpc.png" alt="VPC Creation">}}

##### Adding a Private Subnet

1. From the navigation pane, select *Subnets*.

1. Select *Create Subnet*.

1. Select your *VPC ID* from the drop-down.

1. Enter *Subnet name* for the private subnet (for example, WorkSpaces Private Subnet 2).

1. To make an appropriate selection for *Availability Zone*, see Availability Zones for Amazon WorkSpaces.

1. Enter the CIDR block for the subnet in the *IPv4 CIDR block*. For example, 10.0.2.0/24. Ensure you provide a unique value.

1. Select *Create Subnet*.

{{< figure src="/images/automate/ha_aws_subnet.png" alt="VPC Subnets">}}

{{< note >}}

Refer [select correct CIDR block](https://www.calculator.net/ip-subnet-calculator.html?cclass=a&csubnet=20&cip=172.31.0.0&ctype=ipv4&printit=0&x=82&y=36) page for selecting unique CIDR value for your VPC ID to make it unique.

{{< /note >}}

## Understanding CIDR

We need to understand IPv4 addresses/ notation to understand CIDR blocks. For eg, 10.10.101.5, might be the address of the database, and it's a 32-bit binary number. So, the 10 maps to the first octet of the 0001010, another octet for the second 10, a third octet, fourth octet, each one ranging from 0 to 255 as far as our numbers. We are not describing a single number and describing a range of numbers, all the possible IP addresses that begin with the numbers 10.10. So, to describe a number, a range of numbers, that begin 10.10 using CIDR notation. CIDR stands for Classless Inter-Domain Routing, a 32 -bit number underlying the octets.

In this case, we are freezing the first 16 bits, and wild card the rest, so we draw out the number as 10.10, meaning these are the numbers that are going to stay the same. It doesn't matter commonly we'll put zeros here. And then, after the slash, announced how many bits are frozen, so 10.10.0.0. whatever/16 indicates, in this case, the first 2 octets never change, the last 2 can be whatever you want. Inside a notation is then simply determining how many bits are you freezing. The rest are all being wild carded in IPv4 notation.

/16 is the most common number we'll see for a VPC CIDR block. It's also the most you're allowed to do. You certainly could go smaller than /16, and smaller being 17, 18, 19, and so on in this case, and you can go as small as /28, but that's only going to give you a possible 12 addresses, 16 addresses minus the ones we take away, that's going to be for use inside your VPC, whereas a /16 is going to give you about 65,000 possible addresses.

If /16 is my CIDR block for your main VPC, we then subdivide into subnets, which need to be a subset of the /16, so they all need to start at least with a 10.10, but if I want multiple subnets, I then don't want any collisions, commonly we'll see those as /24. What a /24 means, means I'm freezing the first 24 bits. So say I want the subnet where my database lives, so we call that out as 10.10.101. who cares /24, which means for this subnet, if I'm defining it as a /24, I could have any private IP address beginning from 0 up to 255. In other words, wildcard the last 8 bits.

A /32 is a single specific address. In this case, 10.10.101.5, not wildcarding anything. This is only one IP address used to authorize in a security group traffic. Likewise, if you want to authorize traffic from the entire internet, wild card everything. So 000/0 becomes the final notation to the front-end web servers or other elements.

### Example config.toml file with VPC and CIDR Values

```ruby
region = "ap-south-1"
aws_vpc_id  = "vpc-8d1390e5"
aws_cidr_block_addr  = "172.31.128.0"
```
