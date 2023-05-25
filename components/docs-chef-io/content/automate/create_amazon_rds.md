+++
title = "Create Amazon RDS"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Create Amazon RDS"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/create_amazon_rds.md Create Amazon RDS"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

# Creating an Amazon RDS PostgreSQL Instance and Obtaining Connection Details

This guide will walk you through the process of creating an Amazon RDS PostgreSQL instance and retrieving the necessary connection details, including the hostname, port, username, and password.

## Prerequisites
Before proceeding, make sure you have the following prerequisites in place:
- An active AWS account
- Sufficient permissions to create Amazon RDS instances

## Step 1: Sign in to the AWS Management Console
1. Open your preferred web browser and go to the [AWS Management Console](https://console.aws.amazon.com/).
2. Sign in to your AWS account using your credentials.

## Step 2: Navigate to the Amazon RDS Dashboard
1. Once you are logged in to the AWS Management Console, search for "RDS" in the search bar at the top of the page.
2. Click on the "Amazon RDS" service from the search results to open the Amazon RDS dashboard.

## Step 3: Create a New Amazon RDS PostgreSQL Instance
1. In the Amazon RDS dashboard, click on the "Create database" button.
2. On the "Choose a database creation method" page, select the "Standard Create" option.
3. Under the "Engine options" section, select "PostgreSQL" as the database engine.
4. Choose PostgreSQL 13.5-R1.
5. Under the "Templates" section, select the template that suits your needs or choose the default template.
6. In the "Settings" section, provide the following information:
   - **DB instance identifier**: Enter a unique identifier for your RDS instance.
   - **Master username**: Specify the username for the master user account.
   - **Master password**: Set a secure password for the master user account.
7. In the "DB instance size" section, select the appropriate instance size for your needs.
8. In the "Connectivity" section, 
  - Select "Don't connect to an EC2 compute resource".
  - Select "Network type" as per your requirements.
  - In "Virtual private cloud" select the VPC that you want to use for your Automate cluster.
  - Choose any private subnet available in your VPC.
  - In "Public Access" select "NO"
9. Configure the remaining settings as per your requirements.
10. Review all the settings and make sure they are accurate.
11. Click on the "Create database" button to start the creation process.

## Step 4: Wait for the Amazon RDS Instance to be Created
1. The RDS instance creation process may take a few minutes. Wait for the process to complete.
2. You can monitor the progress of the instance creation on the Amazon RDS dashboard.

## Step 5: Open port in RDS security group

1. Go to the Amazon RDS dashboard.
2. Find and select your newly created PostgreSQL instance from the list.
3. In the instance details view, navigate to the "Connectivity & security" tab.
4. Open the Security Group under "VPC security groups"
5. Under "Inbound Rules", edit and select "Type" as "PostgreSQL"
6. Select "Source" as "custom" and give appropriate cidr block for your VPC
7. Click "Save Rules"

## Step 6: Retrieve Connection Details
Once the Amazon RDS PostgreSQL instance is created successfully, you can obtain the necessary connection details.

1. Go to the Amazon RDS dashboard.
2. Find and select your newly created PostgreSQL instance from the list.
3. In the instance details view, navigate to the "Connectivity & security" tab.
4. Here you will find the following connection details:
   - **Instance URL**: This is the endpoint or hostname of your RDS instance. It will look something like `my-rds-instance.abcdefg12345.us-east-1.rds.amazonaws.com`.
   - **Port**: The port number on which your PostgreSQL instance is listening. The default port is usually `5432`.
   - **Username**: The username of the master user account you specified during instance creation.
   - **Password**: The password for the master user account.

## Step 6: Connect to Your Amazon RDS PostgreSQL Instance
Using the connection details obtained in the previous step, you can now connect to your Amazon RDS PostgreSQL instance from Automate.

Congratulations! You have successfully created an Amazon RDS PostgreSQL instance and its ready to be used with Automate.