+++
title = "Create an Amazon OpenSearch Cluster and Obtain Connection Details"

draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Create Amazon OpenSearch"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/create_amazon_opensearch.md Create Amazon OpenSearch"
    weight = 204
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

You can follow the AWS documentation directly for detailed steps on [how to create an Amazon OpenSearch Cluster](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html).

Below is our guide on the steps required to create an Amazon OpenSearch cluster. This guide will walk you through creating an Amazon OpenSearch cluster and retrieving the necessary connection details, including the hostname, username, and password.

## Prerequisites

Before proceeding, make sure you have the following prerequisites in place:

- An active AWS account
- Sufficient permissions to create Amazon OpenSearch clusters

## Step 1: Sign in to the AWS Management Console

1. Open your preferred web browser and go to the [AWS Management Console](https://console.aws.amazon.com/).
1. Sign in to your AWS account using your credentials.

## Step 2: Navigate to the Amazon OpenSearch Service Dashboard

1. Once logged in to the AWS Management Console, search for **OpenSearch** in the search bar at the top of the page.
1. Click on the **Amazon OpenSearch Service** service from the search results to open the Amazon OpenSearch Service dashboard.

## Step 3: Create a New Amazon OpenSearch 1.3 Cluster

1. Click on **Create a new domain** button in the Amazon OpenSearch Service dashboard.
1. Enter a new **Domain name** for your OpenSearch cluster.
1. In **Domain creation method**, select **Standard create**.
1. Choose the appropriate deployment configuration, such as development or production, based on your requirements.
1. Under the **Engine options** section, select **1.3** as the version for your cluster.
1. In the **Networks** section, provide the following information:
   - **VPC**: Select the VPC in which you have your automate cluster.
   - **Subnets**: Select all three private subnets available in your VPC.
   - **Security groups**: Select a security block with incoming access from your VPC CIDR range.
1. In **Fine-grained access control**,
   - Enable **Fine-grained access control**
   - Choose **Create master user**
   - Enter the **Master username** and **Master password** you want to create
1. In **Access policy**,
   - Select **Configure domain level access policy**
   - Choose action **Allow**.
1. Configure the remaining settings per your requirements, such as the number of nodes and storage options.
1. Review all the settings and make sure they are accurate.
1. Click the **Confirm** button to start the cluster creation process.

## Step 4: Wait for the Amazon OpenSearch Cluster to be Created

1. The Amazon OpenSearch cluster creation process may take several minutes. Wait for the process to complete.
1. You can monitor the progress of the cluster creation on the Amazon OpenSearch Service dashboard.

## Step 5: Retrieve Connection Details

You can obtain the necessary connection details once the Amazon OpenSearch 1.3 cluster is created successfully.

1. Go to the Amazon OpenSearch Service dashboard.
1. Find and select your newly created cluster from the list.
1. In the cluster details view, navigate to the **Endpoint** tab.
1. Here, you will find the following connection details:
   - **Domain name**: This is the domain name we gave for this OpenSearch cluster.
   - **Domain URL**: This is the endpoint or hostname of your OpenSearch cluster. It will look something like `my-opensearch-cluster-1234567890.us-east-1.es.amazonaws.com`.
   - **Username**: The username for accessing your OpenSearch cluster.
   - **Password**: The password for the specified username.

## Step 6: Connect to Your Amazon OpenSearch 1.3 Cluster

Using the connection details obtained in the previous step, you can now connect to your Amazon OpenSearch 1.3 cluster from Automate.
Congratulations! You have successfully created an Amazon OpenSearch 1.3 cluster, and it's ready to be used with Automate.
