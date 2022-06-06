+++
title = "Performance Benchmarking"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Performance Benchmarking"
    parent = "automate/deploy_high_availability/performance_benchmarking"
    identifier = "automate/deploy_high_availability/performance_benchmarking/performance_benchmarking.md Performance Benchmarking"
    weight = 220
+++

This document lists the results of the performance testing done in Chef Automate HA for different type of HA cluster configurations and load testing with compliance/node data.

The performance testing pushes four types of node data at the same time, mentioned below:

- Infrastructure Node Success
- Infrastructure Node Failure
- Compliance Success
- Compliance Failure

Below are the different cluster configuration used to do performace testing:

### Cluster Configuration 1

Total 100k nodes:

- **PostgreSQL:** m5.xlarge (vCPU: 4, Memory: 16 GB) - 3 nodes each
- **OpenSearch:** m5.2xlarge (vCPU: 8, Memory: 32 GB) - 3 nodes each
- **Automate and Chef-Server:** c5.xlarge (vCPU: 4, Memory: 8 GB) - 2 nodes each

Following are the results:

| Node Type | Total No. of Nodes | concurrency | Passed | Failed | Total Time Taken |
| :--------------- | :---- | :--- | :------ | :----- |:--------------- |
| Compliance success | 25000 | 250 | 18517 | 6483 | 7 minutes |
| Node Failure | 25000 | 250 | 18961 | 6039 | 14 minutes |
| Node Success | 25000 | 250 | 22570 | 2430 | 36 minutes |
| Compliance failure | 25000 | 250 | 21515 | 3485 | 28 minutes |

### Cluster Configuration 2

Total 100k nodes:

- **PostgreSQL:** m5.2xlarge (vCPU: 8, Memory: 32 GB) - 3 nodes each
- **OpenSearch:** m5.2xlarge (vCPU: 8, Memory: 32 GB) - 5 nodes each
- **Automate and Chef-Server:** c5.xlarge (vCPU: 4, Memory: 8 GB) - 4 nodes each

Following are the results:

| Node Type | Total No. of Nodes | concurrency | passed | failed | Total TIme Taken |
| :--------------- | :---- | :--- | :------ | :----- | :--------------- |
| Compliance success | 25000 | 250 | 24974 | 26 | 3 minutes |
| Node Failure | 25000 | 250 | 24947 | 53 | 9 minutes |
| Node Success | 25000 | 250 | 24994 | 6 | 28 minutes |
| Compliance failure | 25000 | 250 | 24972 | 28 | 27 minutes |

## Minimal Node Deployment Test Results

### Cluster Configuration 1

Total 10k nodes

- **PostgreSQL & OpenSearch:** m5.2xlarge (vCPU: 8, Memory: 32 GB)  - 3 nodes on same machines
- **Automate and Chef-Server:** c5.xlarge (vCPU: 4, Memory: 8 GB)  - 2 nodes on same machine

Following are the results:

| Node Type | Total No. of Nodes | concurrency | passed | failed | Total TIme Taken |
| :--------------- | :---- | :--- | :------ | :----- | :--------------- |
| Compliance success | 2500 | 50 | 2500 | 0 | 1 minute |
| Node Failure | 2500 | 50       | 2500 | 0 | 1.5 minutes |
| Node Success | 2500 | 50       | 2500 | 0 | 4 minutes |
| Compliance failure | 2500 | 50 | 2498 | 2 | 5 minutes |

### Cluster Configuration 2

Total 20k nodes

- **PostgreSQL & OpenSearch:** m5.2xlarge (vCPU: 8, Memory: 32 GB)  - 3 nodes on same machines
- **Automate and Chef-Server:** c5.xlarge (vCPU: 4, Memory: 8 GB)  - 2 nodes on same machine

Following are the results:

| Node Type | Total No. of Nodes | concurrency | passed | failed | Total TIme Taken |
| :--------------- | :---- | :--- | :------ | :----- | :--------------- |
| Compliance success | 5000 | 250 | 4743 | 257 | 1 minute |
| Node Failure | 5000 | 250       | 4818 | 182 | 2 minutes |
| Node Success | 5000 | 250       | 4971 | 29 | 7 minutes |
| Compliance failure | 5000 | 250 | 4900 | 100 | 14 minutes |
