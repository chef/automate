+++
title = "IAM Actions"
description = "Reference Actions for Roles"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "reference"
    weight = 60
+++

Reference the chart on this page when creating a *Role* to know which action grants access to what page in the browser.

*IAM Action* lists the associated action or actions required to access that page in the browser.

|  Task           |  IAM Action      | API endpoint  | Browser Page |
| --------------- | ---------------- | ------------- | --------- |
| Event Feed      | event:*         | <endpoint>     | Dashboards |
| Service Groups  | applications:*  | /applications/service-groups | Applications |
| Client Runs     | infra:nodes:*   | /nodes | Infrastructure |
| Chef Servers    | infra:servers:* | <endpoint>     | Infrastructure |
| Reports         | compliance:reporting:*  | /compliance/reporting/reports | Compliance |
| Scan Jobs | [compliance:scannerJobs:* , infra:nodes:* , infra:nodeManagers:* , compliance:profiles:*]| /compliance/scanner/jobs | Compliance |
| Profiles | compliance:profiles:* | <endpoint>     | Compliance |
| Node Management | notifications:* | <endpoint>     | Settings |
| Node Management | datafeed:* | /datafeed/destination | Settings |
| Node Management | [infra:nodeManagers:* , infra:nodes:* , secrets:*] | <endpoint>  | Settings |
| Node Management | secrets:* | /secrets | Settings |
| Node Management | retention:* | /retention/nodes | Settings |
| Identity | iam:users:* | <endpoint>     | Settings |
| Identity | iam:teams:* | <endpoint>     | Settings |
| Identity | iam:tokens:* | <endpoint>     | Settings |
| Access Management | iam:policies:* | <endpoint>     | Settings |
| Access Management | iam:roles:*    | <endpoint>     | Settings |
| Access Management | iam:projects:* | <endpoint>     | Settings |
