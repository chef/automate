+++
title = "Nodes API"
description = "Using the Nodes Service"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "compliance"
    weight = 40
+++

### Nodes

The `/nodes` endpoint in Chef Automate is something of a 'logbook' of the nodes in your infrastructure.

When a user creates a node, that node is added to the `/nodes` endpoint.

When a user adds a node integration, like aws or azure, nodes are added to the `/nodes` endpoint.

When an inspec report is ingested, a node is added to the `/nodes` endpoint. If it already exists, the last contact time is updated.

### Node Status

All nodes have one of three possible statuses: 'unknown', 'reachable', and 'unreachable'. The default status is 'unknown'.

Each time a user adds one or more nodes manually or with a node integration (AWS, Azure, or GCP), Chef Automate runs an`inspec detect` job on the newly added node(s).
If the detect job is successful, the node status updates from 'unknown' to 'reachable', and the platform information is updated from the `inspec detect` results.
If the detect job is unsuccessful, meaning the node could not be reached, the node's status updates to 'unreachable'.
The status updates every time a scan job runs on the node.

### Node State

All nodes have a state.
Possible states are unknown(''), 'running', 'stopped', and 'terminated'. Default state: '' (unknown).

<!-- Node state can be updated manually for all Automate (manually managed) nodes.
```
``` I need to expose that endpoint in the gateway -->

 For all nodes added through integrations, node state is updated both when nodes are added and on a scheduled polling interval.

If a node is found to have a state other than 'running', the node status is then also updated to 'unreachable'.

Node state is updated to 'running' on report ingestion if the end time recorded by the inspec report is less than ten minutes from the ingestion time.

### Filtering Nodes

The `/nodes` endpoint supports filtering by:

- name
- platform_name
- platform_release
- manager_type ('automate', 'aws-ec2', 'aws-api', ...)
- manager_id
- account_id (the aws account ID or azure tenant ID)
- region
- source_id (a reference to the primary provider's node)
- state
- statechange_timestamp
- status
- tags

### Bulk Node Add

 Use the `nodes/bulk-create` endpoint to add multiple nodes with the same set of tags and credentials.  Specifying a `name_prefix` for the nodes in question results in a node name of `prefix-host`.  Specified tags will be added to each node. The endpoint takes an array of node objects, allowing users to add as many nodes as needed.

```bash
curl -s --insecure -H "api-token: $token_val" https://a2-dev.test/api/v0/nodes/bulk-create -d '
{"nodes": [{
     "name_prefix": "my-ssh-node",
     "manager":"automate",
     "target_config": {
        "backend":"ssh",
        "hosts":["localhost","127.0.0.1"],
        "secrets":["2998c3a1-d596-43d4-b2b3-4837a46cee19"],
        "port": 22
      },
      "tags": [
        { "key":"test-node", "value":"is-amazing" },
        { "key":"compliance-service", "value":"rockin-like-whoa" },
        { "key":"_no_auto_detect", "value":"true" }
      ]
    },
    {
     "name": "my-other-node",
     "manager":"automate",
      "target_config": {
        "backend":"ssh",
        "hosts":["localhost"],
        "secrets":["2998c3a1-d596-43d4-b2b3-4837a46cee19"],
        "port": 22
      },
      "tags": [
        { "key":"test-node", "value":"is-more-amazing" }
      ]
    }
  ]
}'
```

### Bulk Node Delete

 The `/nodes/delete` endpoint allows users to bulk-delete nodes based on a query. To examine the outcome of this destructive action before running it, test the query first on the `api/v0/nodes/search` endpoint.

```bash
curl -s --insecure -H "api-token: $token_val"
https://a2-dev.test/api/v0/nodes/delete -d '{
  "filters": [
    {"key": "name", "values": ["vj*"]}
  ]
}'
```
