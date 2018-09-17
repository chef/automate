+++
title = "Elasticsearch"
description = "Using an External Elasticsearch Cluster"
draft = true
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "configuring_automate"
    weight = 30
+++

Migrating Standalone Automate 2 to an External Elasticsearch Cluster

Setup your external cluster
Install Elasticsearch on all of your cluster members using version 6.2.2, with the cluster_name set to "chef-insights". Elastic provides instructions for configuring Elasticsearch here https://www.elastic.co/guide/en/elasticsearch/reference/6.2/_installation.html.
We will need the IP address of each node for the later steps. In my example it’s a 3 node cluster and the IPs are 10.1.1.11, 10.1.1.12, 10.1.1.13.

You can test that it’s working correctly like so
```
curl 10.1.1.11:9200/_cluster/health?pretty
{
  "cluster_name" : "chef-insights",
  "status" : "green",
  "timed_out" : false,
  "number_of_nodes" : 3,
  "number_of_data_nodes" : 3,
  "active_primary_shards" : 0,
  "active_shards" : 0,
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 0,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 100.0
}
```
You want to make sure status is green, and number_of_nodes is however many nodes you stood up.






2. Join A2 to your External Cluster.
Create a toml file that contains the settings for A2s internal Elasticsearch to join your cluster.
In this example I’ll name the file “join_cluster.toml”
```
[elasticsearch.v1.sys.node]
  master = true
  data = true
[elasticsearch.v1.sys.network]
  host = "0.0.0.0"
[elasticsearch.v1.sys.transport]
  port = "9300-9400"
[elasticsearch.v1.sys.discovery]
  ping_unicast_hosts = "[10.1.1.11, 10.1.1.12, 10.1.1.13]"
```
Apply the config with the config patch command. This will cause A2 to restart the built in Elasticsearch.
```
chef-automate config patch join_cluster.toml
```

```
Updating deployment configuration

Applying deployment configuration
  Configured automate-elasticsearch
Success: Configuration patched
```
You can check that this worked by running this command on your A2 server.

```
curl localhost:10141/_cluster/health?pretty
```

```
{
  "cluster_name" : "chef-insights",
  "status" : "green",
  "timed_out" : false,
  "number_of_nodes" : 4,
  "number_of_data_nodes" : 4,
  "active_primary_shards" : 355,
  "active_shards" : 710,
  "relocating_shards" : 2,
  "initializing_shards" : 0,
  "unassigned_shards" : 0,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 100.0
}
```
You want to make sure the health is Green. You will notice that it is relocating 2 shards. Currently Elasticsearch is trying to rebalance data equally between your 3 ES nodes and the built in ES in Automate. You do not need to wait for this to finish before moving to the next step.

3. Move all data out of Automate’s built in Elasticsearch.


Use curl to change Elasticsearch’s configuration so that it moves all data out to your ES cluster nodes. In this example there are three things you will need to consider.

cluster.routing.allocation.exclude._ip: This needs to be the IP address your Automate server uses to communicate with the elasticsearch servers.
When in doubt this command will tell you which IP is being used
`curl -s localhost:10141/_nodes/_local?pretty | grep transport_address`

cluster.routing.allocation.cluster_concurrent_rebalance: The number of shards to move at a time. The default is 2, we’re planning to do 10 at a time to speed things along.

Indices.recovery.max_bytes_per_sec: We set the limit to 80 MB/s, which is a bit over half of a 1Gbps link. Feel free to tweak this based on how much bandwidth you have. The default is 40.

```
curl -XPUT localhost:10141/_cluster/settings -H 'Content-Type: application/json' -d '{
"transient": {
 "cluster.routing.allocation.exclude._ip": "10.1.1.10",
 "cluster.routing.allocation.cluster_concurrent_rebalance": "10",
 "indices.recovery.max_bytes_per_sec": "80mb"
}
}'
```


```
{"acknowledged":true,"persistent":{},"transient":{"cluster":{"routing":{"allocation":{"cluster_concurrent_rebalance":"10","exclude":{"_ip":"10.1.1.10"}}}},"indices":{"recovery":{"max_bytes_per_sec":"80mb"}}}}
```

To check on progress, get node name of built in ES with
```
curl -s localhost:10141/_nodes/_local?pretty | grep \"name\" | head -1
```
Then when you have the uniq name
```
watch 'curl -s localhost:10141/_cat/shards | grep 7gPK9oY | sort'
```
When everything has moved that should return nothing.


4. Set Final Configuration for Automate’s Built in Elasticsearch.


The last step will be to configure Elasticsearch to be a coordinating only node, similar to how Elastic recommends for Kibana here. https://www.elastic.co/guide/en/kibana/6.2/production.html

Before running this step verify again that all data has been moved off of A2s built in Elasticsearch.
```
curl localhost:10141/_cluster/health?pretty
```
Make sure that health is Green and the following fields are 0.
````
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 0,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
```

Update your “join_cluster.toml” from earlier and change master and data to “false”.

```
[elasticsearch.v1.sys.node]
  master = false
  data = false
[elasticsearch.v1.sys.network]
  host = "0.0.0.0"
[elasticsearch.v1.sys.transport]
  port = "9300-9400"
[elasticsearch.v1.sys.discovery]
  ping_unicast_hosts = "[10.1.1.11, 10.1.1.12, 10.1.1.13]"
```
Apply the patch the same way as before,

```
chef-automate config patch join_cluster.toml
```

```
Updating deployment configuration

Applying deployment configuration
  Configured automate-elasticsearch
Success: Configuration patched
```

At this point you should be finished. Verify that Elasticsearch is in a good state.
```
curl localhost:10141/_cluster/health?pretty
```

```
{
  "cluster_name" : "chef-insights",
  "status" : "green",
  "timed_out" : false,
  "number_of_nodes" : 4,
  "number_of_data_nodes" : 3,
  "active_primary_shards" : 355,
  "active_shards" : 710,
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 0,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 100.0
}
```
number_of_data_nodes should equal the number of nodes in your external cluster.
number_of_nodes should be equal number of ES nodes + your Automate server.

If everything looks good, Congratulations your done.
