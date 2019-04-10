#!/bin/bash

set -eo pipefail

cluster_health=`curl -s http://localhost:10141/_cluster/health | jq '.number_of_nodes, .active_primary_shards, .active_shards, .relocating_shards, .unassigned_shards, .delayed_unassigned_shards, .number_of_pending_tasks, .number_of_in_flight_fetch, .task_max_waiting_in_queue_millis, .initializing_shards, .active_shards_percent_as_number'`

es_metrics[0]=`echo $cluster_health | awk '{print $1}'`
es_metrics[1]=`echo $cluster_health | awk '{print $2}'`
es_metrics[2]=`echo $cluster_health | awk '{print $3}'`
es_metrics[3]=`echo $cluster_health | awk '{print $4}'`
es_metrics[4]=`echo $cluster_health | awk '{print $5}'`
es_metrics[5]=`echo $cluster_health | awk '{print $6}'`
es_metrics[6]=`echo $cluster_health | awk '{print $7}'`
es_metrics[7]=`echo $cluster_health | awk '{print $8}'`
es_metrics[8]=`echo $cluster_health | awk '{print $9}'`
es_metrics[9]=`echo $cluster_health | awk '{print $10}'`
es_metrics[10]=`echo $cluster_health | awk '{print $11}'`

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

aws cloudwatch put-metric-data $per_host_options --metric-name ClusterNumberOfNodes --unit Count --value ${es_metrics[0]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterActivePrimaryShards --unit Count --value ${es_metrics[1]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterActiveShards --unit Count --value ${es_metrics[2]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterRelocatingShards --unit Count --value ${es_metrics[3]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterUnassignedShards --unit Count --value ${es_metrics[4]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterDelayedUnassignedShards --unit Count --value ${es_metrics[5]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterNumberOfPendingTasks --unit Count --value ${es_metrics[6]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterNumberOfInFlightFetch --unit Count --value ${es_metrics[7]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterTaskMaxWaitingInQueueMillis --unit Milliseconds --value ${es_metrics[8]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterInitializingShards --unit Count --value ${es_metrics[9]}
aws cloudwatch put-metric-data $per_host_options --metric-name ClusterActiveShardsPercentAsNumber --unit Count --value ${es_metrics[10]}
