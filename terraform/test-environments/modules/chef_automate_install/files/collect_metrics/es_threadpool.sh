#!/bin/bash

set -eo pipefail

command=`curl -s -XGET "http://localhost:10141/_nodes/_local/stats/thread_pool" | jq '(.. | .thread_pool?.index.threads | numbers) , (.. | .thread_pool?.index.queue | numbers) , (.. | .thread_pool?.index.active | numbers) , (.. | .thread_pool?.index.rejected | numbers) , (.. | .thread_pool?.index.largest | numbers) , (.. | .thread_pool?.index.completed | numbers)'`

mapfile -t es_metrics <<<"$command"

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexThreads --unit Bytes --value ${es_metrics[0]}
aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexQueue --unit Percent --value ${es_metrics[1]}
aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexActive --unit Count --value ${es_metrics[2]}
aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexRejected --unit Milliseconds --value ${es_metrics[3]}
aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexLargest --unit Count --value ${es_metrics[4]}
aws cloudwatch put-metric-data $per_host_options --metric-name ThreadpoolIndexCompleted --unit Milliseconds --value ${es_metrics[5]}
