#!/bin/bash

set -eo pipefail

command=`curl -s -XGET "http://localhost:10141/_nodes/_local/stats/jvm" | jq '(.. | .jvm?.mem.heap_used_in_bytes | numbers ) , (.. | .jvm?.mem.heap_used_percent | numbers ) , (.. | .jvm?.gc.collectors.old.collection_count | numbers ) , (.. | .jvm?.gc.collectors.old.collection_time_in_millis | numbers ) , (.. | .jvm?.gc.collectors.young.collection_count | numbers ) , (.. | .jvm?.gc.collectors.young.collection_time_in_millis | numbers )'`

mapfile -t es_metrics <<<"$command"

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

aws cloudwatch put-metric-data $per_host_options --metric-name HeapUsedInBytes --unit Bytes --value ${es_metrics[0]}
aws cloudwatch put-metric-data $per_host_options --metric-name HeapUsedPercent --unit Percent --value ${es_metrics[1]}
aws cloudwatch put-metric-data $per_host_options --metric-name GCOldCollectionCount --unit Count --value ${es_metrics[2]}
aws cloudwatch put-metric-data $per_host_options --metric-name GCOldCollectionTimeInMillis --unit Milliseconds --value ${es_metrics[3]}
aws cloudwatch put-metric-data $per_host_options --metric-name GCYoungCollectionCount --unit Count --value ${es_metrics[4]}
aws cloudwatch put-metric-data $per_host_options --metric-name GCYoungCollectionTimeInMillis --unit Milliseconds --value ${es_metrics[5]}
