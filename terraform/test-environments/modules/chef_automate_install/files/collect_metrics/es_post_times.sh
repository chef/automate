#!/bin/bash

set -eo pipefail

# This script is intended to run every minute from cron.

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

LOGS_LAST_MIN=$(journalctl --since "1 minute ago" -u chef-automate)

# Count HTTP codes
data_collector_logs=$(echo "$LOGS_LAST_MIN" | { grep data-collector || true; } | { grep POST || true; })
COUNT_2XX=`echo "$data_collector_logs" | awk '{print $13}' | { grep -c "^2" || true; }`
COUNT_3XX=`echo "$data_collector_logs" | awk '{print $13}' | { grep -c "^3" || true; }`
COUNT_4XX=`echo "$data_collector_logs" | awk '{print $13}' | { grep -c "^4" || true; }`
COUNT_5XX=`echo "$data_collector_logs" | awk '{print $13}' | { grep -c "^5" || true; }`

aws cloudwatch put-metric-data $per_host_options --metric-name 2xxStatuses --unit Count --value ${COUNT_2XX}
aws cloudwatch put-metric-data $per_host_options --metric-name 3xxStatuses --unit Count --value ${COUNT_3XX}
aws cloudwatch put-metric-data $per_host_options --metric-name 4xxStatuses --unit Count --value ${COUNT_4XX}
aws cloudwatch put-metric-data $per_host_options --metric-name 5xxStatuses --unit Count --value ${COUNT_5XX}

# Get average DataCollector POST times in seconds
#
# Parsing automate-load-balancer log:
# automate-load-balancer.default(O): - [09/Mar/2018:20:11:22 +0000]  "POST /data-collector/v0/ HTTP/1.1" 200 "7.735" 2 "-" "Go-http-client/1.1" "10.42.5.165:2000" "200" "7.734" 45250
#
DC_POST_REQUEST_TIME=$(echo "$data_collector_logs" | awk '{gsub(/\"/, "")} {sum+=$(NF-7)} END {print sum/NR}')
aws cloudwatch put-metric-data $per_host_options --metric-name 1MinuteAveragePostRequestTime --unit Seconds --value ${DC_POST_REQUEST_TIME}

# Get the number of RPC Calls received through the legacy-endpoint (by the Gateway)
GATEWAY_LEGACY_RPC_CALLS=$(echo "$LOGS_LAST_MIN" | { grep automate-gateway || true; } | { grep "rpc call" || true; } | { grep -c ProcessLegacyEvent || true; })
aws cloudwatch put-metric-data $per_host_options --metric-name LegacyEvents --unit Count --value ${GATEWAY_LEGACY_RPC_CALLS}

# Metrics
LOGS_METRICS=$(echo "$LOGS_LAST_MIN" | { grep metric || true; })
LOGS_INGEST_TIME=$(echo "$LOGS_METRICS" | { grep type=ingest_time || true; })
LOGS_ES_DOC_INSERT_TIME=$(echo "$LOGS_METRICS" | { grep type=doc_insert || true; })

# Get the number of ChefRun messages ingested and failed (by the Ingest pipeline)
INGESTED_MESSAGES=$(echo "$LOGS_INGEST_TIME" | { grep -c "Message ingested successfully" || true; })
FAILED_MESSAGES=$(echo "$LOGS_INGEST_TIME" | { grep -c "Unable to ingest message" || true; })
# TODO: Add official "Unsupported Messages"
# @afiune: Currently we are not ingesting the 'run_start' messages, fix this when we DO ingest them
UNSUPPORTED_MESSAGES=$(echo "$LOGS_LAST_MIN" | { grep message_type || true; } | { grep -c "Unsupported message" || true; })
UNKNOWN_MESSAGES=$(expr $GATEWAY_LEGACY_RPC_CALLS - $INGESTED_MESSAGES - $UNSUPPORTED_MESSAGES - $FAILED_MESSAGES)
aws cloudwatch put-metric-data $per_host_options --metric-name SuccessIngestedMessages --unit Count --value ${INGESTED_MESSAGES}
aws cloudwatch put-metric-data $per_host_options --metric-name FailedIngestMessages --unit Count --value ${FAILED_MESSAGES}
aws cloudwatch put-metric-data $per_host_options --metric-name UnsupportedIngestMessages --unit Count --value ${UNSUPPORTED_MESSAGES}
aws cloudwatch put-metric-data $per_host_options --metric-name UnknownIngestMessages --unit Count --value ${UNKNOWN_MESSAGES}

# Average Ingest Pipeline time (How long does a message goes through the ingest pipeline?)
INGEST_PIPELINE_TIME_CHEF_ACTION=$(echo "$LOGS_INGEST_TIME" | { grep message=ChefAction || true; } | { grep "Message ingested successfully" || true; } | awk '{gsub(/ms/, "")} {print $(NF-1)}' |cut -d= -f2 | awk '{sum+=$1} END {print sum/NR}')
INGEST_PIPELINE_TIME_CHEF_RUN=$(echo "$LOGS_INGEST_TIME" | { grep message=ChefRun || true; } | { grep "Message ingested successfully" || true; } | awk '{gsub(/ms/, "")} {print $(NF-1)}' |cut -d= -f2 | awk '{sum+=$1} END {print sum/NR}')
aws cloudwatch put-metric-data $per_host_options --metric-name 1MinuteAverageIngestChefActionPipelineTime --unit Milliseconds --value ${INGEST_PIPELINE_TIME_CHEF_ACTION}
aws cloudwatch put-metric-data $per_host_options --metric-name 1MinuteAverageIngestChefRunPipelineTime --unit Milliseconds --value ${INGEST_PIPELINE_TIME_CHEF_RUN}

# Average ES insertion time (How long does ES takes to insert documents?)
ES_DOC_INSERT_TIME=$(echo "$LOGS_ES_DOC_INSERT_TIME" | awk '{gsub(/ms/, "")} {print $(NF-1)}' |cut -d= -f2 | awk '{sum+=$1} END {print sum/NR}')
aws cloudwatch put-metric-data $per_host_options --metric-name 1MinuteAverageESDocInsertTime --unit Milliseconds --value ${ES_DOC_INSERT_TIME}
