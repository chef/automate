#!/bin/bash

set -eo pipefail

actions_records=$(curl -s http://localhost:10141/actions-20*/_count | jq '.count // 0')
actions_lucene_records=$(curl -s http://localhost:10141/actions-20*/_stats/docs | jq '._all.total.docs.count // 0')
actions_index_bytes=$(curl -s http://localhost:10141/actions-20*/_stats/store | jq '._all.total.store.size_in_bytes // 0')

compliance_records=$(curl -s http://localhost:10141/comp-*/_count | jq '.count // 0')
compliance_lucene_records=$(curl -s http://localhost:10141/comp-*/_stats/docs | jq '._all.total.docs.count // 0')
compliance_index_bytes=$(curl -s http://localhost:10141/comp-*/_stats/store | jq '._all.total.store.size_in_bytes // 0')

converge_records=$(curl -s http://localhost:10141/converge-history-20*/_count | jq '.count // 0')
converge_lucene_records=$(curl -s http://localhost:10141/converge-history-20*/_stats/docs | jq '._all.total.docs.count // 0')
converge_index_bytes=$(curl -s http://localhost:10141/converge-history-20*/_stats/store | jq '._all.total.store.size_in_bytes // 0')

curtime=`date +%s`
actions_index="actions"
compliance_index="compliance"
converge_index="converge"
actions_records_per_minute=`/opt/collect_metrics/index_records.py $actions_index $curtime $actions_records`
compliance_records_per_minute=`/opt/collect_metrics/index_records.py $compliance_index $curtime $compliance_records`
converge_records_per_minute=`/opt/collect_metrics/index_records.py $converge_index $curtime $converge_records`

actions_records_per_minute_int=`echo $actions_records_per_minute | awk '{split($0,a,/\./)} END {print a[1]}'`
compliance_records_per_minute_int=`echo $compliance_records_per_minute | awk '{split($0,a,/\./)} END {print a[1]}'`
converge_records_per_minute_int=`echo $converge_records_per_minute | awk '{split($0,a,/\./)} END {print a[1]}'`

if [ $actions_records_per_minute_int -lt 0 ]; then
  actions_records_per_minute=0
fi

if [ $compliance_records_per_minute_int -lt 0 ]; then
  compliance_records_per_minute=0
fi

if [ $converge_records_per_minute_int -lt 0 ]; then
  converge_records_per_minute=0
fi

es_metrics[0]=$actions_records
es_metrics[1]=$actions_lucene_records
es_metrics[2]=$actions_index_bytes
es_metrics[3]=$actions_records_per_minute
es_metrics[4]=$compliance_records
es_metrics[5]=$compliance_lucene_records
es_metrics[6]=$compliance_index_bytes
es_metrics[7]=$compliance_records_per_minute
es_metrics[8]=$converge_records
es_metrics[9]=$converge_lucene_records
es_metrics[10]=$converge_index_bytes
es_metrics[11]=$converge_records_per_minute

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

aws cloudwatch put-metric-data $per_host_options --metric-name ActionsRecords --unit Count --value ${es_metrics[0]}
aws cloudwatch put-metric-data $per_host_options --metric-name ActionsLuceneRecords --unit Count --value ${es_metrics[1]}
aws cloudwatch put-metric-data $per_host_options --metric-name ActionsIndexBytes --unit Bytes --value ${es_metrics[2]}
aws cloudwatch put-metric-data $per_host_options --metric-name ActionsRecordsPerMinute --unit Count --value ${es_metrics[3]}
aws cloudwatch put-metric-data $per_host_options --metric-name ComplianceRecords --unit Count --value ${es_metrics[4]}
aws cloudwatch put-metric-data $per_host_options --metric-name ComplianceLuceneRecords --unit Count --value ${es_metrics[5]}
aws cloudwatch put-metric-data $per_host_options --metric-name ComplianceIndexBytes --unit Bytes --value ${es_metrics[6]}
aws cloudwatch put-metric-data $per_host_options --metric-name ComplianceRecordsPerMinute --unit Count --value ${es_metrics[7]}
aws cloudwatch put-metric-data $per_host_options --metric-name ConvergeRecords --unit Count --value ${es_metrics[8]}
aws cloudwatch put-metric-data $per_host_options --metric-name ConvergeLuceneRecords --unit Count --value ${es_metrics[9]}
aws cloudwatch put-metric-data $per_host_options --metric-name ConvergeIndexBytes --unit Bytes --value ${es_metrics[10]}
aws cloudwatch put-metric-data $per_host_options --metric-name ConvergeRecordsPerMinute --unit Count --value ${es_metrics[11]}
