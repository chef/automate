{
  "widgets": [
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "DataCollector HTTP Codes",
              "metrics": [
                  [ "${cloudwatch_namespace}", "2xxStatuses" ],
                  [ ".", "5xxStatuses" ],
                  [ ".", "3xxStatuses" ],
                  [ ".", "4xxStatuses" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "DataCollector POST Time",
              "metrics": [
                  [ "${cloudwatch_namespace}", "1MinuteAveragePostRequestTime" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES Document Ingest Time",
              "metrics": [
                  [ "${cloudwatch_namespace}", "1MinuteAverageESDocInsertTime" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "Pipeline Ingest Time",
              "metrics": [
                  [ "${cloudwatch_namespace}", "1MinuteAverageIngestChefRunPipelineTime" ],
                  [ ".", "1MinuteAverageIngestChefActionPipelineTime" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": true,
              "region": "${aws_region}",
              "title": "ES Ingested Docs/Minute",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ActionsRecordsPerMinute" ],
                  [ ".", "ComplianceRecordsPerMinute" ],
                  [ ".", "ConvergeRecordsPerMinute" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": true,
              "region": "${aws_region}",
              "title": "ES Docs Count",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ActionsRecords" ],
                  [ ".", "ComplianceRecords" ],
                  [ ".", "ConvergeRecords" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": true,
              "region": "${aws_region}",
              "title": "Lucene Docs Count",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ActionsLuceneRecords" ],
                  [ ".", "ComplianceLuceneRecords" ],
                  [ ".", "ConvergeLuceneRecords" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": true,
              "region": "${aws_region}",
              "title": "ES Store Size",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ActionsIndexBytes" ],
                  [ ".", "ComplianceIndexBytes" ],
                  [ ".", "ConvergeIndexBytes" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES Heap Used Percent",
              "metrics": [
                  [ "${cloudwatch_namespace}", "HeapUsedPercent" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES Heap Used",
              "metrics": [
                  [ "${cloudwatch_namespace}", "HeapUsedInBytes" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES GC Young Collection Time",
              "metrics": [
                  [ "${cloudwatch_namespace}", "GCYoungCollectionTimeInMillis" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES GC Young Collection Count",
              "metrics": [
                  [ "${cloudwatch_namespace}", "GCYoungCollectionCount" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES Disk Queue Size",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ESDiskQueueSize" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": true,
              "region": "${aws_region}",
              "title": "Gateway Legacy Events",
              "metrics": [
                  [ "${cloudwatch_namespace}", "SuccessIngestedMessages" ],
                  [ ".", "FailedIngestMessages" ],
                  [ ".", "UnsupportedIngestMessages" ],
                  [ ".", "UnknownIngestMessages" ]
              ]
          }
      },
      {
          "type": "metric",
          "properties": {
              "view": "timeSeries",
              "stacked": false,
              "region": "${aws_region}",
              "title": "ES Shard Health",
              "metrics": [
                  [ "${cloudwatch_namespace}", "ClusterActiveShards" ],
                  [ ".", "ClusterInitializingShards" ],
                  [ ".", "ClusterRelocatingShards" ],
                  [ ".", "ClusterDelayedUnassignedShards" ],
                  [ ".", "ClusterUnassignedShards" ],
                  [ ".", "ClusterNumberOfPendingTasks" ],
                  [ ".", "ClusterActivePrimaryShards" ]
              ]
          }
      }
  ]
}