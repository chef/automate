#!/bin/bash -e

which -s grpcurl || (echo "grpcurl is not installed, aborting..." && exit 1)
which -s jq || (echo "js is not installed, aborting..." && exit 2)

COMP_PORT=${PORT:-10121}
MANAGER_PORT=${MANAGER_PORT:-10120}
HOST=${HOST:-localhost}
SSL_CERT="$(PWD)/../../../dev/certs/compliance-service.crt"
SSL_KEY="$(PWD)/../../../dev/certs/compliance-service.key"
CURL_COMP="--insecure -cert $SSL_CERT -key $SSL_KEY $HOST:$COMP_PORT"
CURL_OPTS_COMP="--insecure -cert $SSL_CERT -key $SSL_KEY $HOST:$COMP_PORT chef.automate.domain.compliance"
CURL_OPTS_MANAGER="--insecure -cert $SSL_CERT -key $SSL_KEY $HOST:$MANAGER_PORT chef.automate.domain.nodemanager"
RAND_NR=$((RANDOM%100))

# grpcurl $CURL_COMP list
# See all the Methods of a Service
# grpcurl $CURL_COMP describe chef.automate.domain.compliance.jobs.JobsService
# See the Job message struct
# grpcurl $CURL_COMP describe chef.automate.domain.compliance.jobs.Job
grpcurl $CURL_COMP describe chef.automate.domain.compliance.reporting.ReportingService.ListReports

echo && echo "Compliance Service version:"
grpcurl $CURL_OPTS_COMP.version.VersionService/Version

NODE_NAME="curl_it docker node $RAND_NR"
echo && echo "Adding docker node '$NODE_NAME'"
NODE_ID=`grpcurl -d @ $CURL_OPTS_MANAGER.nodes.NodesService/Create <<EOM | jq -r .id
{
  "name": "$NODE_NAME",
  "manager": "automate",
  "target_config": {
    "backend": "docker",
    "host": "cc_pg",
    "sudo": false
  }
}
EOM
`

echo && echo "Adding a detect job for the new node '$NODE_NAME' with id '$NODE_ID'"
grpcurl -d @ $CURL_OPTS_COMP.jobs.JobsService/Create <<EOM
{
  "name": "curl_it docker node detect job $RAND_NR",
  "type": "detect",
  "nodes": ["$NODE_ID"],
  "retries": 1
}
EOM

echo && echo "Adding an exec job for the new node '$NODE_NAME' with id '$NODE_ID'"
grpcurl -d @ $CURL_OPTS_COMP.jobs.JobsService/Create <<EOM
{
  "name": "curl_it docker node exec job $RAND_NR",
  "type": "exec",
  "nodes": ["$NODE_ID"],
  "profiles": ["https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"],
  "retries": 1
}
EOM


# List all nodes
# grpcurl $CURL_OPTS_MANAGER.nodes.NodesService/List
# Get one report for a specific node
REPORT_ID=`grpcurl -d @ $CURL_OPTS_COMP.reporting.ReportingService/ListReports <<EOM  | jq -r .reports[0].id
{
  "id": "$NODE_ID"
}
EOM
`

if [ -n "$REPORT_ID" ]; then
  echo && echo "Showing the first 5 lines of report '$REPORT_ID' ..."
  grpcurl -d @ $CURL_OPTS_COMP.reporting.ReportingService/ReadReport <<EOM | head -5
  {
    "id": "$REPORT_ID"
  }
EOM
else
  echo "Node '$NODE_ID' has no complete reports to show..."
fi

# List all jobs
# grpcurl $CURL_OPTS_COMP.jobs.JobsService/List

# List all reporting profiles for a specific date
# grpcurl -d @ $CURL_OPTS_COMP.reporting.ReportingService/ListProfiles <<EOM
# {
#   "filters": [
#     { "type": "end_time", "values":["2020-06-20T23:59:59Z"] }
#   ],
#   "page": 1,
#   "per_page": 100
# }
# EOM
