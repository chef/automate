#!/bin/bash

if [ "$LOGS_URL" != "" ]; then
  if aws s3api head-bucket --profile chef-cd --bucket $DASHBOARD_URL &>/dev/null; then
    echo "$LOGS_URL" | aws --profile chef-cd s3 cp - s3://$DASHBOARD_URL/assets/deploy_logs
  else
    echo "The $DASHBOARD_URL has not been created yet -- skipping the deploy marker"
  fi
fi
