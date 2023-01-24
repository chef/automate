#!/usr/bin/env bash

echo $1
echo $2
if hab pkg exec core/aws-cli aws s3 ls s3://$1; then
  echo "Bucket already exists"
else
  export BUCKET_CREATION=$(hab pkg exec core/aws-cli aws s3api create-bucket --bucket $1  --acl private --create-bucket-configuration LocationConstraint=$2)
  echo "$BUCKET_CREATION"
  if echo "$BUCKET_CREATION" | grep -q "http://$1.s3.amazonaws.com/"; then
    echo "Bucket is created"
  else
    echo "Error in creating bucket"
  fi
fi
