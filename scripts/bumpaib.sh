#!/bin/bash

TFVARS=$1

if [ -f "${TFVARS}" ]; then
  echo "Updating ${TFVARS}"
  sed -i -e "s/^\(.*end_aib_dest_file.*\)\.aib\"/\1_.aib\"/" "${TFVARS}"
  echo "New values:"
  grep "end_aib_dest_file" "${TFVARS}"
  echo "Now run the apply from the terraform directory!"
else
  echo "'${TFVARS}' doesn't exist! Pass in the _full_ path to the terraform.tfvars file"
fi
