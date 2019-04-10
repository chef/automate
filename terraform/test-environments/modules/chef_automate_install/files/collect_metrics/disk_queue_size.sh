#!/bin/bash

set -eo pipefail

root_device=$(findmnt --noheadings --output SOURCE --target /hab)

root_parent_device=$(lsblk --noheadings --output PKNAME "$root_device")

command=`iostat -y -x -d /dev/"$root_parent_device" 1 1 | awk 'NR==4 {print $9}'`

per_host_options="--namespace A2_Performance_Test_dev --region us-west-2"

aws cloudwatch put-metric-data $per_host_options --metric-name ESDiskQueueSize --unit Count --value ${command}
