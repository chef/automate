#!/usr/bin/env bash

set -euo pipefail

get_route53_hosted_zone_id() {
  local dns_name
  dns_name="$1"

  aws --profile chef-cd route53 list-hosted-zones-by-name --output text --dns-name "$dns_name" --query "HostedZones[?Name == '$dns_name'].[Id]"
}

get_a2_route53_records() {
  hosted_zone_id="$(get_route53_hosted_zone_id "cd.chef.co.")"

  aws --profile chef-cd route53 list-resource-record-sets --hosted-zone-id "$hosted_zone_id" --output text --query "ResourceRecordSets[?starts_with(@.Name, 'a2-')].[ResourceRecords[]|[0].Value, Name]"
}

terminate_abandoned_a2_instances() {
  while read -r ip record_name; do
    hostname="${record_name%.}"

    date="$(aws --profile chef-cd ec2 describe-instances --output text --no-paginate --query "Reservations[?Instances[?(Tags[?Key == 'Name'].Value|[0] == '$hostname') && (PrivateIpAddress == '$ip')]].[Instances|[0].LaunchTime]")"

    get_a2_hostname_instances_launched_before_date "$date" | terminate_amazon_instances
  done
}

get_a2_hostname_instances_launched_before_date() {
  aws --profile chef-cd ec2 describe-instances --output text --no-paginate --query "Reservations[?Instances[?(Tags[?Key == 'Name'].Value|[0] == '$hostname') && (LaunchTime < '$date')]].[Instances|[0].InstanceId]"
}

terminate_amazon_instances() {
  while read -r instance_id; do
    terminate_amazon_instance "$instance_id"
  done
}

terminate_amazon_instance() {
  local instance_id
  instance_id="$1"

  if aws --profile chef-cd ec2 terminate-instances --instance-ids "$instance_id"; then
    echo "Terminated amazon instance $instance_id"
  else
    echo "Failed to terminate amazon instance $instance_id"
  fi
}

get_a2_route53_records | terminate_abandoned_a2_instances
