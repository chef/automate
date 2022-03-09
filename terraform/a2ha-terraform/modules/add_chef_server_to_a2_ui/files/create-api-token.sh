#!/bin/bash

function create_token() {
  timestamp=$(date +%s)
  sudo chef-automate iam token create admin.$timestamp --admin > /var/tmp/token
  sudo touch /var/tmp/token-generated.Done
  sudo touch /var/tmp/chef_server_list
}

if [ -e /var/tmp/token-generated.Done ]
then
   echo "This is an upgrade scenario, where token is already generated"
else
   create_token 
fi
