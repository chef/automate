#!/bin/bash

# prompt the user to input the number of organisations to create
echo "Enter Chef Server Url"
read SERVER_URL

echo "Enter the number of organisations to create"
read NUM_ORGS

echo "Enter the number of users to create per organisation"
read NUM_USERS

echo "Enter the number of environment to create per organisation"
read NUM_ENVS

echo "Enter the number of nodes to create per organisation"
read NUM_NODES

FLAG_VALUE="--key ~/.chef/pivotal.pem -u pivotal --config-option ssl_verify_mode=verify_none --config-option verify_api_cert=false"

# loop through and create the directories
for i in $(seq 1 $NUM_ORGS)
do
  knife org create "org$i" "org$i" -f ~/.chef/"org$i.pem" -s $SERVER_URL $FLAG_VALUE

  # loop through to create and associate user in the organisation
  for j in $(seq 1 $NUM_USERS)
  do
    # create and associate the user
    knife user create -u "user-$j-of-org-$i" --first-name "user $j" "user-$j-of-org-$i" --email "user-$j-of-org-$i@example.com" --password password -o "org$i" -f ~/.chef/"user$j.pem" -s $SERVER_URL $FLAG_VALUE
  done

  # loop through to create environment per organisation
  for k in $(seq 1 $NUM_ENVS)
  do
    json_data="{ \"name\": \"env-$k\", \"default_attributes\": {}, \"json_class\": \"Chef::Environment\", \"description\": \"env-$k-description\", \"cookbook_versions\": {}, \"chef_type\": \"environment\" }"
     echo $json_data > environment.json

     knife raw -s $SERVER_URL -m POST /organizations/"org$i"/environments -i environment.json $FLAG_VALUE
     echo "Environment created env-$k"
     for l in $(seq 1 $NUM_NODES)
     do
         json_data="{ \"name\": \"node-$l-for-env-$k-of-org-$i\", \"chef_environment\": \"env-$k\", \"chef_type\": \"node\", \"json_class\": \"Chef::Node\", \"override\": {}, \"default\": {}, \"run_list\": [ \"recipe[unicorn]\" ] }"

         echo $json_data > node.json
         knife raw -s $SERVER_URL -m POST /organizations/"org$i"/nodes -i node.json $FLAG_VALUE
         knife node policy set "node-$l-for-env-$k-of-org-$i" 'test-policy-group' 'test-policy' --config-option chef_server_url=$SERVER_URL/organizations/"org$i"
     done
  done
done

cd /home/ubuntu/cookbook

#Find all files in the directory and loop through them
# loop through and create the directories
for i in $(seq 1 $NUM_ORGS)
do
  for file in *; 
  do
    # Extract the contents of the tar file
    knife upload $file -s $SERVER_URL/organizations/"org$i" --config-option cookbook_path=/home/ubuntu/cookbook
  done
done

echo "Done"