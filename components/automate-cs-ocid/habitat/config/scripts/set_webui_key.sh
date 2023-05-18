WEBUI_SRC_PATH="/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem"
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' $WEBUI_SRC_PATH`
if [[ -z $WEBUI_KEY ]]
  then
    echo "Could not find the webui key in erchef service. Pls wait for erchef to be running before OCID can be started..."
    exit 1
fi
export PRIVATE_CHEF_SECRETS_PATH="$OCID_CONFIG_FOLDER_PATH/private-chef-secrets.json"
ruby "{{pkg.svc_config_path}}/tasks/set_webui_key.rb"
