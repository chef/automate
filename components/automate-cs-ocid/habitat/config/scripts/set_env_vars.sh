export ENV_CONFIG_FILE_PATH="$OCID_PKG_CONFIG_FOLDER_PATH/settings/$RAILS_ENV.yml"
export CHEF_SERVER_ENDPOINT="{{cfg.ocid.chef_server_config.endpoint}}"
export CHEF_SERVER_SUPERUSER="{{cfg.ocid.chef_server_config.superuser}}"
export CHEF_SERVER_SSL_VERIFY_MODE="{{cfg.ocid.chef_server_config.ssl_verify_mode}}"

ruby "{{pkg.svc_config_path}}/tasks/set_env_vars.rb"
