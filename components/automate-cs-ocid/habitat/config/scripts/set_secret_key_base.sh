export WEBUI_SRC_PATH="/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem"
export SECRET_KEY_BASE_FILE_PATH="$OCID_PKG_CONFIG_FOLDER_PATH/secret_key_base.txt"
export CUSTOM_SECRET_KEY_BASE="{{cfg.ocid.secret_key_base}}"

ruby "{{pkg.svc_config_path}}/tasks/save_secret_key_base_file.rb"

# Using `2>/dev/null` along with `cat` command suppresses the error
# in case file doesn't exist and it returns empty string.
export SECRET_KEY_BASE=`cat $SECRET_KEY_BASE_FILE_PATH 2>/dev/null`
echo "OCID secret_key_base is set."
