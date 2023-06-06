# Copying this file into oc_id application directory so that this file
# can be executed as a rake task in the later stage of the application lifecycle.
cp "{{pkg.svc_config_path}}/tasks/oauth_application.rake" $(hab pkg path "chef/oc_id")/oc_id/lib/tasks

cd "$(hab pkg path 'chef/oc_id')/oc_id"

# export BUNDLE_SILENCE_ROOT_WARNING=1 GEM_PATH
# build_line "Setting BUNDLE_SILENCE_ROOT_WARNING=$BUNDLE_SILENCE_ROOT_WARNING"

bundle config path "vendor/bundle"

# TODO :: Remove following line once new ocid hab package is used
echo "gem 'tzinfo-data'" >> Gemfile
bundle install

# tmp directory is required for storage of sessions
mkdir -p tmp && chmod 777 -R tmp
