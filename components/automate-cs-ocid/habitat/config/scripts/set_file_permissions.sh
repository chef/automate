# Giving read+write permission to hab user
# This is required so that 'db:migrate' can be executed from the run hook
chmod 746 "$(hab pkg path 'chef/oc_id')/oc_id/db/schema.rb"
chmod 746 "$(hab pkg path 'chef/oc_id')/oc_id/config/private-chef-secrets.json"

# Giving read+write+execute permission to hab user on settings folder and its contents
chmod 747 -R "$(hab pkg path 'chef/oc_id')/oc_id/config/settings"