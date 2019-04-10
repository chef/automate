# NOTE workflow_api doesn't care for the requests' signatures, but delivery-cmd
# uses Chef::REST -- so this needs to be set to some key material:
client_key "/var/opt/delivery/workspace/etc/builder_key"
