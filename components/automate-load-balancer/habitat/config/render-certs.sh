#!{{pkgPathFor "core/bash"}}/bin/bash

exec 2>&1
# This file is for openssl to put random bits into when doing key generation.
export RANDFILE="{{pkg.svc_data_path}}/.rnd"
touch $RANDFILE

# Hab does not support dynamic templates so we're iterating over our frontend_tls
# and generating certificate and key files for each server listener in nginx.
{{#each cfg.frontend_tls as |tls| ~}}
cert_file="{{../pkg.svc_data_path}}/{{tls.server_name}}.cert"
key_file="{{../pkg.svc_data_path}}/{{tls.server_name}}.key"
echo "{{~ tls.cert ~}}" > $cert_file
echo "{{~ tls.key ~}}" > $key_file

chmod 0600 $cert_file $key_file
{{/each ~}}

