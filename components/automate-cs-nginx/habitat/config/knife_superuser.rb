node_name "pivotal"
chef_server_url "https://localhost:{{cfg.service.port}}/"
chef_server_root "https://localhost:{{cfg.service.port}}/"
no_proxy "127.0.0.1"
client_key "/hab/svc/automate-cs-oc-erchef/data/pivotal.pem"
# TODO: update this for mutual TLS auth
ssl_verify_mode :verify_none

ssl_client_cert "{{pkg.svc_config_path}}/service.crt"
ssl_client_key  "{{pkg.svc_config_path}}/service.key"
