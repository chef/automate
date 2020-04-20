package deployment

const configTemplate = `
# This is a default Chef Automate configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate instance with default settings.

[global.v1]
  # The external fully qualified domain name.
  # When the application is deployed you should be able to access 'https://<fqdn>/'
  # to login.
  fqdn = "{{ .Fqdn }}"

  # TLS Certificates for External-Facing Services
  {{- .TLSWarning}}
  [[global.v1.frontend_tls]]
    # The TLS certificate for the load balancer frontend.
    cert = """{{ .FrontendCert }}"""

    # The TLS RSA key for the load balancer frontend.
    key = """{{ .FrontendKey }}"""

{{- if ne .ProxyHost ""}}
  [global.v1.proxy]
    # Proxy settings
    host = "{{ .ProxyHost }}"
    port = {{ .ProxyPort }}
{{- if ne .ProxyUser ""}}
    username = "{{ .ProxyUser }}"
{{- end}}
{{- if ne .ProxyPassword ""}}
    password = "{{ .ProxyPassword }}"
{{- end}}
{{- if .NoProxy}}
    no_proxy = ["{{ StringsJoin .NoProxy "\",\"" }}"]
{{- end}}
{{- end}}


# Deployment service configuration.
[deployment.v1]
  [deployment.v1.svc]
    # Habitat channel to install hartifact from.
    # Can be 'dev', 'current', or 'acceptance'
    channel = "{{ .Channel }}"
    upgrade_strategy = "{{ .UpgradeStrategy }}"
    deployment_type = "{{ .DeploymentType }}"

# License Control service configuration.
[license_control.v1]
  [license_control.v1.svc]
    # The Chef Software provided license token required to run Chef Automate.
    # This can also be set with the "chef-automate license apply" command.
    license = ""

# Chef Automate services can be configured to better meet the needs of
# your particular installation. For details see:
#
# https://automate.chef.io/docs/configuration/
#
[elasticsearch.v1.sys.runtime]
  heapsize = "{{ .ESHeapSize }}"
`
