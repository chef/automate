# Need this super minimal delivery.rb for some automate-ctl functions
{{#if cfg.fqdn ~}}
delivery_fqdn "{{cfg.fqdn}}"
{{else ~}}
delivery_fqdn "{{sys.ip}}"
{{/if ~}}
delivery['chef_username']    = "{{cfg.chef_server.chef_user}}"
delivery['chef_private_key'] = "{{pkg.svc_path}}{{cfg.chef_server.chef_user_private_key}}"
delivery['chef_server']      = "{{cfg.chef_server.url}}"
{{#if cfg.proxy.host ~}}
delivery['proxy']['host'] = "{{cfg.proxy.host}}"
{{/if ~}}
{{#if cfg.proxy.port ~}}
delivery['proxy']['port'] = "{{cfg.proxy.port}}"
{{/if ~}}
{{#if cfg.proxy.user ~}}
delivery['proxy']['user'] = "{{cfg.proxy.user}}"
{{/if ~}}
{{#if cfg.proxy.password ~}}
delivery['proxy']['password'] = "{{cfg.proxy.password}}"
{{/if ~}}
{{#if cfg.proxy.no_proxy ~}}
delivery['proxy']['no_proxy'] = "{{cfg.proxy.no_proxy}}"
{{/if}}