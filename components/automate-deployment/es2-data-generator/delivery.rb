delivery_fqdn 'automate-deployment.test'
delivery['chef_username'] = 'delivery'
delivery['chef_private_key'] = '/var/opt/es2-data-generator/test.pem'
delivery['chef_server'] = 'https://localhost:8443/organizations/test/'
insights['enable'] = true
delivery['primary'] = true
postgresql['listen_address'] = '0.0.0.0'
postgresql['superuser_enable'] = true
postgresql['superuser_password']= 'pokemon'
insights['enable_auth'] = true
kibana['enable'] = false
reaper['enable'] = false

