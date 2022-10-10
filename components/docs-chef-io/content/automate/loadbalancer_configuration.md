+++
title = "Loadbalancer Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Loadbalancer Configuration"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/loadbalancer_configuration.md Loadbalancer Configuration"
    weight = 220
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This guide details how to set up the load balancer configuration for Chef Automate and Chef Infra Server.

## Listed below are some ways for setting up load balancer.
- Using NGINX
- Using HA Proxy

## Load Balancer

Assuming you have DNS configured with domain names:

- Chef Automate: chefautomate.example.com
- Chef Infra Server: chefinfraserver.example.com

## Load Balancer setup using NGINX
### Install Nginx

For Debian / Ubuntu :

```bash
sudo apt-get update
sudo apt-get install nginx
```

For Centos or Redhat :

```bash
sudo yum install epel-release
sudo yum update
sudo yum install nginx
```

### Configure

1. Create new file `/etc/nginx/sites-available/chef-automate-lb.conf`

```bash
upstream chef-automate-servers {
   # Add a list of automate machine ip addresses.
   server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
   server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
   server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
}

# The below section is used for https call
server {
   listen 443 ssl;
   # You need to get your own automate DNS,
   # here we have taken example DNS: chefautomate.example.com
   server_name chefautomate.example.com;
   # Here ssl certificate is added,
   # we have created certificate using certBot, below url is an example for ubuntu machine
   # reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/cert.pem;
   ssl_certificate_key /etc/letsencrypt/live/chefautomate.example.com/privkey.pem;
   ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

   location / {
      proxy_pass https://chef-automate-servers;
      proxy_set_header Host $host;
   }
}

# The below section is used for http call
server {
   listen 80;
   server_name chefautomate.example.com;
   return 301 https://$server_name$request_uri;
}
```

2. Create new file `/etc/nginx/sites-available/chef-infra-server-lb.conf`

```bash
upstream chef-infra-servers {
   # Add a list of infra server machine api addresses.
   server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
   server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
   server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
}

# The below section is used for https call
server {
   listen 443 ssl;
   # You need to get your own infra server DNS,
   # here we have taken example DNS: chefinfraserver.example.com
   server_name chefinfraserver.example.com;
   # Here ssl certificate is added,
   # we have created certificate using certBot, below url is an example for ubuntu machine
   # reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   ssl_certificate /etc/letsencrypt/live/chefinfraserver.example.com/cert.pem;
   ssl_certificate_key /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem;
   ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

   location / {
      proxy_pass https://chef-infra-servers;
      proxy_set_header Host $host;
   }
}

# The below section is used for http call
server {
   listen 80;
   server_name chefinfraserver.example.com;
   return 301 https://$server_name$request_uri;
}
```

3. Enable Sites for Chef Automate and Chef Infra Server

```bash
sudo ln -s /etc/nginx/sites-available/chef-automate-lb.conf /etc/nginx/sites-enabled/
sudo ln -s /etc/nginx/sites-available/chef-infra-server-lb.conf /etc/nginx/sites-enabled/
```

4. Test Nginx Config

```bash
sudo nginx -t
```

5. Restart Nginx

```bash
sudo systemctl restart nginx
```

## Load Balancer setup using HA Proxy

For Debian / Ubuntu :

```bash
sudo apt-get update
sudo apt install haproxy
```

For Centos or Redhat :

```bash
sudo yum install epel-release
sudo yum update
sudo yum install haproxy
```

### Configure

1. HAProxy needs an ssl-certificate to be one file, in a certain format. To do that, we create a new directory where the SSL certificate for automate and infra server that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

   - For Chef Automate:

      ```bash
      sudo mkdir -p /etc/ssl/chefautomate.example.com

      sudo cat /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefautomate.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      ```

   - For Chef Infra Server:

      ```bash
      sudo mkdir -p /etc/ssl/chefinfraserver.example.com

      sudo cat /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      ```

2. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for chef automate and chef infra server.

```bash
# The below section is used for http call
frontend fe_a2ha_http
    mode http
    bind *:80
    redirect scheme https code 301 if !{ ssl_fc }

# The below section is used for https call
frontend fe_a2ha
   mode tcp
   # You need to get your own automate DNS,
   # here we have taken example DNS: chefautomate.example.com and chefinfraserver.example.com
   # Here ssl certificate is added,
   # we have created certificate using certBot, below url is an example for ubuntu machine
   # reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   bind *:443 ssl crt /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem crt /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
   use_backend automate_server if { ssl_fc_sni chefautomate.example.com }
   use_backend chef_infra_server if { ssl_fc_sni chefinfraserver.example.com }

backend automate_server
   balance roundrobin
   # Add a list of automate machine ip addresses.
   server automate1 10.1.0.101:443 check ssl verify none
   server automate2 10.1.0.102:443 check ssl verify none
   server automate3 10.1.0.103:443 check ssl verify none

backend chef_infra_server
   balance roundrobin
   # Add a list of infra server machine ip addresses.
   server infra1 10.1.0.101:443 check ssl verify none
   server infra2 10.1.0.102:443 check ssl verify none
   server infra3 10.1.0.103:443 check ssl verify none
```

3. Test HA Proxy Config

```bash
sudo haproxy -c -f /etc/haproxy/haproxy.cfg
```

4. Restart HA Proxy

```bash
sudo systemctl restart haproxy
```
