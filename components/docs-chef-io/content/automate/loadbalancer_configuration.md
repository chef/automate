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

This guide details how to set up the load balancer configuration for Chef Automate and Chef Infra Server.

## Load Balancer

Assuming you have DNS configured with domain names:

- Chef Automate: chefautomate.example.com
- Chef Infra Server: chefinfraserver.example.com

## Install Nginx

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

## Configure

1. Create new file `/etc/nginx/sites-available/chef-automate-lb.conf`

```bash
upstream chef-automate-servers {
   # Add a list of automate machine ip addresses.
   # Minimum automate machine is 2
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
   # Minimum infra server machine is 2
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
