+++
title = "Loadbalancer Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Loadbalancer Configuration"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/loadbalancer_configuration.md Loadbalancer Configuration"
    weight = 20
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This is the sample guide will show you how to configure a load balancer for Chef Automate and Chef Infra Server. For more details check with expert in your organization.

## Load Balancer Setup Examples

- Using NGINX
- Using HA Proxy

## Load Balancer Prerequisites

-  Before proceeding with the load balancer setup, you must configure DNS for Automate and Chef Server. In this guide, we assume that you have already set up DNS with the following domain names:

   - Chef Automate: chefautomate.example.com
   - Chef Infra Server: chefinfraserver.example.com

## Load Balancer architecture

There are two recommended load balancer setups for Automate, depending on your feasibility:

- Option 1: 2 Load Balancers with 2 Private IPs Each
   - This setup requires two identical load balancer nodes to ensure high availability.
   - Each node needs two private IPs, one for Automate and another for Chef Server.
   - To set up DNS, point the Chef Automate DNS (chefautomate.example.com) to Private IP 1 of both nodes, and the Chef Server DNS (chefinfraserver.example.com) to Private IP 2 of both nodes.

- Option 2: 4 Load Balancers, separate for Automate and separate for Chef Server
   - This setup requires two load balancers for Automate and two for Chef Server to ensure high availability.
   - Each node only requires one private IP.
   - To set up DNS, point the Chef Automate DNS (chefautomate.example.com) to the Automate nodes and the Chef Server DNS (chefinfraserver.example.com) to the Chef Server nodes.

With these load balancer setups, you can ensure high availability for Chef Automate and Chef Infra Server.

## 2 Load Balancer Setup with two private IPs each

### Load Balancer setup using NGINX

#### Install Nginx on all Load Balancers

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

#### Configure

1. Create a new file `/etc/nginx/sites-available/chef-automate-lb.conf`

   ```bash
   upstream chef-automate-servers {
      # Add a list of automate machine IP addresses.
      server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
   }

   # The below section is used for HTTPS call
   server {
      # Add the private IP that's connected to Automate DNS, like 10.1.1.194:443 
      listen <PRIVATE-IP-AUTOMATE>:443 ssl;
      # You need to get your own automate DNS,
      # Here, we have taken an example DNS: chefautomate.example.com
      server_name chefautomate.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This URL is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/chefautomate.example.com/privkey.pem;
      ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

      location / {
         proxy_pass https://chef-automate-servers;
         proxy_set_header Host $host;
      }
   }

   # The below section is used for HTTP calls
   server {
      listen 80;
      server_name chefautomate.example.com;
      return 301 https://$server_name$request_uri;
   }
   ```

1. Create a new file `/etc/nginx/sites-available/chef-infra-server-lb.conf`

   ```bash
   upstream chef-infra-servers {
      # Add a list of infra server machine API addresses.
      server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
   }

   # The below section is used for HTTPS call
   server {
      # Add the private IP that's connected to Chef Server DNS, like 10.1.1.67:443 
      listen <PRIVATE-IP-CHEF-SERVER>:443 ssl;
      # You need to get your infra server DNS,
      # Here, we have taken an example DNS: chefinfraserver.example.com
      server_name chefinfraserver.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This URL is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem;
      ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

      location / {
         proxy_pass https://chef-infra-servers;
         proxy_set_header Host $host;
      }
   }

   # The below section is used for HTTP calls
   server {
      listen 80;
      server_name chefinfraserver.example.com;
      return 301 https://$server_name$request_uri;
   }
   ```

1. Enable Sites for Chef Automate and Chef Infra Server

   ```bash
   sudo ln -s /etc/nginx/sites-available/chef-automate-lb.conf /etc/nginx/sites-enabled/
   sudo ln -s /etc/nginx/sites-available/chef-infra-server-lb.conf /etc/nginx/sites-enabled/
   ```

1. Test Nginx Config

   ```bash
   sudo nginx -t
   ```

1. Restart Nginx

   ```bash
   sudo systemctl restart nginx
   ```

### Load Balancer setup using HA Proxy

#### Install HA Proxy on all Load Balancers

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

#### Configure

1. HAProxy needs an SSL certificate to be one file in a specific format. To do that, we create a new directory with the SSL certificate for Chef Automate and Infra Server that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

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

1. Once HA Proxy is installed, add the following to the configuration file at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for Chef Automate and Chef Infra Server.

   ```bash
   # The below section is used for HTTP calls
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own Automate DNS and Chef Server,
   # Here, we have taken example DNS: chefautomate.example.com and chefinfraserver.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal

   frontend chef-automate-servers
      # Add the private IP thats connected to Automate DNS, like 10.1.1.194:443 
      bind <PRIVATE-IP-AUTOMATE>:443 ssl crt /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      mode http
      default_backend chef-automate-servers

   frontend chef-infra-servers
      # Add the private IP thats connected to Chef Server DNS, like 10.1.1.67:443 
      bind <PRIVATE-IP-CHEF-SERVER>:443 ssl crt /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      mode http
      default_backend chef-infra-servers

   backend chef-automate-servers
      mode http
      balance roundrobin
      # Add a list of automate machine ip addresses.
      server automate1 10.1.0.101:443 check ssl verify none
      server automate2 10.1.0.102:443 check ssl verify none
      server automate3 10.1.0.103:443 check ssl verify none

   backend chef-infra-servers
      mode http
      balance roundrobin
      # Add a list of infra server machine ip addresses.
      server infra1 10.1.0.101:443 check ssl verify none
      server infra2 10.1.0.102:443 check ssl verify none
      server infra3 10.1.0.103:443 check ssl verify none
   ```

1. Test HA Proxy Config

   ```bash
   sudo haproxy -c -f /etc/haproxy/haproxy.cfg
   ```

1. Restart HA Proxy

   ```bash
   sudo systemctl restart haproxy
   ```

## 4 Load Balancers Setup, separate for Automate and separate for Chef Server 

### Load Balancer setup using NGINX

#### Install Nginx on all the Load Balancers

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

#### Configure these on Automate Load Balancers

1. Create a new file `/etc/nginx/sites-available/chef-automate-lb.conf`

   ```bash
   upstream chef-automate-servers {
      # Add a list of automate machine IP addresses.
      server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
   }

   # The below section is used for HTTPS calls
   server {
      listen 443 ssl;
      # You need to get your own automate DNS,
      # Here, we have taken an example DNS: chefautomate.example.com
      server_name chefautomate.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This URL is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/chefautomate.example.com/privkey.pem;
      ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

      location / {
         proxy_pass https://chef-automate-servers;
         proxy_set_header Host $host;
      }
   }

   # The below section is used for HTTP calls
   server {
      listen 80;
      server_name chefautomate.example.com;
      return 301 https://$server_name$request_uri;
   }
   ```

1. Enable Sites for Chef Automate

   ```bash
   sudo ln -s /etc/nginx/sites-available/chef-automate-lb.conf /etc/nginx/sites-enabled/
   ```

1. Test Nginx Config

   ```bash
   sudo nginx -t
   ```

1. Restart Nginx

   ```bash
   sudo systemctl restart nginx
   ```

#### Configure these on Chef Server Load Balancers

1. Create a new file `/etc/nginx/sites-available/chef-infra-server-lb.conf`

   ```bash
   upstream chef-infra-servers {
      # Add a list of infra server machine IP addresses.
      server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
   }

   # The below section is used for HTTPS calls
   server {
      listen 443 ssl;
      # You need to get your own infra server DNS,
      # Here, we have taken an example DNS: chefinfraserver.example.com
      server_name chefinfraserver.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This URL is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem;
      ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

      location / {
         proxy_pass https://chef-infra-servers;
         proxy_set_header Host $host;
      }
   }

   # The below section is used for HTTP calls
   server {
      listen 80;
      server_name chefinfraserver.example.com;
      return 301 https://$server_name$request_uri;
   }
   ```

1. Enable Sites for Chef Automate and Chef Infra Server

   ```bash
   sudo ln -s /etc/nginx/sites-available/chef-infra-server-lb.conf /etc/nginx/sites-enabled/
   ```

1. Test Nginx Config

   ```bash
   sudo nginx -t
   ```

1. Restart Nginx

   ```bash
   sudo systemctl restart nginx
   ```

### Load Balancer setup using HA Proxy

#### Install HA Proxy on all Load Balancers

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

#### Configure on Automate Load Balancers

1. HAProxy needs an SSL certificate to be one file in a specific format. To do that, we create a new directory with the SSL certificate for Automate that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

   - For Chef Automate:

      ```bash
      sudo mkdir -p /etc/ssl/chefautomate.example.com

      sudo cat /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefautomate.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      ```

1. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for Chef Automate.

   ```bash
   # The below section is used for HTTP calls
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own automate DNS,
   # Here we have taken an example DNS: chefautomate.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This URL is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal

   frontend chef-automate-servers
      bind *:443 ssl crt /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      mode http
      default_backend chef-automate-servers

   backend chef-automate-servers
      mode http
      balance roundrobin
      # Add a list of automate machine ip addresses.
      server automate1 10.1.0.101:443 check ssl verify none
      server automate2 10.1.0.102:443 check ssl verify none
      server automate3 10.1.0.103:443 check ssl verify none
   ```

1. Test HA Proxy Config

   ```bash
   sudo haproxy -c -f /etc/haproxy/haproxy.cfg
   ```

1. Restart HA Proxy

   ```bash
   sudo systemctl restart haproxy
   ```

#### Configure on Chef Server Load Balancers

1. HAProxy needs an SSL certificate to be one file in a specific format. To do that, we create a new directory with the SSL certificate for Infra Server that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

   - For Chef Infra Server:

      ```bash
      sudo mkdir -p /etc/ssl/chefinfraserver.example.com

      sudo cat /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      ```

1. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for Chef Infra Server.

   ```bash
   # The below section is used for HTTP calls
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own Chef Server DNS,
   # Here we have taken an example DNS: chefinfraserver.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal

   frontend chef-infra-servers
      bind *:443 ssl crt /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      mode http
      default_backend chef-infra-servers

   backend chef-infra-servers
      mode http
      balance roundrobin
      # Add a list of infra server machine ip addresses.
      server infra1 10.1.0.101:443 check ssl verify none
      server infra2 10.1.0.102:443 check ssl verify none
      server infra3 10.1.0.103:443 check ssl verify none
   ```

1. Test HA Proxy Config

   ```bash
   sudo haproxy -c -f /etc/haproxy/haproxy.cfg
   ```

1. Restart HA Proxy

   ```bash
   sudo systemctl restart haproxy
   ```
