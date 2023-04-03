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

This guide details how to set up the load balancer configuration for Chef Automate and Chef Infra Server.

## Listed below are some examples for setting up load balancer

- Using NGINX
- Using HA Proxy

## Load Balancer Prerequisites

-  Configure DNSs for Automate and Chef Server.
  
   In the following examples we have assumed that you have DNSs configured with these domain names:

   - Chef Automate: chefautomate.example.com
   - Chef Infra Server: chefinfraserver.example.com

## Load Balancer architecture

Following are the recommended architecture for Load Balancer setup in Automate. You can choose either one of them based on your feasibility.

- 2 Load Balancers with 2 private ips each (each LB is shared between Automate and Chef Server)
   - In this setup we need two indentical Load Balancer nodes to ensure High Availablity.
   - Each node requires two private ips( one for Automate and another for Chef Server ).
   - Automate DNS (chefautomate.example.com) should be pointed to the Private IP 1 of both the nodes and Chef Server DNS (chefinfraserver.example.com) should be pointed to the Private IP 2 of both the node.
- 4 Load Balancers, separate for Automate and separate for Chef Server (single private IP required on each node unlike the above example)
   - In this setup we need 2 Load Balancers for Automate and 2 for Chef Server to ensure High Availability.
   - Automate DNS (chefautomate.example.com) should be pointed to the Automate nodes and Chef Server DNS (chefinfraserver.example.com) should be pointed to the Chef Server nodes.


## 2 Load Balancer Setup with 2 private ips each

### Load Balancer setup using NGINX

### Install Nginx on all Load Balancers

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
      # Add the private IP thats connected to Automate DNS, like 10.1.1.194:443 
      listen <PRIVATE-IP-AUTOMATE>:443 ssl;
      # You need to get your own automate DNS,
      # here we have taken example DNS: chefautomate.example.com
      server_name chefautomate.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem;
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

1. Create new file `/etc/nginx/sites-available/chef-infra-server-lb.conf`

   ```bash
   upstream chef-infra-servers {
      # Add a list of infra server machine api addresses.
      server 10.1.0.101:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.102:443 max_fails=2 fail_timeout=30s;
      server 10.1.0.103:443 max_fails=2 fail_timeout=30s;
   }

   # The below section is used for https call
   server {
      # Add the private IP thats connected to Chef Server DNS, like 10.1.1.67:443 
      listen <PRIVATE-IP-CHEF-SERVER>:443 ssl;
      # You need to get your own infra server DNS,
      # here we have taken example DNS: chefinfraserver.example.com
      server_name chefinfraserver.example.com;
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem;
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

### Install HA Proxy on all Load Balancers

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

1. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for chef automate and chef infra server.

   ```bash
   # The below section is used for http call
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own automate DNS,
   # here we have taken example DNS: chefautomate.example.com and chefinfraserver.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   frontend chef-infra-servers
      # Add the private IP thats connected to Chef Server DNS, like 10.1.1.67:443 
      bind <PRIVATE-IP-CHEF-SERVER>:443 ssl crt /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      mode tcp
      default_backend chef-infra-servers

   frontend chef-automate-servers
      # Add the private IP thats connected to Automate DNS, like 10.1.1.194:443 
      bind <PRIVATE-IP-AUTOMATE>:443 ssl crt /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      mode tcp
      default_backend chef-automate-servers

   backend automate_server
      balance roundrobin
      http-request set-header Host chefautomate.example.com
      # Add a list of automate machine ip addresses.
      server automate1 10.1.0.101:443 check ssl verify none
      server automate2 10.1.0.102:443 check ssl verify none
      server automate3 10.1.0.103:443 check ssl verify none

   backend chef_infra_server
      balance roundrobin
      http-request set-header Host chefinfraserver.example.com
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

### Install Nginx on all the Load Balancers

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
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem;
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

1. Create new file `/etc/nginx/sites-available/chef-infra-server-lb.conf`

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
      # Generate SSL certificates and give the path of the certificate and key file.
      # If you want to use letsencript certificates, you can use the certBot
      # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
      ssl_certificate /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem;
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

### Install HA Proxy on all Load Balancers

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

1. HAProxy needs an ssl-certificate to be one file, in a certain format. To do that, we create a new directory where the SSL certificate for automate and infra server that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

   - For Chef Automate:

      ```bash
      sudo mkdir -p /etc/ssl/chefautomate.example.com

      sudo cat /etc/letsencrypt/live/chefautomate.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefautomate.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      ```

1. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for chef automate and chef infra server.

   ```bash
   # The below section is used for http call
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own automate DNS,
   # here we have taken example DNS: chefautomate.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   frontend chef-automate-servers
      bind *:443 ssl crt /etc/ssl/chefautomate.example.com/chefautomate.example.com.pem
      mode tcp
      default_backend chef-automate-servers

   backend automate_server
      balance roundrobin
      http-request set-header Host chefautomate.example.com
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

1. HAProxy needs an ssl-certificate to be one file, in a certain format. To do that, we create a new directory where the SSL certificate for automate and infra server that HAProxy reads will live. Then we output the "live" (latest) certificates from LetsEncrypt and dump that output into the certificate file for HAProxy to use:

   - For Chef Infra Server:

      ```bash
      sudo mkdir -p /etc/ssl/chefinfraserver.example.com

      sudo cat /etc/letsencrypt/live/chefinfraserver.example.com/fullchain.pem \
         /etc/letsencrypt/live/chefinfraserver.example.com/privkey.pem \
         | sudo tee /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      ```

1. Once HA Proxy is installed, add the following to the configuration file present at `/etc/haproxy/haproxy.cfg`. This will set the load balancer config for chef automate and chef infra server.

   ```bash
   # The below section is used for http call
   frontend fe_a2ha_http
      mode http
      bind *:80
      redirect scheme https code 301 if !{ ssl_fc }

   # You need to get your own automate DNS,
   # here we have taken example DNS: chefautomate.example.com and chefinfraserver.example.com
   # Generate SSL certificates and give the path of the certificate and key file.
   # If you want to use letsencript certificates, you can use the certBot
   # This url is an example for ubuntu machine reference: https://certbot.eff.org/instructions?ws=nginx&os=ubuntufocal
   frontend chef-infra-servers
      bind *:443 ssl crt /etc/ssl/chefinfraserver.example.com/chefinfraserver.example.com.pem
      mode tcp
      default_backend chef-infra-servers

   backend chef_infra_server
      balance roundrobin
      http-request set-header Host chefinfraserver.example.com
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
