+++
title = "Loadbalancer Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Loadbalancer Configuration"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/loadbalancer_configuration.md Loadbalancer Configuration"
    weight = 230
+++

This guide details how to set up the load balancer configuration for Chef Automate and Chef Infra Server.


## Primary Load Balancer

Assuming you have DNS configured for domain names for:
- Chef Automate : chefautomate.example.com
- Chef Infra Server : chefinfraserver.example.com

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

1. Create new site file in `/etc/nginx/sites-available/` called `chef-automate-lb.conf`
```bash
upstream backend {
   server 10.1.0.101:443; 
   server 10.1.0.102:443;
   server 10.1.0.103:443;
}

server {
   listen 443 ssl;
   server_name chefautomate.example.com;
   ssl_certificate /etc/letsencrypt/live/chefautomate.example.com/cert.pem;
   ssl_certificate_key /etc/letsencrypt/live/chefautomate.example.com/privkey.pem;
   ssl_protocols TLSv1 TLSv1.1 TLSv1.2;

   location / {
      proxy_pass http://backend;
   }
}

server {
   listen 80;
   server_name chefautomate.example.com;
   return 301 https://$server_name$request_uri;
}
```


## Nginx

1. Install **Nginx** on server Debian and Ubuntu using the following commands:

```bash
sudo apt-get update  
sudo apt-get install nginx 

# centos and redhat 
sudo yum install epel-release 
sudo yum update 
sudo yum install nginx 
```

2. To Configure Nginx as a loadbalancer, create `.conf` file for the Load balancer `vi /etc/nginx/conf.d/load-balancer.conf`. In the `load-balancer.conf` file, add the following content:

```sh
#List of servers behind LB 

upstream backend {

  server 172.31.46.240:443;
  server 172.31.8.233:443;
  server 172.31.24.254:443;

},
server { 

  Listen 443 ssl; 
  ssl_certificate /etc/ssl/private/test.crt; 
  ssl_certificate_key /etc/ssl/private/test.pem; 
  ssl_protocols TLSv1 TLSv1.1 TLSv1.2; 

  location / { 
    proxy_pass https://backend; 
  } 
}
```

3. Check if the configuration is correct using the `sudo nginx –t ` command.

4. Remove the already existing default running the site on **Debian** and **Ubuntu** systems. Remove the default symbolic link from the sites-enabled folder.

```bash
sudo rm /etc/nginx/sites-enabled/default
```

**CentOS** hosts doesn't use the same link. Instead, rename the `default.conf` in the `conf.d/` directory to an extension except `.conf`. Example: `sudo mv /etc/nginx/conf.d/default.conf /etc/nginx/conf.d/default.conf.disabled `.

5. Restart the nginx using `sudo systemctl restart nginx `.
