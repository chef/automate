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

This guide details how to set up the load balancer configuration with:

- HAProxy
- Nginx

## HAProxy

The steps to set up the load balancer using HAProxy are:

1. Install HAProxy on the server using the following commands:

```bash
sudo add-apt-repository ppa:vbernat/haproxy-1.8 
sudo apt-get update 
sudo apt-get install haproxy
```

2. Create a self-signed SSL certificate to make communication encrypted using the following steps:

```bash
apt-get -y install openssl 
openssl req -nodes -x509 -newkey rsa:2048 -keyout /etc/ssl/private/test.key -out /etc/ssl/private/test.crt -days 30 
cat /etc/ssl/private/test.key /etc/ssl/private/test.crt > /etc/ssl/private/test.pem 
```

3. **Add HAProxy Listener:** Edit the configuration file to set it for the automate. Once done, you will find some default settings in the file.

```bash
sudo vi /etc/haproxy/haproxy.cfg 
```

Add two listeners, one on **Port 80** and the other on **Port 443** in the configuration file.

```bash
frontend http_80 
bind 172.31.10.107:80 
mode http 
default_backend automate-servers 

frontend https_443 
bind 172.31.10.107:443 ssl crt /etc/ssl/private/test.pem 
mode http 
http-request set-header X-Forwarded-For %[src] 

reqadd X-Forwarded-Proto:\ https 

option http-server-close 

default_backend automate-servers 

#Add Backend Web Servers 
#Here, we will add a backend server where we want to forward our requests. 
 
backend automate-servers 

balance roundrobin 
server ec2-3-8-188-123.eu-west2.compute.amazonaws.com 3.8.188.123:443 check ssl verify none 
server ec2-3-9-146-238.eu-west-2.compute.amazonaws.com 3.9.146.238:443 check ssl verify none 
server ec2-18-132-204-33.eu-west-2.compute.amazonaws.com 18.132.204.33:443 check ssl verify none 
```

4. The configured file looks like as shown below:

```bash
global 

log /dev/log	local0
log /dev/log	local1 notice 
chroot /var/lib/haproxy 

stats socket /run/haproxy/admin.sock mode 660 level admin expose-fd listeners 
stats timeout 30s 
user haproxy 
group haproxy 
daemon 

# Default SSL material locations 

ca-base /etc/ssl/certs 
crt-base /etc/ssl/private 

# Default ciphers to use on SSL-enabled listening sockets. 
# For more information, see ciphers(1SSL). This list is from: 
#  https://hynek.me/articles/hardening-your-web-servers-ssl-ciphers/ 
# An alternative list with additional directives can be obtained from 
#  https://mozilla.github.io/server-side-tls/ssl-config-generator/?server=haproxy 

ssl-default-bind-ciphers ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:ECDH+AES128:DH+AES:RSA+AESGCM:RSA+AES:!aNULL:!MD5:!DSS 

ssl-default-bind-options no-sslv3 

defaults 

log	global 
mode	http 
option	httplog 
option	dontlognull

  timeout connect 5000
  timeout client  50000
  timeout server  50000

errorfile 400 /etc/haproxy/errors/400.http
errorfile 403 /etc/haproxy/errors/403.http
errorfile 408 /etc/haproxy/errors/408.http
errorfile 500 /etc/haproxy/errors/500.http
errorfile 502 /etc/haproxy/errors/502.http
errorfile 503 /etc/haproxy/errors/503.http
errorfile 504 /etc/haproxy/errors/504.http
  
frontend http 

  bind 172.31.10.107:80 
  mode http 
  default_backend automate-servers 

frontend https_443 
bind 172.31.10.107:443 ssl crt /etc/ssl/private/test.pem
mode http 

http-request set-header X-Forwarded-For %[src] 
reqadd X-Forwarded-Proto:\ https 
option http-server-close 
default_backend automate-servers 

backend automate-servers 

  balance roundrobin 

  server ec2-3-8-188-123.eu-west-2.compute.amazonaws.com 3.8.188.123:443 check ssl verify none 
  server ec2-3-9-146-238.eu-west-2.compute.amazonaws.com 3.9.146.238:443 check ssl verify none 
  server ec2-18-132-204-33.eu-west-2.compute.amazonaws.com 18.132.204.33:443 check ssl verify none 
 ```

5. Check configuration file using `haproxy -c -f /etc/haproxy/haproxy.cfg `.

6. Restart haproxy using `service haproxy restart `.

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
