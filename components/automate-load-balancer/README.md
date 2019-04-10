# Automate Load Balancer

Automate Load Balancer is a wrapper package for nginx. It provides the
front-end for web and API clients to connect to Chef Automate.

Though nginx is capable of implementing complex application logic, the
configuration should be kept minimal. If you need to implement
application behavior in a load balancer context, implement it in
automate-gateway.

## Binding to Upstream Services

The deployment service runs Automate Load Balancer with habitat's
relaxed binding mode. This means that the load balancer will run
immediately regardless of whether upstreams are up or not. If a new
upstream becomes available, habitat will reconfigure and restart the
load balancer.

* https://www.habitat.sh/docs/developing-packages/#pkg-binds
* https://www.habitat.sh/blog/2018/05/0560-Release/

## Optional Upstreams

We take advantage of the relaxed habitat binding behavior above to
integrate the load balancer with optional components. Currently, only
Chef Server is available, though others will likely be added in the
future.

We separate the configuration of optional upstreams from the main A2
routes. Core A2 routes are defined in the `nginx.conf` file. Optional
components are defined in `COMPONENT_NAME-location.conf` and
`COMPONENT_NAME-upstream.conf` files. The main configuration file loads
them via a glob pattern so the `-location.conf` and `-upstream.conf`
parts of the file names are required.

The general pattern to follow for the upstream file is like this:

```
upstream cs-nginx {
   {{ ~#eachAlive bind.automate-cs-nginx.members as |member| }}
   server {{member.sys.ip}}:{{member.cfg.port}};
   {{ ~else }}
   server 127.0.0.1:65535; # force 502
   {{ ~/eachAlive }}
}
```
