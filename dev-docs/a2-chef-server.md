## Context
The Automate deployment service can be used to deploy a standalone chef server without
Automate. We consider this feature in its current state suitable for customer use as a
services option. We have discussed the work to make deploying a single chef server via the
Automate deployment service a full DIY product feature, but as of yet, we do not have
concrete plans or schedule. Deploying HA chef server via the Automate deployment service
is intended as a services option only.

As of July 2019, the audience for the chef server deployed via the Automate deployment service consists
of:

1. opsworks, which deploys an all-in-one Automate with chef server on a single node
2. the Customer Success team, which in July 2019 will start using the deployment service to deploy HA
   chef server at selected customer sites,
3. the Professional Services team, which is planning to start using the deployment service
   to deploy HA chef server some time around August 2019.

This document was written with the latter two groups in mind.

## Deploy a new standalone chef server via Automate
1. Install the chef-automate CLI tool. You will need one that contains commits: look for
   hashes.
2. Run `chef-automate init-config` to generate an editable config.toml.
3. Add the following to the config.toml from 2:
```
[erchef.v1.sys.data_collector]
enabled = false
```
Make any other config changes necessary.
4. Run `chef-automate deploy --product chef-server config.toml` to install the chef
   server. When the chef server comes up, the running services are:
```
   $ sudo ./chef-automate status

   Status from deployment with channel [dev] and type [local]

   Service Name            Process State  Health Check  Uptime (s) PID
   deployment-service      running        ok            400        12974
   backup-gateway          running        ok            280        13361
   automate-postgresql     running        ok            277        13464
   automate-pg-gateway     running        ok            280        13354
   automate-elasticsearch  running        ok            279        13399
   automate-es-gateway     running        ok            274        13706
   pg-sidecar-service      running        ok            274        13715
   es-sidecar-service      running        ok            258        14344
   license-control-service running        ok            275        13650
   automate-cs-bookshelf   running        ok            273        13771
   automate-cs-oc-bifrost  running        ok            271        13857
   automate-cs-oc-erchef   running        ok            258        14352
   automate-cs-nginx       running        ok            270        13920
   automate-load-balancer  running        ok            267        14166
```

## Forward data from the Automate-deployed chef server to Automate
1. Make sure you have the data collector token from your Automate instance handy.
2. On the node where you have deployed the chef server, create a chef_server_patch.toml file that contains
```
   [global.v1.external.automate]
   enable = true
   endpoint = "https://automate.example.com/data-collector/v0/" # is this right?
   [global.v1.external.automate.auth]
   token = "yourdatacollectortoken"

   [deployment.v1.svc]           # is this section necessary?
   products = ["chef-server"]

   [erchef.v1.sys.data_collector]
   enabled = true
   ```
3. Patch your chef server deployment: `chef-automate config patch chef_server_patch.toml`

## Deploy HA chef server via the Automate deployment-service
HA for the Automate-deployed chef server involves externally load-balanced chef servers sharing external
elasticsearch and postgres. As such, it is a services option and not intended as a DIY
option for customers.

### Deploy a chef server with external data services via the Automate deployment service
1. Install the chef-automate CLI tool. You will need one that contains commits: look for
   hashes.
2. Run `chef-automate init-config` to generate an editable config.toml.
3. Add the following to the config.toml from 2:
```
   [erchef.v1.sys.data_collector]
   enabled = false
```
4. Configure external elasticsearch by adding the following to the config.toml:
```
   [global.v1.external.elasticsearch]
   enable = true
   nodes = ["http://server1:9200", "http://server2:9200"]     # can be either http or https, but not a mixture

   # Uncomment and fill out if using elasticsearch with basic auth
   # [global.v1.external.elasticsearch.auth]
   # scheme = "basic_auth"
   # [global.v1.external.elasticsearch.auth.basic_auth]
   # username = "<username>"
   # password = "<password>"
   # [global.v1.external.elasticsearch.ssl]
   # root_cert_file = "</path/to/cert/file>"
   # server_name = "<elasticsearch server name>"
```
5. Configure external postgres by adding the following to the config.toml:
```
  [global.v1.external.postgresql]
  enable = true
  nodes = ["127.0.0.1:5555"]

  [global.v1.external.postgresql.auth]
  enable = true
  scheme = "password"

  [global.v1.external.postgresql.auth.password.superuser]
  username = "<pg superuser username>"
  password = "<pg superuser password>"
  [global.v1.external.postgresql.auth.password.dbuser]
  username = "<dbuser username>"
  password = "<dbuser password>"

  [global.v1.external.postgresql.backup]
  enable = true

  # Uncomment and fill out this section if using postgres with ssl
  # [global.v1.external.postgresql.ssl]
  # enable = true
  # root_cert = "</path/to/rootCA.pem>"
```
6. Make any other config changes necessary.
7. Run `chef-automate deploy --product chef-server config.toml` to install the chef
   server.

### Create a bootstrap bundle (.abb)
The bootstrap bundle contains chef server keys needed to start the second chef server.

On your original chef server node, run
   `chef-automate bootstrap bundle create </path/to/bundle.abb>`i
to create a bundle containing the necessary keys, etc., to deploy a second chef server.

### Deploy a second chef server on another node
1. Copy the bootstrap bundle to the node where you wish to deploy the second chef server.
2. Run `chef-server init-config` to generate a config.toml.
   a. Edit the config.toml to add the data collector configuration:
```
   [erchef.v1.sys.data_collector]
   enabled = false
```
   b. Edit the config.toml to use the same external elasticsearch and postgres
   configuration as the first chef server.
3. Run `chef-server deploy --product chef-server --bootstrap-bundle /path/to/bundle.abb
   config.toml` to deploy the second chef server.
4. On both chef servers, patch the config as described previously to forward data from the chef servers to
   Automate.
5. Set up external load balancing to direct traffic to both chef servers.
