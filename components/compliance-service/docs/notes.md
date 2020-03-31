## working with compliance service in A2 (built)

#### get logs:
```
sudo journalctl -fu chef-automate | grep compliance-service
or
chef-automate system-logs
```

#### change config settings:
```
vi config.toml


[compliance.v1.sys.logger]
level = "debug"

[compliance.v1.sys.nodemanager]
awsec2_polling_interval = 60
azurevm_polling_interval = 180
manual_polling_interval = 1440

[compliance.v1.sys.agent]
workers = 50
buffer_size = 1000


sudo chef-automate config patch config.toml
```

#### look up what indices are in elastic:
```
curl -s http://localhost:10141/_cat/indices/comp*
```

#### delete all indices in elastic:
```
curl -s http://localhost:10141/_cat/indices/comp*?h=i | while read compliance_index; do curl -X DELETE http://localhost:10141/$compliance_index; done

sudo chef-automate restart-services
```

#### get a token for api calls
```
sudo chef-automate iam token create <token-name> --admin
```

## working with compliance service in A2 (dev mode/hab studio)

#### all the above commands are available while working in dev mode (hab studio), here are a few extras...

#### get logs from another terminal window:
```
tail -f /hab/studios/src/hab/sup/default/sup.log
```

#### login to the database (postgres):
```
chef-automate dev psql
```
