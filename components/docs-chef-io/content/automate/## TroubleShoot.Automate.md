


## TroubleShoot

1. In case of Restore fails with below error 
```
 level=error msg="Failed to load metadata for service automate-postgresql" backup_id=20220825075554 error="error opening backup metadata key automate-postgresql/metadata.json: blob (key \"08-25-22/20220825075554/automate-postgresql/metadata.json\") (code=Unknown): InvalidAccessKeyId: The Access Key Id you provided does not exist in our records.\n\tstatus code: 403, request id: 170E87BC29DEE874, host id: " restore_id=20220825075936
 level=error msg="Failed to restore services" backup_id=20220825075554 error="error opening backup metadata key automate-postgresql/metadata.json: blob (key \"08-25-22/20220825075554/automate-postgresql/metadata.json\") (code=Unknown): InvalidAccessKeyId: The Access Key Id you provided does not exist in our records.\n\tstatus code: 403, request id: 170E87BC29DEE874, host id: " restore_id=20220825075936
 ```
  > Resolution 
- Go to the Standalone automate 
lkl


```
 /hab/svc/backup-gateway/run: line 41: /var/opt/chef-automate/backups/.tmp/.creds: Permission denied
 /hab/svc/backup-gateway/run: line 42: /var/opt/chef-automate/backups/.tmp/.creds: Permission denied
time="2022-08-25T08:12:03Z" level=warning msg="service backup-gateway has PID 0 with state OK" 
```

```
level=error msg="Failed to restore services" backup_id=20220825075554 error="es-sidecar-service failed to restore snapshot: rpc error: code = Unavailable desc = connection error: desc = \"transport: Error while dialing dial tcp 127.0.0.1:10123: connect: connection refused\"" restore_id=20220825081109
level=info msg="finished streaming call with code OK" grpc.code=OK grpc.method=DeployStatus grpc.request.deadline="2022-08-25T20:11:08Z" grpc.service=chef.automate.domain.deployment.Deployment grpc.start_time="2022-08-25T08:11:59Z" grpc.time_ms=297326.8 span.kind=server system=grpc
level=fatal msg="Failed to start ES Watcher" error="failed to create elasticsearch client: Failed to create ES client: elastic: Error 401 (Unauthorized)"
```


```
Aug 25 09:09:26 ip-10-1-0-122 hab[4278]: deployment-service.default(O): time="2022-08-25T09:09:26Z" level=error msg="Failed to restore synchronous operations" backup_id=20220825075554 error="es-sidecar-service failed to restore snapshot: rpc error: code = Unknown desc = failed to create snapshot repository: elasticsearch repository create request failed for repo \"chef-automate-es6-event-feed-service\"; request data: '{s3 map[base_path:08-25-22/automate-elasticsearch-data/chef-automate-es6-event-feed-service bucket:a2ha-bloomberg compress:false]}': elastic: Error 500 (Internal Server Error): [chef-automate-es6-event-feed-service] path [08-25-22/automate-elasticsearch-data/chef-automate-es6-event-feed-service] is not accessible on master node [type=repository_verification_exception]" restore_id=20220825090512
Aug 25 09:09:26 ip-10-1-0-122 hab[4278]: deployment-service.default(O): time="2022-08-25T09:09:26Z" level=error msg="Failed to restore services" backup_id=20220825075554 error="es-sidecar-service failed to restore snapshot: rpc error: code = Unknown desc = failed to create snapshot repository: elasticsearch repository create request failed for repo \"chef-automate-es6-event-feed-service\"; request data: '{s3 map[base_path:08-25-22/automate-elasticsearch-data/chef-automate-es6-event-feed-service bucket:a2ha-bloomberg compress:false]}': elastic: Error 500 (Internal Server Error): [chef-automate-es6-event-feed-service] path [08-25-22/automate-elasticsearch-data/chef-automate-es6-event-feed-service] is not accessible on master node [type=repository_verification_exception]" restore_id=20220825090512
Aug 25 09:09:26 ip-10-1-0-122 hab[4278]: deployment-service.default(O): time="2022-08-25T09:09:26Z" level=info msg="finished streaming call with code OK" grpc.code=OK grpc.method=DeployStatus grpc.request.deadline="2022-08-25T21:05:11Z" grpc.service=chef.automate.domain.deployment.Deployment grpc.start_time="2022-08-25T09:05:57Z" grpc.time_ms=208355.36 span.kind=server system=grpc
Aug 25 09:09:33 ip-10-1-0-122 hab[4278]: automate-ui.default(O): - [25/Aug/2022:09:09:33 +0000]  "HEAD / HTTP/1.1" 200 "0.000" 0 "-" "curl/7.79.1" "-" "-" "-" 82
Aug 25 09:09:50 ip-10-1-0-122 hab[4278]: deployment-service.default(O): time="2022-08-25T09:09:50Z" level=warning msg="Skipping periodic converge because disable file is present" file=/hab/svc/deployment-service/data/converge_disable
```
Go to the opensearch cluster and do the curl request 