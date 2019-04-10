# Chef Automate Assigned Numbers Authority

## Port Assignment Policy

Where possible we try to choose ports in the following range:

    10113-10162

When adding a service, choose a port from those marked UNUSED in the
list below.  Add the name of the service it will use used for. If
appropriate, also add it to the list of ports to be checked in

pkg/preflight/checks.go

## Port Assignments

```
|  Port | Service/Use                  |
|-------+------------------------------|
|    80 | automate-load-balancer       |
|   443 | automate-load-balancer       |
|  2000 | automate-gateway             |
|  2001 | automate-gateway             |
|  5432 | automate-postgresql          |
|  2192 | ingest-service               |
|  9631 | hab-sup                      |
|  9638 | hab-sup                      |
| 10100 | pg-sidecar-service           |
| 10113 | authn-service                |
| 10114 | UNUSED (prev. authn-service) |
| 10115 | session-service              |
| 10116 | automate-dex                 |
| 10117 | automate-dex                 |
| 10118 | UNUSED                       |
| 10119 | config-mgmt-service          |
| 10120 | nodemanager-service          |
| 10121 | compliance-service           |
| 10122 | ingest-service               |
| 10123 | es-sidecar-service           |
| 10124 | license-control-service      |
| 10125 | notifications-service        |
| 10126 | UNUSED                       |
| 10127 | local-user-service           |
| 10128 | teams-service                |
| 10129 | data-lifecycle-service       |
| 10130 | authz-service                |
| 10131 | secrets-service              |
| 10132 | event-service                |
| 10133 | applications-service         |
| 10134 | UNUSED                       |
| 10135 | UNUSED                       |
| 10136 | UNUSED                       |
| 10137 | UNUSED                       |
| 10138 | UNUSED                       |
| 10139 | UNUSED                       |
| 10140 | NATS Server (event-service)  |
| 10141 | automate-elasticsearch       |
| 10142 | automate-elasticsearch       |
| 10143 | backup-gateway               |
| 10144 | automate-es-gateway          |
| 10145 | automate-pg-gateway          |
| 10146 | automate-pg-gateway          |
| 10147 | UNUSED                       |
| 10148 | UNUSED                       |
| 10149 | UNUSED                       |
| 10150 | UNUSED                       |
| 10151 | UNUSED                       |
| 10152 | UNUSED                       |
| 10153 | UNUSED                       |
| 10154 | UNUSED                       |
| 10155 | UNUSED                       |
| 10156 | UNUSED                       |
| 10157 | UNUSED                       |
| 10158 | UNUSED                       |
| 10159 | UNUSED                       |
| 10160 | automate-deployment          |
| 10161 | automate-ui                  |
| 10162 | authn-service                |
| 10200 | automate-cs-nginx            |
| 10201 | automate-cs-bookshelf        |
| 10202 | automate-cs-oc-bifrost       |
| 10203 | automate-cs-oc-erchef        |
| 10204 | automate-cs-nginx            |
```
