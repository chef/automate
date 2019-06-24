# Teams Service

Teams Service is an API for defining Local Teams that will be used as part of
the new Authorization model for A2. Whether the whole AuthZ API will live in
here or just the Local Teams part is subject to debate :)

## Getting Started

### Running Tests

To run tests against an in-memory store, simply run:

```
make test
```

If you wish to run the tests against postgres, first spin a pg docker container up:

```
make setup_docker_pg
```

Once up, you can run the tests over and over:

```
make test_with_db
```

If you wish to run the tests in VSCode or elsewhere, set the following in the proper environment:

```
export PG_URL="postgresql://postgres@127.0.0.1:5432/teams_test?sslmode=disable&timezone=UTC"
```

Once you are done testing against the database, kill the container
and unset `PG_URL` if you previously set it:

```
make kill_docker_pg
unset PG_URL
```

** Note: CI will always run both in-memory and postgres tests. **

### Spinning up the GRPC Service

In the root of the repo:

```
dep ensure -vendor-only
```

In this directory:

```
$ make start
```

Hit the GRPC server:

```
$ grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key localhost:9093 chef.automate.domain.teams.Teams/GetVersion

<Version Response>
```
NOTE: In a deployed habitat environment (using for example, this workflow: https://github.com/chef/automate/blob/master/docs/DEV_ENVIRONMENT.md#using-the-hab-studio-from-a-vm-via-vagrant) you can see the port teams is listening on by running `cat /hab/svc/teams-service/config/config.yml`.

### Testing through Automate Gateway

While `teams-cli` is still running, spin up the gateway in its own terminal:

```
cd components/automate-gateway
make run
```

This will compile and run the gateway. Once the gateway starts, it will listen
to HTTP requests on port `2000` and forward them to our GRPC service. You can
hit the gateway via curl:

```
$ curl --insecure -H "Authorization: Bearer dev" https://localhost:2000/auth/teams/version
{"name":"teams-service","version":"0.0.1","sha":"TODO","built":"TODO"}
```

### API

#### CreateTeam()

```
++> echo '{"name": "Test Team", "description": "This is my favorite team."}' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/CreateTeam

id: "da39a3ee5e6b4b0d3255bfef95601890afd80709"
name: "Test Team"
description: "This is my favorite team."
```

```
curl -H "Authorization: Bearer dev" -H "Content-Type: application/json" -d '{"name":"Test Team", "description":"This my favorite team."}' https://localhost:2000/auth/teams --insecure
```

#### GetTeams()

```
++> grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key localhost:9093 chef.automate.domain.teams.Teams/GetTeams

teams {
  id: <TEAM ID>
  name: "Test Team"
  description: "This is my favorite team."
}
```

```
curl -H "Authorization: Bearer dev" -H "Content-Type: application/json" https://localhost:2000/auth/teams --insecure
```

#### GetTeam()

```
echo '{"id": <TEAM ID>"}' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key localhost:9093 chef.automate.domain.teams.Teams/GetTeam

id: <TEAM ID>
name: "FAVORITE Team"
description: "This is my favorite team."

// due to grpcurl omitting empty returns by default, user_ids will only display if users have been added to the team
```

```
curl -H "Authorization: Bearer dev" -H "Content-Type: application/json" https://localhost:2000/auth/teams/<TEAM ID> --insecure
```

#### UpdateTeam()

```
echo '{"id": "<TEAM ID>", "name": "FAVORITE TEAM", "description": "This really is my favorite team."}' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/UpdateTeam

id: <TEAM ID>
name: "FAVORITE TEAM"
description: "This really is my favorite team."
```

```
curl -X PUT -H "Authorization: Bearer dev" -H "Content-Type: application/json" -d '{"name":"FAVORITE TEAM", "description":"This really my favorite team."}' https://localhost:2000/auth/teams --insecure
```

#### DeleteTeam()

```
echo '{"id": "<TEAM ID>"}' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/DeleteTeam

id: <TEAM ID>
name: "FAVORITE Team"
description: "This is my favorite team."
```

```
curl -X DELETE -H "Authorization: Bearer dev" -H "Content-Type: application/json" https://localhost:2000/auth/teams/<TEAM ID> --insecure
```

#### AddUsers()

```
echo '{"id": <TEAM ID>, "user_ids":["1ba7b810-9dad-11d1-80b4-00c04fd430c8", "2ba7b810-9dad-11d1-80b4-00c04fd430c8"] }' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/AddUsers

id: <TEAM ID>
name: "FAVORITE Team"
description: "This is my favorite team."
userIds: [
      "1ba7b810-9dad-11d1-80b4-00c04fd430c8",
      "2ba7b810-9dad-11d1-80b4-00c04fd430c8"
    ]
```

```
curl -X POST -H "Authorization: Bearer dev" -H "Content-Type: application/json" -d '{"user_ids":["1ba7b810-9dad-11d1-80b4-00c04fd430c8", "2ba7b810-9dad-11d1-80b4-00c04fd430c8"]}' https://localhost:2000/auth/teams/<TEAM ID>/users --insecure
```

#### RemoveUsers()

```
echo '{"id": <TEAM ID>, "user_ids":["1ba7b810-9dad-11d1-80b4-00c04fd430c8"] }' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/RemoveUsers

id: <TEAM ID>
name: "FAVORITE Team"
description: "This is my favorite team."
userIds: [
      "2ba7b810-9dad-11d1-80b4-00c04fd430c8"
    ]
```

```
curl -X PUT -H "Authorization: Bearer dev" -H "Content-Type: application/json" -d '{"user_ids":["1ba7b810-9dad-11d1-80b4-00c04fd430c8"]}' https://localhost:2000/auth/teams/<TEAM ID>/users --insecure
```

#### PurgeUserMembership()
This deletes a given user ID from every team it belongs to and returns a list of those teams. Not exposed via HTTP

```
echo '{"user_id": "2ba7b810-9dad-11d1-80b4-00c04fd430c8"}' | grpcurl -v -insecure -cert dev/certs/teams-service.crt --key dev/certs/teams-service.key -d @ localhost:9093 chef.automate.domain.teams.Teams/PurgeUserMembership

ids: ["d6b2a6cd-217e-4a61-bcbc-2db168dd6b77"]
```
