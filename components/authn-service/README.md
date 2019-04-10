# authn service

## Responsibilities of this service

### Request authenticity

This service is used for checking a request's authenticity.

For **non-human clients**, it has to ensure that the request originates from a security principal
we know:

This works by checking that the requests has the correct headers (or basic auth, some other mechanism),
as per our local credentials database.

For all other requests (those with an active human in the mix), it tries to validate the ID token,
and if that fails (or there's no ID token present), it redirects to the internal OIDC identity
provider (currently dex).

#### Configuration

Authenticating requests works by configuring a _set of authenticators_, via config.yml:

```yaml
authenticators:
# proper OIDC: check signature, expiry, audience, and issuer
- id: dex
  type: oidc
  config:
    issuer: http://127.0.0.1:5556/dex
    client_id: example-app
# same as oidc, but doesn't interact with the server
# merely checks some token details (iss, aud)
# => can be used without dex, but looks very similar to the setup that is
#    using dex
- id: oidc-mock
  type: mock-oidc
  config:
    issuer: http://127.0.0.1:5556/dex
    client_id: example-app
# static mock: authenticate every request as coming from foobear
# ⚠️ only for testing and development ️⚠️ -- if this in authenticators,
# authentication never fails
- id: static
  type: mock-static
  config:
    requestor: foobear
```

### React to authenticating users

**Note**: We'll have to mirror the group settings (for external groups), but this still TODO.

## Mode of Operation

The service operates as a (reverse) proxy:

```yaml
upstream: http://traefik/internal # reverse proxy this host...
proxy_http: 0.0.0.0:9090          # ...on this endpoint
```

Currently, it does _not_ persist any state on the local filesystem.


## How to use it

### CLI interface

This service consists in a single executable, `authn`, with the following command line interface:

```
$ ./authn
Usage:
  authn [flags]
  authn [command]

Available Commands:
  help        Help about any command
  serve       Begin serving requests.
  version     Print the version and exit

Flags:
  -h, --help   help for authn

Use "authn [command] --help" for more information about a command.
```

## Building and Running within Habitat studio

This requires working installations of [habitat](https://habitat.sh) and [direnv](https://direnv.net/).
If you haven't ever done that before, setup your Habitat CLI environment:

    $ hab cli setup

Then, you can use the studio as development environment:

    $ hab studio enter

## Building and running outside of Habitat studio

    Please note, we advise you run this service in Habitat studio, which we are using as our working dev environment.

### Building and running the tests

    $ make test

### Building and running the service

_This example uses the mock-credentials authenticator example config, but there are other config files
that configure other authenticators in the examples directory._

    $ make build && ./authn serve examples/config-mock-credentials.yml

### Authentication

The authentication API works analogous to HTTP header-based auth -- it's checking the
metadata coming along with the request.

```
$ grpc_cli call 127.0.0.1:9091 auth.Authentication.Authenticate ''
connecting to 127.0.0.1:9091
Rpc failed with status code 16, error message: request not authenticated
```

To send a GRPC call with metadata, use this for client tokens, based on the
api-token or x-data-collector-token header (assuming the service is configured to use that,
which it should be for the immediately foreseeable future),

```
$ grpc_cli call 127.0.0.1:9091 auth.Authentication.Authenticate '' --metadata "api-token:_o29rYivBitJ7peJKIX3cBuJXnU="
connecting to 127.0.0.1:9091
Sending client initial metadata:
api-token : _o29rYivBitJ7peJKIX3cBuJXnU=
actor: "0920b880-a8bf-48a2-be51-ef23c4994fe4"

Rpc succeeded with OK status
```

and that for adding a JWT ID token:

```
$ export TOK="eyJhb..." # your token
$ grpc_cli call 127.0.0.1:9091 auth.Authentication.Authenticate '' --metadata "authorization:bearer $TOK"
```

If your request is meant to go through automate-gateway's GRPC endpoint, you'll have to ensure the
proper metadata headers are set, by using the same arguments outlined above.

#### HTTP1 endpoint

While not available as a public API in A2 (so no entry in the load balancer), the authenticate API is also exposed via HTTP1, accessible at /api/authenticate, for use by other A2 services (e.g. the Workflow back end).

```
[234][default:/src:0]# curl -v -H "api-token: $TOK" http://0.0.0.0:10555/api/authenticate
*   Trying 0.0.0.0...
* TCP_NODELAY set
* Connected to 0.0.0.0 (127.0.0.1) port 10555 (#0)
> GET /api/authenticate HTTP/1.1
> Host: 0.0.0.0:10555
> User-Agent: curl/7.54.1
> Accept: */*
> api-token: 4_VoqDUP8wMV0jL9EvHMqTJc18A=
>
< HTTP/1.1 200 OK
< Content-Type: application/json
< Grpc-Metadata-Content-Type: application/grpc
< Date: Tue, 14 Aug 2018 17:40:28 GMT
< Content-Length: 56
<
* Connection #0 to host 0.0.0.0 left intact
{"subject":"token:c0a812fb-f2d3-4a3a-9c19-88250c590ff9"}
```
