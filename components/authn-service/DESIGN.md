# Design document

## How authn-service works (What)

### Request authentication

When this service is a request, it tries to _authenticate_ the request's sender, and informs the requestor of that try's outcome.

The determination of the request's sender works by checking three things (order *TBD*):

- JWT id token from `Authorization: Bearer XY` header
- service account credentials from *TBD* (http basic/digest auth? some header?)
- api client credentials from *TBD* (http basic/digest auth? some header?)
- - First pass of this is client sending `Client-Id` and `Client-Encoding`, where the encoding is 
    the client-side encoding of the client id, to be checked server side using the shared secret (e.g. `curl -v -H "Client-ID: SomeClient" -H "Client-Encoding: correctlyencodedclientid" http://localhost:8080/authenticate`)

#### Response

A success response includes headers indicating which sender was determined:

```
X-Requestor: user:alice
```

```
X-Requestor: service:inspec/dc-west-1
```

```
X-Requestor: client:alice/profile-uploader-ci
```

Note that the exact naming scheme is *TBD* as it is what identifies users with our authz-service.

## Explanations (Why)

Explanations of why things are done the way they are.

### Why does it use HTTP headers for communicating the result of request authentication?

One way this could be used is via [nginx' `access_by_lua_*` functionality](https://github.com/openresty/lua-nginx-module#access_by_lua_block):

A Lua block would be put into its configuration that will be called on each request, and determines if the request is "allowed" (in this case, it's only authentication that matters), and pass it on:

```lua
location / {
   access_by_lua '
       ngx.req.read_body()
       local res = ngx.location.capture("/authn") -- this is the authn-service

       if res.status == ngx.HTTP_OK then
           -- left out: process response body
           ngx.log(ngx.CRIT, res.body)
           -- take header from response, put it in the request that is forwarded
           ngx.req.set_header("x-authn-response", res.header["x-requestor"])
           return
       end

       if res.status == ngx.HTTP_FORBIDDEN then
           ngx.exit(res.status)
       end

       ngx.exit(ngx.HTTP_INTERNAL_SERVER_ERROR)
   ';

   # proxy_pass/fastcgi_pass/postgres_pass/...
}
```

#### Note about Nginx

~It's conceivable that we don't end up using nginx, so this service _might become a proxy_ itself.~

⏩ **It is a proxy now, too!**

### Why proxying?

While looking for ways to reconcile our initial plan (see [Request flow issue](docs/Request_flow_issue.md)) with the planned deployment model of Automate 2.0, which puts an emphasis on a fairly limited, little-complexity frontend service, [Træfik](https://traefik.io/), the issue came up that we might **just do the proxying ourselves**.

Since it was both fairly easy to get up and running in the service, and results in a deployment with low complexity (no nginx, no upstreams, location lines with regexp matches, lua handlers...), it was decided that it's at least worth trying.
From the perspective of authn-service's code, it doesn't matter much, so keep both interfaces alive for now -- with the intention to kill the one we don't end up using later on.


### Why do we not pass the JWT ID token to every service?

There's two reasons for that.

For one thing, it's meant as a simplification for the backend services --
To verify the JWT token, you need to synchronize the OIDC provider's public key set, and besides that, there's a few things that can go wrong.
Having one place that does the ID token verification (correctly) seems preferable.

The bigger reason, though, is supporting non-human clients and service accounts.

![diagram](docs/images/initial_situation.png)

Both of these are not necessarily tied to any human user, and going through Dex' login process would be cumbersome.

An alternative to the approach taken here would have been to make Dex aware of non-human clients, e.g. by adding them as local users, and adding a client credential flow feature.

![diagram](docs/images/dex_changed_only_situation.png)

However, that's not what that flow is made for (its "clients" are supposed to be OIDC clients, which is different from our use case, I believe).
Another alternative would be to change the authz-service to take care of the same business.

![diagram](docs/images/authz_service_changed_only_situation.png)

All things considered, this approach seems preferable because we are decoupled from both the OIDC provider and the authz-service used.
We could switch either one and don't have to re-implement all the ways we'd like to be able to authenticate requests.
However, it also means that for every downstream service we use (and authz-service may be among them), we'll have to make them be able to consume our _one special header_.

![diagram](docs/images/authn_service_situation.png)
