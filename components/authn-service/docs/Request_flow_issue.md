# How to do the Request Flow?

## The Problem

So far, we've assumed that some API gateway (think nginx) would be used that is able to check a requests authenticity by sending (parts of) the request to another service -- the `authn-service` you're looking at.

However, traefik isn't able to do that, and the developer's don't appear willing to add that functionality.

### Starting point

![starting point](images/traefik_situation.png)

### Problem: How do we get a request through `authn-service` before it reaches another service?

#### `authn-service` could become a proxy

- does traefik support internal endpoints (i.e. the same load balancing, but without external access to their routes)?
  - this seems to be doable -- adding another entry point, and overriding some front-ends' entry point setting
- but would this mean it needs to know the other back-ends?
  - if set up properly, it could keep the request Path, and use that for forwarding as well
  - request routing would still happen in the internal entry point of traefik
- the downside: we suddenly develop a proxy where we had a simpler service before

![how this would look like](images/traefik_int_ext_and_authn_proxy.png)

####  `authn-service` could be turned into a traefik plugin:

![authn plugin](images/traefik_authn_plugin.png)

- this has a downside -- extending `authn-service`'s functionality (e.g. dispatching events when a user ID token is seen to auto-provision user records, etc) is more difficult with a traefik plugin
- also, this is messing up the separation of concerns quite a bit, I feel...

#### `authn-service` could be queried as part of the authorization process (policy decision)

![authn as part of authz](images/traefik_authn_as_part_of_authz.png)
- this has a downside -- the services would not know who the request was coming from -- that might be useful information

#### `authn-service` could be queried explicitly by the services

![authn queried explicitly](images/traefik_authn_queried_explicitly.png)
- this has a downside -- each service would be burdened with those steps

## Conclusion

Not considering anything outside of this document (i.e., will the future of request routing look entirely different? gRPC proxying etc.), it seems like *making `authn-service` a proxy* could be the best of these options:

1. It's independent of the API gateway/reverse proxy used:
Its only requirement is support for internal and external load balancing endpoints -- which is a lesser requirement than the authentication sub-request feature of nginx and its derivatives.

2. It also has little impact on the other services.

## Trying it

It turns out a simple reverse proxy is super-easy to set up using Go's standard library.
See https://github.com/chef/automate/components/authn-service/commit/fe4b32b8e4a5c9776881ee57c2f06f206b296b05.

## References

- [traefik: support for sub-request authorization (nginx auth_request)](https://github.com/containous/traefik/issues/391)
- [SO: How would one approach authentication with traefik?](https://stackoverflow.com/questions/44442914/how-would-one-approach-authentication-with-traefik)
