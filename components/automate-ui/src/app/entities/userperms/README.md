# Accessing Introspection Data for API methods

When code makes any call to the backend, e.g.
"/api/v0/cfgmgmt/stats/run_counts", the system checks whether the current user
has permission to access that endpoint.  If so, the call proceeds; otherwise, a
403 error is returned.

While that provides a robust security model, it does not by itself provide
a robust user experience. In many cases it is useful to know a priori
what calls on a given view one can make before deciding what to render.

The backend provides an introspection endpoint where one may query
what the user has permissions for.

The endpoint is "/api/v0/auth/introspect" and supports two methods:

1. GET - Does an "IntrospectAll", returning data for all non-parameterized endpoints
   (i.e. concrete endpoints like "/apis/iam/v2/teams" but not abstract endpoints like "/apis/iam/v2/users/{email}").
2. POST - Does an "Introspect", returning data for the single endpoint path specified in the payload
   (an example payload might be "path=/apis/iam/v2/users/foo@bar.com" corresponding to "/apis/iam/v2/users/{email}").

Data from either call returns the same structure:

```json
{
  endpoints: {
    <endpoint_path>: {
      "get": <boolean>,
      "put": <boolean>,
      "post": <boolean>,
      "delete": <boolean>
    },
    <endpoint_path>: {
      "get": <boolean>,
      "put": <boolean>,
      "post": <boolean>,
      "delete": <boolean>
    },
    . . .
  }
}
```

Example:

```json
{
  "endpoints": {
    "/apis/iam/v2/tokens": { "delete": false, "get": true, "post": true, "put": false },
    "/apis/iam/v2/policies": { "delete": false, "get": true, "post": true, "put": false },
    "/api/v0/cfgmgmt/stats/run_counts": { "delete": false, "get": true, "post": false, "put": false },
    "/api/v0/cfgmgmt/suggestions": { "delete": false, "get": true, "post": false, "put": false },
    "/api/v0/cfgmgmt/version": { "delete": false, "get": true, "post": false, "put": false }
    . . .
  }
}
```

Dispatching the `getPerms()` action (userperms.actions.ts) is wired up to invoke the `IntrospectAll` endpoint.
