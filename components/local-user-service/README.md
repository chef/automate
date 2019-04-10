## Local User Service

This service is a simple go GRPC wrapper around [Dex's password GRPC API](https://github.com/dexidp/dex/blob/master/api/api.proto).
We are using it for local user creation and management currently.

### Local User Management

Most users will be managed by external identity providers, but AuthN also provides for defining users locally.
AuthN is the source of truth for these locally defined users, and is responsible for their verification against a locally persisted password.

### Password Validation

When creating a user, or updating an existing user's password, the service performs minimal password policy validation.

The requirements are:
- at least 8 characters
- at least 3 unique characters (so no "aabbaabb")

If the password validation fails, the GRPC method will return `status.InvalidArgument`, which translates into HTTP status code 400 when used through grpc-gateway (which powers automate-gateway).
