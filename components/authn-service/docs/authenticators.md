## Definitions

### Authenticator
The interface that implements the Authenticate function. This function returns
 a Requestor if the user was successfully authenticated or an error if not.
If the Authenticator returns an error, then it failed to run and the request
was invalid.
If it returns nil and no Requestor, then the Authenticator ran but
failed to authenticate.
If it returns a Requestor without an error, then the user was authenticated
successfully and the returned Requestor is taken to be the request's Requestor.
Once a Requestor is returned, the Authn service breaks out of the Authenticators
loop and runs the hooks.

The Authn service will iterate through as many Authenticators as configured;
if no Authenticator returns a Requestor successfully, it returns 401.

### Credentials Authenticator
The Credentials Authenticator is used to authenticate non-human actors. It has
a store that maps tokens to IDs per actor. ClientEncoding is decoded using the
token corresponding to the ClientIdentifier.
If the ClientEncoding matches the server's encoding, then the ClientIdentifier
was encoded with the correct token and a Requestor is returned.
If the ClientEncoding does not match the server's encoding, then the
ClientIdentifier used a different token, so no Requestor is returned.

ClientIdentifier is sent up as `Client-ID` and ClientEncoding is sent up as
`Client-Encoding`.

### Requestor
The Requestor represents the authenticated actor (user or client), which
contains information, like an ID, about the actor. Using the information
provided by the Requestor, Authn service sets the identity header.

### Hooks
Authn runs the hooks after the user is authenticated, taking the returned
Requestor as an argument. The Hook interface accepts a Context and a Requestor.
