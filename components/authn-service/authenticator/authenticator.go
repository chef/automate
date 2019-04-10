package authenticator

import (
	"net/http"
)

// Authenticator is a mechanism for authenticating requests
//
// Implementations are expected to implement the Authenticator interface
type Authenticator interface {
	Authenticate(*http.Request) (Requestor, error)
}

// Requestor represents the result of authenticating a request. An
// Authenticator has the following options for returning the result:
// - if error is not nil, something went wrong
// - if error is nil, but Requestor is nil, too, this Authenticator wasn't able
//   to authenticate this request (the server thus goes on trying the other
//   Authenticators)
// - if error is nil, and Requestor is not, this is taken to be the request's
//   (authenticated) sender, and returned by the server.
type Requestor interface {
	// Subject returns the namespaced string representation of the authenticated
	// requestor ("user:local:some-id" or "token:some-id")
	Subject() string

	// Teams returns the namespaced array of teams, if present (tokens don't have any)
	Teams() []string
}

// LocalUser allows for changing their teams, and only local users implement
// this interface.
// Note 2018/03/15 (sr): unfortunately, this implies quite a bit of work in
// having the mock authenticators work for local/non-local user situations.
type LocalUser interface {
	// UserID allows for retrieving the UserID of a local user, to be used for
	// fetching their local teams
	UserID() string
	// AppendTeams allows adding local teams to the exiting teams. It expects
	// non-namespaced team names, e.g. "admins", NOT "team:local:admins"
	AppendTeams([]string)
}
