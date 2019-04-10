// +build !prod

package mock

import (
	"net/http"
	"net/url"

	"go.uber.org/zap"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/lib/tls/certs"
)

// StaticConfig is used for configuring mock-static authenticators
type StaticConfig struct {
	ExternalID string   `json:"external_id"`
	ConnID     string   `json:"conn_id"`
	UserID     string   `json:"user_id"`
	Teams      []string `json:"teams"`
	Email      string
}

// StaticAuthenticator holds the state of a mock-static authenticator
type StaticAuthenticator struct {
	externalID string
	connID     string
	userID     string
	teams      []string
	email      string
	logger     *zap.Logger
}

type staticRequestor struct {
	externalID, userID, connID string
	teams                      []string
}

type localStaticRequestor struct {
	email string
	staticRequestor
}

func (r *staticRequestor) Subject() string {
	return "user:" + r.connID + ":" + r.userID
}

func (r *staticRequestor) Teams() []string {
	teams := make([]string, len(r.teams))
	for i, team := range r.teams {
		teams[i] = "team:" + r.connID + ":" + team
	}
	return teams
}

func (l *localStaticRequestor) AppendTeams(teams []string) {
	l.teams = append(l.teams, teams...)
}

func (l *localStaticRequestor) UserID() string {
	return l.userID
}

// NewStaticAuthenticator returns a mock authenticator that always returns the
// passed requestor, its type, and connID. Only useful for dev and testing.
func NewStaticAuthenticator(externalID, connID, userID, email string, teams []string,
	logger *zap.Logger) authenticator.Authenticator {
	if connID == "" {
		connID = "mock-static"
	}
	return &StaticAuthenticator{
		externalID: externalID,
		connID:     connID,
		userID:     userID,
		teams:      teams,
		email:      email,
		logger:     logger,
	}
}

// Open returns an authentication strategy that always returns the configured
// requestor.
func (c *StaticConfig) Open(u *url.URL, _ *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	return NewStaticAuthenticator(c.ExternalID, c.ConnID, c.UserID, c.Email, c.Teams, logger), nil
}

// Authenticate processes the passed request, checking its ID token in the
// `Authorization: Bearer XY` header
func (a *StaticAuthenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	requestor := staticRequestor{
		externalID: a.externalID,
		connID:     a.connID,
		teams:      a.teams,
		userID:     a.userID,
	}
	if a.email != "" {
		return &localStaticRequestor{email: a.email, staticRequestor: requestor}, nil
	}
	return &requestor, nil
}
