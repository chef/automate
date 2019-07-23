// +build !prod

package mock

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"

	"go.uber.org/zap"
	jose "gopkg.in/square/go-jose.v2"
	"gopkg.in/square/go-jose.v2/jwt"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/lib/oidc"
	"github.com/chef/automate/lib/tls/certs"
)

// OidcConfig is used for configuring mock-oidc authenticators
type OidcConfig struct {
	Issuer   string   `json:"issuer"`
	Audience string   `json:"client_id"`
	Groups   []string `json:"groups"`
	ConnID   string   `json:"conn_id"`
	UserID   string   `json:"user_id"`
	Email    string
}

// OidcAuthenticator is used for configuring mock-oidc authenticators
type OidcAuthenticator struct {
	iss    string
	aud    string
	groups []string
	connID string
	userID string
	email  string
	logger *zap.Logger
}

type oidcRequestor struct {
	userID, connID string
	teams          []string
}

type localOIDCRequestor struct {
	email string

	oidcRequestor
}

func (l *localOIDCRequestor) Subject() string {
	return "user:local:" + l.email
}

func (l *localOIDCRequestor) AppendTeams(teams []string) {
	l.teams = append(l.teams, teams...)
}

func (l *localOIDCRequestor) UserID() string {
	return l.userID
}

func (r *oidcRequestor) Subject() string {
	switch r.connID {
	default:
		return "user:" + r.connID + ":" + r.userID
	}
}

func (r *oidcRequestor) Teams() []string {
	teams := make([]string, len(r.teams))
	for i, team := range r.teams {
		teams[i] = "team:" + r.connID + ":" + team
	}
	return teams
}

// NewAuthenticator returns a mock authenticator which requires no server
// interaction. It only checks claims and returns their name.
func NewAuthenticator(iss, aud string, groups []string,
	connID, userID, email string, logger *zap.Logger) authenticator.Authenticator {

	if connID == "" {
		connID = "mock-oidc"
	}
	if userID == "" {
		connID = "mock-user"
	}
	return &OidcAuthenticator{
		iss:    iss,
		aud:    aud,
		groups: groups,
		connID: connID,
		userID: userID,
		email:  email,
		logger: logger,
	}
}

// Open returns an authentication strategy that statically checks the iss and
// aud claims only
func (c *OidcConfig) Open(_ *url.URL, _ *certs.ServiceCerts, logger *zap.Logger) (authenticator.Authenticator, error) {
	return NewAuthenticator(c.Issuer, c.Audience, c.Groups, c.ConnID, c.UserID, c.Email, logger), nil
}

// Authenticate processes the passed request, checking its ID token in the
// `Authorization: Bearer XY` header
func (a *OidcAuthenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	token, err := oidc.ExtractBearerToken(r)
	if err != nil {
		return nil, err
	}

	parts := strings.SplitN(token, ".", 3)
	if len(parts) != 3 {
		return nil, fmt.Errorf("failed decoding ID token payload '%v'", parts)
	}
	b, err := decodePayload(parts[1])
	if err != nil {
		return nil, fmt.Errorf("failed decoding ID token payload: %v", err)
	}

	cl := jwt.Claims{}
	if err := json.Unmarshal(b, &cl); err != nil {
		return nil, fmt.Errorf("failed decoding ID token as JSON: %v", err)
	}

	// Note 2017/08/01 (sr): We could validate more here, but this mock
	// authenticator should still be simple and useful
	expected := jwt.Expected{
		Issuer:   a.iss,
		Audience: jwt.Audience{a.aud},
	}
	if err := cl.Validate(expected); err != nil {
		return nil, err
	}

	if a.connID == constants.LocalConnectorID {
		return &localOIDCRequestor{
			email:         a.email,
			oidcRequestor: oidcRequestor{userID: a.userID, teams: a.groups, connID: a.connID},
		}, nil
	}

	return &oidcRequestor{userID: a.userID, teams: a.groups, connID: a.connID}, nil
}

func decodePayload(b64 string) (val []byte, err error) {
	val, err = base64.RawURLEncoding.DecodeString(b64)
	return
}

// GenerateMockJWT generates a JWT ID token containing the passed sub/iss/aud
// claims. It is signed using HS256 with the secret "sharedsecret" -- only to be
// used for mocking tests.
func GenerateMockJWT(subject, issuer, audience string) string {
	c := &jwt.Claims{
		Subject:  subject,
		Issuer:   issuer,
		Audience: jwt.Audience{audience},
	}
	signer := mustMakeSigner([]byte("sharedsecret"))
	raw, err := jwt.Signed(signer).Claims(c).CompactSerialize()
	if err != nil {
		panic(err)
	}
	return raw
}

func mustMakeSigner(k interface{}) jose.Signer {
	sig, err := jose.NewSigner(jose.SigningKey{Algorithm: jose.HS256, Key: k}, nil)
	if err != nil {
		panic("failed to create signer:" + err.Error())
	}

	return sig
}
