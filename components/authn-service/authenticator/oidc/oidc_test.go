package oidc

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"

	"go.uber.org/zap"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/lib/tls/test/helpers"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
}

func NewTestAuthenticator(
	issuer, clientID string,
	verifier IDTokenVerifier,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	ctx, cancel := context.WithCancel(context.Background())

	return &Authenticator{
		verifier: verifier,
		ctx:      ctx,
		cancel:   cancel,
		logger:   logger,
	}, nil
}

type failingVerifier struct {
	err error
}

func newFailingVerifier(err error) *failingVerifier {
	return &failingVerifier{err: err}
}

func (v *failingVerifier) Verify(ctx context.Context, rawIDToken string) (DexIDToken, error) {
	return nil, v.err
}

type passingVerifier struct {
	idToken *mockToken
}

type mockToken struct {
	sub, connID, userID string
}

func (m *mockToken) Subject() string {
	return m.sub
}

func (m *mockToken) Claims(sink interface{}) error {
	claims := fmt.Sprintf(`{"federated_claims":{"connector_id": %q, "user_id": %q}}`,
		m.connID, m.userID)
	return json.Unmarshal([]byte(claims), sink)
}

func newPassingVerifier(idToken *mockToken) *passingVerifier {
	return &passingVerifier{idToken: idToken}
}

func (v *passingVerifier) Verify(ctx context.Context, rawIDToken string) (DexIDToken, error) {
	return v.idToken, nil
}

func TestAuthenticateExtractsIDTokenAndCallsVerifier(t *testing.T) {
	issuer := "https://mockissuer"
	clientID := "client-id"
	verifierErr := errors.New("something went wrong")
	verifier := newFailingVerifier(verifierErr)
	authn, err := NewTestAuthenticator(issuer, clientID, verifier, logger)
	if err != nil {
		t.Fatalf("failed to setup test authenticator: %v", err)
	}

	req := httptest.NewRequest("GET", "/authenticate", nil)
	req.Header.Set("Authorization", "bearer token")

	requestor, err := authn.Authenticate(req)
	if !strings.Contains(err.Error(), verifierErr.Error()) {
		t.Errorf("authenticator unexpected error: %v", err)
	}
	if requestor != nil {
		t.Errorf("authenticator unexpected requestor: %+v", requestor)
	}
}

func TestAuthenticateWhenTokenExtractionFailsReturnsError(t *testing.T) {
	issuer := "https://mockissuer"
	clientID := "client-id"
	// verifier doesn't matter as we don't reach that line. so let's just use the
	// failing one
	verifier := newFailingVerifier(errors.New("something went wrong"))
	expectedErr := "failed to extract bearer token"
	authn, err := NewTestAuthenticator(issuer, clientID, verifier, logger)
	if err != nil {
		t.Fatalf("failed to setup test authenticator: %v", err)
	}

	// Note: no authorization header
	req := httptest.NewRequest("GET", "/authenticate", nil)

	requestor, err := authn.Authenticate(req)
	if !strings.Contains(err.Error(), expectedErr) {
		t.Errorf("authenticator unexpected error: %v", err)
	}
	if requestor != nil {
		t.Errorf("authenticator unexpected requestor: %+v", requestor)
	}
}

func TestAuthenticateWhenEverythingSucceedsReturnsSubject(t *testing.T) {
	issuer := "https://mockissuer"
	clientID := "client-id"
	verifier := newPassingVerifier(&mockToken{
		sub:    "alice",
		connID: "mock-connector",
		userID: "mock-user-id"})
	authn, err := NewTestAuthenticator(issuer, clientID, verifier, logger)
	if err != nil {
		t.Fatalf("failed to setup test authenticator: %v", err)
	}

	// Note: actual token doesn't matter (we've injected an always-passing
	// verifier)
	req := httptest.NewRequest("GET", "/authenticate", nil)
	req.Header.Set("Authorization", "bearer token")

	requestor, err := authn.Authenticate(req)
	if err != nil {
		t.Errorf("authenticator unexpected error: %v", err)
	}
	if requestor == nil {
		t.Fatalf("authenticator expected requestor 'alice', got nil")
	}
	assert.Equal(t, "user:mock-connector:mock-user-id", requestor.Subject())
}

func TestCustomClientReturnsClientWithCustomTransport(t *testing.T) {
	upstream, err := url.Parse("http://traefik:81")
	if err != nil {
		t.Fatal(err)
	}

	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	client := customClient("automate-dex", upstream, serviceCerts, logger)
	if client == http.DefaultClient {
		t.Fatal("expected return not to be `http.DefaultClient`")
	}
	tr := client.Transport

	override, ok := tr.(*overrideLocalTransport)
	if !ok {
		t.Fatal("expected client's transport to be `overrideLocalTransport`")
	}

	if override.upstream != upstream {
		t.Fatalf("expected client's transport's override to be %#v, got %#v", upstream.String(), override.upstream.String())
	}
}

func TestSubject(t *testing.T) {
	tests := map[string]struct {
		r       authenticator.Requestor
		subject string
	}{
		"ldap user": {&requestor{connID: "ldap", userID: "alice"}, "user:ldap:alice"},
		"saml user": {&requestor{connID: "saml", userID: "alice"}, "user:saml:alice"},
		"local user": {&localRequestor{"alice@example.com", requestor{connID: "local", userID: "alice"}},
			"user:local:alice@example.com"},
	}
	for desc, test := range tests {
		t.Run(desc, func(t *testing.T) {
			assert.Equal(t, test.subject, test.r.Subject())
		})
	}
}

func TestTeams(t *testing.T) {
	tests := map[string]struct {
		r     authenticator.Requestor
		teams []string
	}{
		"ldap user": {&requestor{connID: "ldap", teams: []string{"admins"}}, []string{"team:ldap:admins"}},
		"saml user": {&requestor{connID: "saml", teams: []string{"admins"}}, []string{"team:saml:admins"}},
		"local user": {&localRequestor{"alice@example.com", requestor{connID: "local", teams: []string{"admins"}}},
			[]string{"team:local:admins"}},
	}
	for desc, test := range tests {
		t.Run(desc, func(t *testing.T) {
			assert.ElementsMatch(t, test.teams, test.r.Teams())
		})
	}
}
