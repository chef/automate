package tokens

import (
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"go.uber.org/zap"

	tokenmock "github.com/chef/automate/components/authn-service/tokens/mock"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	uuid "github.com/chef/automate/lib/uuid4"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
}

func TestTokenAuthWithBackend(t *testing.T) {
	tokenID := uuid.Must(uuid.NewV4()).String()
	tests := []struct {
		name       string
		request    func(*http.Request)
		tokens     []*tokens.Token
		headers    []string
		expectFail bool
		subject    string
	}{
		{
			name:    "when token matches",
			headers: []string{"x-foo"},
			tokens: []*tokens.Token{
				{ID: tokenID, Value: "foobear", Active: true},
			},
			request: func(r *http.Request) {
				r.Header.Set("x-foo", "foobear")
			},
			subject: "token:" + tokenID,
		},
		{
			name:    "when token matches, but uses odd header case",
			headers: []string{"x-foo"},
			tokens: []*tokens.Token{
				{ID: tokenID, Value: "foobear", Active: true},
			},
			request: func(r *http.Request) {
				r.Header.Set("x-FOo", "foobear")
			},
			subject: "token:" + tokenID,
		},
		{
			name:    "when token matches second configured header",
			headers: []string{"x-foo", "x-bar"},
			tokens: []*tokens.Token{
				{ID: tokenID, Value: "foobear", Active: true},
			},
			request: func(r *http.Request) {
				r.Header.Set("x-bar", "foobear")
			},
			subject: "token:" + tokenID,
		},
		{
			name:    "when token does not match",
			headers: []string{"x-foo"},
			tokens: []*tokens.Token{
				{ID: tokenID, Value: "foobear"},
			},
			request: func(r *http.Request) {
				r.Header.Set("x-foo", "xys")
			},
			expectFail: true,
		},
		{
			name:    "when token does not exist",
			headers: []string{"x-foo"},
			tokens:  []*tokens.Token{},
			request: func(r *http.Request) {
				r.Header.Set("x-foo", "xys")
			},
			expectFail: true,
		},
	}

	for _, d := range tests {
		t.Run(d.name, func(t *testing.T) {
			u, _ := url.Parse("https://upstream")

			cfg := &HeaderTokenConfig{
				Headers: d.headers,
				Storage: StorageConfig{
					Type: "mock",
					Config: &tokenmock.Config{
						Tokens: d.tokens,
					},
				},
			}
			authn, err := cfg.Open(u, nil, logger)
			if err != nil {
				t.Fatal(err)
			}

			r := httptest.NewRequest("GET", "/whatever", nil)
			if d.request != nil {
				d.request(r)
			}

			actual, err := authn.Authenticate(r)
			if d.expectFail && err == nil {
				t.Error("expected failure, got err == nil")
			}
			if !d.expectFail && err != nil {
				t.Errorf("expected no failure, got err=%v", err)
			}
			if !d.expectFail {
				if got, want := actual.Subject(), d.subject; got != want {
					t.Errorf("requestor.Subject() got!=want: got %v, want %v", got, want)
				}
			}
		})
	}
}
