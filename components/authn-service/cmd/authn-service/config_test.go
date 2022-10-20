package main

import (
	"testing"

	"github.com/ghodss/yaml"
	"github.com/kylelemons/godebug/pretty"

	"github.com/chef/automate/components/authn-service/authenticator/mock"
	"github.com/chef/automate/components/authn-service/authenticator/oidc"
	tokenauthn "github.com/chef/automate/components/authn-service/authenticator/tokens"
	tokenmock "github.com/chef/automate/components/authn-service/tokens/mock"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	uuid "github.com/chef/automate/lib/uuid4"
)

var _ = yaml.YAMLToJSON

func TestUnmarshalConfig(t *testing.T) {
	tokenID := uuid.Must(uuid.NewV4()).String()
	tests := map[string]struct {
		rawConfig      []byte
		expectedConfig Config
	}{
		"mock adapters": {
			rawConfig: []byte(`
upstream: http://127.0.0.1:1982
logger:
  format: json
  level: debug
authenticators:
- id: static
  type: mock-static
  config:
    external_id: groundhog
- id: foo-mock
  type: mock-oidc
  config:
    issuer: https://oidc.server/path
    client_id: client-id
- id: foo
  type: oidc
  config:
    issuer: https://oidc.server/path
    client_id: client-id
- id: foo-mock-header-token
  type: mock-header-token
  config:
    header: x-whatever
    tokens:
      token1: secret1
      token2: secret2
`),
			expectedConfig: Config{
				Upstream: "http://127.0.0.1:1982",
				Logger:   Logger{Level: "debug", Format: "json"},
				Authenticators: []Authenticator{
					{
						Type: "mock-static",
						ID:   "static",
						Config: &mock.StaticConfig{
							ExternalID: "groundhog",
						},
					},
					{
						Type: "mock-oidc",
						ID:   "foo-mock",
						Config: &mock.OidcConfig{
							Issuer:   "https://oidc.server/path",
							Audience: "client-id",
						},
					},
					{
						Type: "oidc",
						ID:   "foo",
						Config: &oidc.Config{
							Issuer:   "https://oidc.server/path",
							ClientID: "client-id",
						},
					},
					{
						Type: "mock-header-token",
						ID:   "foo-mock-header-token",
						Config: &mock.HeaderTokenConfig{
							Header: "x-whatever",
							Tokens: map[string]string{"token1": "secret1", "token2": "secret2"},
						},
					},
				},
			},
		},
		"header-adapter-with-backends": {
			rawConfig: []byte(`
upstream: http://127.0.0.1:1982
authenticators:
- id: header-token-mock
  type: header-token
  config:
    headers:
      - x-whatever
    storage:
      type: mock
      config:
        tokens:
          - id: ` + tokenID + `
            value: foobear
- id: header-token-postgres
  type: header-token
  config:
    headers:
      - x-whatever-else
      - and-more
    storage:
      type: postgresql
      config:
        pg_url: postgresql://user:password@127.0.0.1:5432/dbname?sslmode=disable
      `),
			expectedConfig: Config{
				Upstream: "http://127.0.0.1:1982",
				Authenticators: []Authenticator{
					{
						Type: "header-token",
						ID:   "header-token-mock",
						Config: &tokenauthn.HeaderTokenConfig{
							Headers: []string{"x-whatever"},
							Storage: tokenauthn.StorageConfig{
								Type: "mock",
								Config: &tokenmock.Config{
									Tokens: []*tokens.Token{
										{ID: tokenID, Value: "foobear"},
									},
								},
							},
						},
					},
					{
						Type: "header-token",
						ID:   "header-token-postgres",
						Config: &tokenauthn.HeaderTokenConfig{
							Headers: []string{"x-whatever-else", "and-more"},
							Storage: tokenauthn.StorageConfig{
								Type: "postgresql",
								Config: &pg.Config{
									PGURL: "postgresql://user:password@127.0.0.1:5432/dbname?sslmode=disable",
								},
							},
						},
					},
				},
			},
		},
		"mock token adapter": {
			rawConfig: []byte(`
upstream: http://127.0.0.1:1982
tokens:
  type: mock
  config:
    tokens:
      - id: ` + tokenID + `
        value: foobear
      `),
			expectedConfig: Config{
				Upstream: "http://127.0.0.1:1982",
				Token: Token{
					Type: "mock",
					Config: &tokenmock.Config{
						Tokens: []*tokens.Token{
							{ID: tokenID, Value: "foobear"},
						},
					},
				},
			},
		},
		"pg token adapter": {
			rawConfig: []byte(`
upstream: http://127.0.0.1:1982
tokens:
  type: postgresql
  config:
    pg_url: postgresql://user:pass@host:2333/dbname?sslmode=disable
      `),
			expectedConfig: Config{
				Upstream: "http://127.0.0.1:1982",
				Token: Token{
					Type: "postgresql",
					Config: &pg.Config{
						PGURL: "postgresql://user:pass@host:2333/dbname?sslmode=disable",
					},
				},
			},
		},
		"GRPC": {
			rawConfig: []byte(`
upstream: http://127.0.0.1:1982
grpc: 0.0.0.0:8910
      `),
			expectedConfig: Config{
				Upstream: "http://127.0.0.1:1982",
				GRPC:     "0.0.0.0:8910",
			},
		},
		"teams-service endpoint": {
			rawConfig: []byte(`
teams_address: 127.0.0.2:2912
      `),
			expectedConfig: Config{
				TeamsAddress: "127.0.0.2:2912",
			},
		},
	}

	for n, d := range tests {
		t.Run(n, func(t *testing.T) {
			var c Config
			if err := yaml.Unmarshal(d.rawConfig, &c); err != nil {
				t.Fatalf("failed to decode config: %v: %v", string(d.rawConfig), err)
			}
			if diff := pretty.Compare(c, d.expectedConfig); diff != "" {
				t.Errorf("got!=want: %s", diff)
			}
		})
	}
}
