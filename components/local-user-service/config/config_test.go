package config

import (
	"testing"

	"github.com/ghodss/yaml"
	"github.com/kylelemons/godebug/pretty"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/components/local-user-service/users/dex"
	usersmock "github.com/chef/automate/components/local-user-service/users/mock"
)

var _ = yaml.YAMLToJSON

func TestUnmarshalConfig(t *testing.T) {
	tests := map[string]struct {
		rawConfig      []byte
		expectedConfig Config
	}{
		"mock users adapter": {
			rawConfig: []byte(`
users:
  type: mock
  config:
    users:
      alice:
        id: alice
        name: Alice
        email: alice@email.com
        password: 123abc
      `),
			expectedConfig: Config{
				Users: Users{
					Type: "mock",
					Config: &usersmock.Config{
						Users: map[string]users.ShowUser{
							"alice": {
								ID:    "alice",
								Name:  "Alice",
								Email: "alice@email.com",
							},
						},
					},
				},
			},
		},
		"dex users adapter": {
			rawConfig: []byte(`
users:
  type: dex
  config:
    grpcHost: 127.0.0.1:5557
      `),
			expectedConfig: Config{
				Users: Users{
					Type: "dex",
					Config: &dex.Config{
						GRPCHost: "127.0.0.1:5557",
					},
				},
			},
		},
		"GRPC": {
			rawConfig: []byte(`
grpc: 0.0.0.0:8910
      `),
			expectedConfig: Config{
				GRPC: "0.0.0.0:8910",
			},
		},
		"path to a1_user_data.json": {
			rawConfig: []byte(`
a1_user_data: /hab/svc/local-user-service/data/a1_user_data.json
      `),
			expectedConfig: Config{
				A1UserData: "/hab/svc/local-user-service/data/a1_user_data.json",
			},
		},
		"path to a1_user_roles_data.json": {
			rawConfig: []byte(`
a1_user_roles_data: /hab/svc/local-user-service/data/a1_user_roles_data.json
      `),
			expectedConfig: Config{
				A1UserRolesData: "/hab/svc/local-user-service/data/a1_user_roles_data.json",
			},
		},
	}

	for n, d := range tests {
		t.Run(n, func(t *testing.T) {
			var c Config
			if err := yaml.Unmarshal(d.rawConfig, &c); err != nil {
				t.Fatalf("failed to decode config: %v", err)
			}
			if diff := pretty.Compare(c, d.expectedConfig); diff != "" {
				t.Errorf("got!=want: %s", diff)
			}
		})
	}
}
