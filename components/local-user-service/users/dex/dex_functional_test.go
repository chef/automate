// This tests talks to an in-memory dex service, to check that we can actually
// talk to the real thing.
package dex_test

import (
	"context"
	"crypto/tls"
	"fmt"
	"net/http/httptest"
	"net/url"
	"testing"
	"time"

	dex_server "github.com/dexidp/dex/server"
	"github.com/dexidp/dex/storage"
	"github.com/dexidp/dex/storage/memory"
	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"

	"github.com/chef/automate/components/local-user-service/users/dex"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestPasswordValidation(t *testing.T) {
	clientID, clientSecret := "automate-api", "" // public client

	connectors := map[string][]storage.Connector{
		"local only": []storage.Connector{
			{
				ID:              "local",
				Type:            "mockPassword",
				Name:            "local",
				ResourceVersion: "1",
				Config:          []byte(fmt.Sprintf(`{"username":%q,"password":%q}`, "alice", "somethingStrange")),
			},
		},
		"local + something else": []storage.Connector{
			{
				ID:              "local",
				Type:            "mockPassword",
				Name:            "local",
				ResourceVersion: "1",
				Config:          []byte(fmt.Sprintf(`{"username":%q,"password":%q}`, "alice", "somethingStrange")),
			},
			{
				ID:              "saml",
				Type:            "mockCallback",
				Name:            "local",
				ResourceVersion: "1",
			},
		},
	}

	for setting, conns := range connectors {
		t.Run(setting, func(t *testing.T) {
			mux := mux.NewRouter()
			s := httptest.NewUnstartedServer(mux)
			dexCerts := helpers.LoadDevCerts(t, "automate-dex")
			s.TLS = &tls.Config{
				Certificates: []tls.Certificate{*dexCerts.ServiceKeyPair},
			}
			s.StartTLS()
			defer s.Close()

			baseURL, err := url.Parse(s.URL)
			require.NoError(t, err)
			issuerURL, err := baseURL.Parse("/dex")
			require.NoError(t, err)

			dexServer := newDexTestServer(t, clientID, clientSecret, conns, issuerURL.String())
			mux.PathPrefix("/dex").HandlerFunc(dexServer.ServeHTTP)

			serviceCerts := helpers.LoadDevCerts(t, "local-user-service")

			l, err := zap.NewDevelopment()
			require.NoError(t, err)

			grpcHost := "127.0.0.1:10000" // does not matter here
			adpCfg := &dex.Config{
				GRPCHost:  grpcHost,
				IssuerURL: issuerURL.String(),
				DexURL:    issuerURL.String(),
			}
			adp, err := adpCfg.Open(l, serviceCerts)
			require.NoError(t, err)

			cases := map[string]struct {
				expectValid        bool
				username, password string
			}{
				"all valid": {
					username:    "alice",
					password:    "somethingStrange",
					expectValid: true,
				},
				"unknown user": {
					username:    "bob",
					password:    "somethingStrange",
					expectValid: false,
				},
				"bad password": {
					username:    "alice",
					password:    "somethingNotSoStrange",
					expectValid: false,
				},
			}

			for name, tc := range cases {
				t.Run(name, func(t *testing.T) {
					ctx := context.Background()
					valid, err := adp.ValidatePassword(ctx, tc.username, tc.password)
					require.NoError(t, err)
					assert.Equal(t, tc.expectValid, valid)
				})
			}
		})
	}
}

func newDexTestServer(t *testing.T,
	clientID, clientSecret string,
	connectors []storage.Connector,
	issuerURL string) *dex_server.Server {
	ctx := context.Background()
	var serv *dex_server.Server
	logger := logrus.New()
	logger.SetLevel(logrus.DebugLevel)

	config := dex_server.Config{
		SkipApprovalScreen: true,
		Issuer:             issuerURL,
		Storage:            memory.New(logger),
		Web: dex_server.WebConfig{
			Dir: "../../../session-service/server/testdata/web", // FIXME
		},
		Logger:             logger,
		IDTokensValidFor:   time.Minute,
		PrometheusRegistry: prometheus.NewRegistry(),
	}

	for _, conn := range connectors {
		if err := config.Storage.CreateConnector(conn); err != nil {
			t.Fatalf("create connector %q: %v", conn.ID, err)
		}
	}

	client := storage.Client{
		ID:           clientID,
		Secret:       clientSecret,
		RedirectURIs: []string{"urn:ietf:wg:oauth:2.0:oob"},
	}
	if err := config.Storage.CreateClient(client); err != nil {
		t.Fatalf("create client: %v", err)
	}

	var err error
	if serv, err = dex_server.NewServer(ctx, config); err != nil {
		t.Fatal(err)
	}
	return serv
}
