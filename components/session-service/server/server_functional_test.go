// These tests instantiate in-memory-storage backend instances of dex and
// session-service, and use a standard http.Client to interacting with them.
// These interactions do properly handle redirects and cookies, so they closely
// approximate what happens in a real-world setting.
package server_test

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/cookiejar"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"
	"time"

	dex "github.com/dexidp/dex/server"
	"github.com/dexidp/dex/storage"
	"github.com/dexidp/dex/storage/memory"
	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/session-service/oidc"
	"github.com/chef/automate/components/session-service/server"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// toggle for logs our no logs in test output
const wantLogs = false

// Validity of id_tokens as issued by dex
const idTokenValidity = time.Minute

// if id_token expiry is this close, refresh
const remainingDuration = 30 * time.Second

func TestMain(t *testing.T) {
	var l logger.Logger
	var err error
	if wantLogs {
		l, err = logger.NewLogger("text", "debug")
		require.NoError(t, err)
	} else {
		l = logger.NewTestLogger()
	}

	clientID, clientSecret := "automate-session-test", "foobear2000"

	mux := mux.NewRouter()
	s := httptest.NewUnstartedServer(mux)
	dexCerts := devDexCerts(t)
	s.TLS = &tls.Config{
		// We have to use the dex certs here because session-service is going
		// to check that the thing its talking to is automate-dex
		Certificates: []tls.Certificate{*dexCerts.ServiceKeyPair},
	}
	s.StartTLS()
	defer s.Close()

	baseURL, err := url.Parse(s.URL)
	require.NoError(t, err)
	signInURL, err := baseURL.Parse("/signin")
	require.NoError(t, err)
	bldrSignInURL, err := baseURL.Parse("https://builder.test/")
	require.NoError(t, err)
	bldrClient := &server.BldrClient{
		SignInURL:    bldrSignInURL,
		ClientID:     "bldr-client",
		ClientSecret: "bldr-secret",
	}
	issuerURL, err := baseURL.Parse("/dex")
	require.NoError(t, err)
	redirectURL, err := baseURL.Parse("/session/callback")
	require.NoError(t, err)

	// nowFunc allows us to arbitrarily trigger token expiry in oauth2's
	// TokenSource in an indirect way: to have an id_token expire, we set dex's
	// clock _back_, so the token we end up getting will have expired already.
	timeLapse := 0 * time.Minute
	nowFunc := func() time.Time {
		return time.Now().Add(-timeLapse)
	}

	dexServer := newDexTestServer(t, nowFunc, clientID, clientSecret, redirectURL.String(), issuerURL.String())
	mux.PathPrefix("/dex").HandlerFunc(dexServer.ServeHTTP)
	mux.PathPrefix("/signin").HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {})

	cfg := oidc.Config{
		ClientID:     clientID,
		ClientSecret: clientSecret,
		RedirectURL:  redirectURL,
		IssuerURL:    issuerURL,
		DexURL:       baseURL,
	}
	sessionServer, err := server.NewInMemory(l, remainingDuration, cfg, signInURL, bldrClient, devSessionCerts(t))
	require.NoError(t, err)

	mux.PathPrefix("/session").
		Handler(http.StripPrefix("/session", http.HandlerFunc(sessionServer.ServeHTTP)))

	newEndpoint, err := baseURL.Parse("/session/new")
	require.NoError(t, err)
	refreshEndpoint, err := baseURL.Parse("/session/refresh")
	require.NoError(t, err)
	tokenEndpoint, err := baseURL.Parse("/session/token")
	require.NoError(t, err)
	userinfoEndpoint, err := baseURL.Parse("/session/userinfo")
	require.NoError(t, err)

	// these things are passed through the various test cases
	var idToken string
	cj, err := cookiejar.New(nil)
	require.NoError(t, err)

	certpool := dexCerts.NewCertPool()
	c := s.Client()
	c.Jar = cj
	c.Transport = &http.Transport{
		TLSClientConfig: &tls.Config{
			ServerName: "automate-dex",
			RootCAs:    certpool,
		},
	}

	t.Run("GET /session/new", func(t *testing.T) {
		// Note: this is following redirects, and we've only got one connector that
		// does not require interaction. Furthermore, we're skipping approval.
		// tl;dr: this one request will get us all the way!
		resp, err := c.Get(newEndpoint.String())
		require.NoError(t, err, "GET /new")
		defer resp.Body.Close()
		assert.Equal(t, http.StatusOK, resp.StatusCode)

		// This final redirect means we've successfully logged in, so we'll want to
		// capture that id_token
		if assert.Contains(t, resp.Request.URL.String(), "/signin#id_token=") {
			vals, err := url.ParseQuery(resp.Request.URL.Fragment)
			require.NoError(t, err)
			idToken = vals.Get("id_token")
		}
	})

	t.Run("GET /session/refresh", func(t *testing.T) {
		req, err := http.NewRequest("GET", refreshEndpoint.String(), nil)
		require.NoError(t, err)
		req.Header.Set("Authorization", fmt.Sprintf("bearer %s", idToken))
		resp, err := c.Do(req)
		require.NoError(t, err, "refresh request")
		defer resp.Body.Close()

		body, err := ioutil.ReadAll(resp.Body)
		require.NoError(t, err, "read refresh response")
		data := struct {
			IDToken string `json:"id_token"`
		}{}
		if err := json.Unmarshal(body, &data); err != nil {
			t.Fatalf("parse refresh response: %s", err)
		}

		assert.Equal(t, idToken, data.IDToken,
			"the token hasn't expired yet, so we're given it back")
	})

	// reset cookie jar
	cj, err = cookiejar.New(nil)
	require.NoError(t, err)

	timeLapse = idTokenValidity - remainingDuration // turn back time (for dex only!)

	t.Run("GET /session/refresh (expired token)", func(t *testing.T) {
		// first get a new (already expired) token
		resp, err := c.Get(newEndpoint.String())
		require.NoError(t, err, "GET /new")
		defer resp.Body.Close()

		// This final redirect means we've successfully logged in, so we'll want to
		// capture that id_token
		if assert.Contains(t, resp.Request.URL.String(), "/signin#id_token=") {
			vals, err := url.ParseQuery(resp.Request.URL.Fragment)
			require.NoError(t, err, "parse fragment")
			idToken = vals.Get("id_token")
		}
		req, err := http.NewRequest("GET", refreshEndpoint.String(), nil)
		require.NoError(t, err, "create refresh request")
		req.Header.Set("Authorization", fmt.Sprintf("bearer %s", idToken))
		resp, err = c.Do(req)
		require.NoError(t, err, "send refresh request")
		defer resp.Body.Close()

		body, err := ioutil.ReadAll(resp.Body)
		require.NoError(t, err, "read refresh response")
		data := struct {
			IDToken string `json:"id_token"`
		}{}
		err = json.Unmarshal(body, &data)
		require.NoError(t, err, "parse refresh response")

		assert.NotEqual(t, idToken, data.IDToken,
			"the token has expired, so we're given a new one")
	})

	t.Run("GET /session/new?id_token_hint=ID_TOKEN", func(t *testing.T) {
		// no redirect we just want to see the dex URL we're sent to
		cl := http.Client{
			CheckRedirect: func(*http.Request, []*http.Request) error {
				return http.ErrUseLastResponse
			},
			Jar:       c.Jar,
			Transport: c.Transport, // copy custom TLS stuff from httptest's client
		}

		resp, err := cl.Get(fmt.Sprintf("%s?id_token_hint=%s", newEndpoint.String(), idToken))
		require.NoError(t, err, "GET /new?id_token_hint=...")
		defer resp.Body.Close()
		require.Equal(t, http.StatusSeeOther, resp.StatusCode)
		require.Contains(t, resp.Header.Get("Location"), "/dex/auth?")
		require.Contains(t, resp.Header.Get("Location"), "connector_id=mock")
	})

	t.Run("GET /session/new (login, logout, try reusing old session", func(t *testing.T) {
		var oldIDToken string

		// start afresh
		cj, err = cookiejar.New(nil)
		require.NoError(t, err)

		// Login
		resp, err := c.Get(newEndpoint.String())
		require.NoError(t, err, "GET /new")
		defer resp.Body.Close()
		assert.Equal(t, http.StatusOK, resp.StatusCode)

		// This indicates that the new-session dance with dex has succeeded
		require.Contains(t, resp.Request.URL.Path, "/signin")
		// This final redirect means we've successfully logged in, so we'll want to
		// capture that id_token
		if assert.Contains(t, resp.Request.URL.String(), "/signin#id_token=") {
			vals, err := url.ParseQuery(resp.Request.URL.Fragment)
			require.NoError(t, err, "parse fragment")
			oldIDToken = vals.Get("id_token") // we need this below
		}

		// save cookie for re-use below
		prevSessCookies := c.Jar.Cookies(newEndpoint)

		// Logout
		// /!\ We _don't_ follow through to the callback handler here -- we want to
		// simulate the scenario where the user has clicked "Sign out", but doesn't
		// re-login via dex.
		// If we DID re-login, the token RenewToken call in the callback handler
		// alone would take care of deleting the old session.

		// To achieve this, we use a new client for the logout; which doesn't
		// follow redirects. But at the same time, we want this client to use the
		// same cookie jar, so we'll trigger session deletion
		cl := http.Client{
			CheckRedirect: func(*http.Request, []*http.Request) error {
				return http.ErrUseLastResponse
			},
			Jar:       c.Jar,
			Transport: c.Transport, // copy custom TLS stuff from httptest's client
		}

		resp, err = cl.Get(newEndpoint.String())
		require.NoError(t, err, "GET /new")
		defer resp.Body.Close()
		require.Equal(t, http.StatusSeeOther, resp.StatusCode)
		require.Contains(t, resp.Header.Get("Location"), "/dex/auth?")

		// Now, we try to get a new id_token via refresh using the OLD session
		// cookie. Note that this requires an (existing) id_token; but that one
		// could have expired by now. Hence session reuse after logout still poses
		// a threat.
		req, err := http.NewRequest("GET", refreshEndpoint.String(), nil)
		require.NoError(t, err)
		// We're reusing the old cookie, and the (old, potentially expired) id_token
		c.Jar.SetCookies(refreshEndpoint, prevSessCookies)
		req.Header.Set("Authorization", fmt.Sprintf("bearer %s", oldIDToken))

		resp, err = c.Do(req)
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusUnauthorized, resp.StatusCode)
	})

	t.Run("GET /session/new with state, client_id, and redirect_uri", func(t *testing.T) {
		c := s.Client()
		cj, err = cookiejar.New(nil)
		require.NoError(t, err)
		cl := http.Client{
			CheckRedirect: func(req *http.Request, _ []*http.Request) error {
				// stop following redirects if we're sent to builder
				if req.URL.Host == "builder.test" {
					return http.ErrUseLastResponse
				}
				return nil
			},
			Jar:       cj,
			Transport: c.Transport, // copy custom TLS stuff from httptest's client
		}

		// construct request with query
		req, err := http.NewRequest("GET", newEndpoint.String(), nil)
		require.NoError(t, err)
		q := req.URL.Query()
		q.Add("redirect_uri", "https://builder.test/")
		q.Add("client_id", "bldr-client")
		q.Add("state", "SomethingBuilderGaveUs")
		req.URL.RawQuery = q.Encode()

		resp, err := cl.Do(req)
		require.NoError(t, err, "GET /new")
		defer resp.Body.Close()
		assert.Equal(t, http.StatusSeeOther, resp.StatusCode)

		loc, err := resp.Location()
		require.NoError(t, err)
		code := loc.Query().Get("code")
		assert.Contains(t, loc.String(), "https://builder.test/")
		assert.NotZero(t, code, "redirect to builder has a code")
		assert.Equal(t, "SomethingBuilderGaveUs", loc.Query().Get("state"), "redirect to builder has correct state")

		t.Run("POST /session/token, with code passed to builder via callback with client_id, client_secret, and redirect_uri", func(t *testing.T) {
			// fresh client, no cookies whatsoever => this request comes from the
			// builder backend service
			c := s.Client()
			data := url.Values{}
			data.Set("code", code)
			reqBody := strings.NewReader(data.Encode())
			reqUrl, err := url.Parse(tokenEndpoint.String())
			require.NoError(t, err)
			q := reqUrl.Query()
			q.Add("redirect_uri", "https://builder.test/")
			q.Add("client_id", "bldr-client")
			q.Add("client_secret", "bldr-secret")
			reqUrl.RawQuery = q.Encode()
			resp, err := c.Post(reqUrl.String(), "application/x-www-form-urlencoded", reqBody)
			require.NoError(t, err)
			assert.Equal(t, http.StatusOK, resp.StatusCode)
			defer resp.Body.Close()
			respMsg := struct {
				AT string `json:"access_token"`
			}{}
			err = json.NewDecoder(resp.Body).Decode(&respMsg)
			require.NoError(t, err)
			idToken := respMsg.AT // yes this is a bit odd, but the dirty deal Okta does
			assert.NotZero(t, idToken)

			t.Run("GET /session/userinfo, with ID token retrieved from /token endpoint", func(t *testing.T) {
				req, err := http.NewRequest("GET", userinfoEndpoint.String(), nil)
				require.NoError(t, err)
				req.Header.Add("Authorization", "bearer "+idToken)
				resp, err := c.Do(req)
				require.NoError(t, err)
				respMsg := struct {
					Sub string `json:"sub"`
					PU  string `json:"preferred_username"`
				}{}
				err = json.NewDecoder(resp.Body).Decode(&respMsg)
				assert.Equal(t, "Cg0wLTM4NS0yODA4OS0wEgRtb2Nr", respMsg.Sub)
				assert.Equal(t, "kilgore@kilgore.trout", respMsg.PU)
			})
		})
	})

}

func newDexTestServer(t *testing.T, nowFunc func() time.Time,
	clientID, clientSecret, redirectURL, issuerURL string) *dex.Server {
	ctx := context.Background()
	var serv *dex.Server
	logger := logrus.New()
	logger.SetLevel(logrus.DebugLevel)

	config := dex.Config{
		SkipApprovalScreen: true,
		Issuer:             issuerURL,
		Storage:            memory.New(logger),
		Web: dex.WebConfig{
			Dir: "testdata/web",
		},
		Logger:             logger,
		IDTokensValidFor:   idTokenValidity,
		Now:                nowFunc,
		PrometheusRegistry: prometheus.NewRegistry(),
	}

	connector := storage.Connector{
		ID:              "mock",
		Type:            "mockCallback",
		Name:            "mock",
		ResourceVersion: "1",
	}
	if err := config.Storage.CreateConnector(connector); err != nil {
		t.Fatalf("create connector: %v", err)
	}

	client := storage.Client{
		ID:           clientID,
		Secret:       clientSecret,
		RedirectURIs: []string{redirectURL},
	}
	if err := config.Storage.CreateClient(client); err != nil {
		t.Fatalf("create client: %v", err)
	}

	var err error
	if serv, err = dex.NewServer(ctx, config); err != nil {
		t.Fatal(err)
	}
	return serv
}

func devDexCerts(t *testing.T) *certs.ServiceCerts {
	return devCerts(t, "automate-dex")
}

func devSessionCerts(t *testing.T) *certs.ServiceCerts {
	return devCerts(t, "session-service")
}

func devCerts(t *testing.T, name string) *certs.ServiceCerts {
	return helpers.LoadDevCerts(t, name)
}
