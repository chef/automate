package server

import (
	"context"
	"crypto/rand"
	"crypto/subtle"
	"crypto/tls"
	"database/sql"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"time"

	"github.com/alexedwards/scs"
	"github.com/alexedwards/scs/stores/memstore"
	"github.com/alexedwards/scs/stores/pgstore"
	"github.com/gorilla/mux"
	"github.com/patrickmn/go-cache"
	"github.com/pkg/errors"
	"golang.org/x/oauth2"

	"github.com/chef/automate/components/session-service/migration"
	"github.com/chef/automate/components/session-service/oidc"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	util "github.com/chef/automate/lib/oidc"
	"github.com/chef/automate/lib/tls/certs"
)

// BldrClient holds the config for the bldr oauth2 client.
type BldrClient struct {
	SignInURL    *url.URL
	ClientID     string
	ClientSecret string
}

// Server holds the server state
type Server struct {
	mux               http.Handler
	log               logger.Logger
	mgr               *scs.Manager
	client            oidc.Client
	bldrClient        *BldrClient
	signInURL         *url.URL
	remainingDuration time.Duration
	serviceCerts      *certs.ServiceCerts
	tokenCache        *cache.Cache
}

const (
	aliveKey                 = "alive"
	refreshTokenKey          = "refresh_token"
	relayStateKeyPrefix      = "relay_state_"  // random, passed to OIDC IdP
	clientStateKeyPrefix     = "client_state_" // coming from the browser, passed back on success
	numRelayStateRandomBytes = 10
	redirectURIKey           = "redirect_uri"

	// This determines how often the PG backend will cleanup expired session
	// records. It does not affect session expiration; only records that have
	// already expired are dropped.
	// Note 2017/12/07 (sr): 12hrs here is completely arbitrary.
	dbCleanupInterval = 12 * time.Hour

	// This duration drives the refresh process: if the token expires within the
	// next minute, we'll refresh. This is a first guess and might need tweaking.
	remainingDuration = time.Minute

	// servingStatus is our -- currently hardcoded -- /health endpoint response.
	// It mimics the grpc.health.v1 response we return in GRPC services.
	servingStatus = "SERVING"
)

// New returns a new instance of the server
func New(
	l logger.Logger,
	migrationConfig *migration.Config,
	oidcCfg oidc.Config,
	bldrClient *BldrClient,
	signInURL *url.URL,
	serviceCerts *certs.ServiceCerts) (*Server, error) {

	oidcClient, err := oidc.New(oidcCfg, 1, serviceCerts, l)
	if err != nil {
		return nil, errors.Wrap(err, "OIDC client")
	}

	if err := migrationConfig.Go(); err != nil { // nolint: vetshadow
		return nil, errors.Wrap(err, "migrations")
	}

	store, err := initStore(migrationConfig.PG)
	if err != nil {
		return nil, errors.Wrap(err, "init store")
	}

	scsManager := createSCSManager(store)

	s := Server{
		log:               l,
		client:            oidcClient,
		signInURL:         signInURL,
		bldrClient:        bldrClient,
		remainingDuration: remainingDuration,

		mgr:          scsManager,
		serviceCerts: serviceCerts,
		tokenCache:   cache.New(1*time.Minute, 5*time.Minute),
	}
	s.initHandlers()

	return &s, nil
}

// NewInMemory instantiates a memory-backed instance of the server, to be used
// for testing.
func NewInMemory(
	l logger.Logger,
	remainingDuration time.Duration,
	oidcCfg oidc.Config,
	signInURL *url.URL,
	bldrClient *BldrClient,
	serviceCerts *certs.ServiceCerts) (*Server, error) {

	oidcClient, err := oidc.New(oidcCfg, 0, serviceCerts, l)
	if err != nil {
		return nil, errors.Wrap(err, "OIDC client")
	}

	scsManager := createSCSManager(memstore.New(time.Hour))

	s := Server{
		log:               l,
		client:            oidcClient,
		signInURL:         signInURL,
		bldrClient:        bldrClient,
		remainingDuration: remainingDuration,

		mgr:        scsManager,
		tokenCache: cache.New(1*time.Minute, 5*time.Minute),
	}
	s.initHandlers()

	return &s, nil
}

func initStore(pgURL *url.URL) (scs.Store, error) {
	db, err := sql.Open("postgres", pgURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "open DB")
	}

	return pgstore.New(db, dbCleanupInterval), nil
}

func (s *Server) initHandlers() {
	r := mux.NewRouter()
	r.HandleFunc("/health", s.healthHandler).Methods("GET")
	r.HandleFunc("/new", s.newHandler).Methods("GET")
	r.HandleFunc("/refresh", s.refreshHandler).
		Methods("GET").
		HeadersRegexp("Cookie", "session=.+")
	r.HandleFunc("/callback", s.callbackHandler).
		Methods("GET").
		HeadersRegexp("Cookie", "session=.+")

	// these are only to be used if Builder is configured to authenticate with Automate
	r.HandleFunc("/token", s.tokenHandler).
		Methods("POST")
	r.HandleFunc("/userinfo", s.userinfoHandler).
		Methods("GET")

	r.PathPrefix("/").HandlerFunc(s.catchAllElseHandler)
	// ^ if none of the above match, it's going to be a 401.
	s.mux = r
}

// ListenAndServe starts a listener, and serves requests using Server's embedded
// http.ServeMux. Only ever returns with non-nil error.
func (s *Server) ListenAndServe(addr string) error {
	s.log.Debugf("listening (https) on %s", addr)
	httpServer := http.Server{
		Addr:    addr,
		Handler: s.mux,
		TLSConfig: &tls.Config{
			Certificates:             []tls.Certificate{*s.serviceCerts.ServiceKeyPair},
			PreferServerCipherSuites: true,
			MinVersion:               tls.VersionTLS12,
			CipherSuites:             secureconn.DefaultCipherSuites(),
		},
	}
	return httpServer.ListenAndServeTLS("", "")
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.mux.ServeHTTP(w, r)
}

func (s *Server) catchAllElseHandler(w http.ResponseWriter, _ *http.Request) {
	httpError(w, http.StatusUnauthorized)
}

// Authorization redirect callback from OAuth2 auth flow.
func (s *Server) callbackHandler(w http.ResponseWriter, r *http.Request) {
	if errMsg := r.FormValue("error"); errMsg != "" {
		http.Error(w, errMsg+": "+r.FormValue("error_description"), http.StatusBadRequest)
		return
	}

	sess := s.mgr.Load(r)
	code := r.FormValue("code")
	if code == "" {
		s.log.Debugf("no code in request: %q", r.Form)
		http.Error(w, fmt.Sprintf("no code in request: %q", r.Form), http.StatusBadRequest)
		return
	}
	state := r.FormValue("state")
	if state == "" {
		s.log.Debugf("no state in request: %q", r.Form)
		http.Error(w, fmt.Sprintf("no state in request: %q", r.Form), http.StatusBadRequest)
		return
	}
	knownRelayState, err := sess.GetBool(relayStateKeyPrefix + state)
	if err != nil {
		s.log.Debugf("bad session data (relay state): %v", err)
		http.Error(w, "bad session data (relay state)", http.StatusBadRequest)
		return
	}
	if !knownRelayState {
		s.log.Debugf("unknown relay state: %q", state)
		http.Error(w, fmt.Sprintf("unknown relay state in request: %q", state), http.StatusBadRequest)
		return
	}
	s.log.Debugf("relay state %q known", state)

	token, err := s.client.Exchange(r.Context(), code)
	if err != nil {
		s.log.Debugf("failed to get token: %v", err)
		http.Error(w, fmt.Sprintf("failed to get token: %v", err), http.StatusInternalServerError)
		return
	}

	// this code variable is stored for use by the builder authn flow in the token handler
	code2, err := generateRelayState()
	if err != nil {
		s.log.Errorf("couldn't generate random relay state: %s", err)
		httpError(w, http.StatusInternalServerError)
		return
	}
	s.tokenCache.Set(code2, token, cache.DefaultExpiration)

	rawIDToken, ok := token.Extra("id_token").(string)
	if !ok {
		s.log.Debug("no id_token in token response")
		http.Error(w, "no id_token in token response", http.StatusInternalServerError)
		return
	}

	if err := sess.RenewToken(w); err != nil { // nolint: vetshadow
		s.log.Debugf("failed to renew session token: %v", err)
		http.Error(w, errors.Wrap(err, "failed to renew session token").Error(), http.StatusInternalServerError)
		return
	}

	if token.RefreshToken != "" {
		err = sess.PutString(w, "refresh_token", token.RefreshToken)
		if err != nil {
			s.log.Debugf("failed to set session token: %v", err)
			http.Error(w, "failed to set refresh_token", http.StatusInternalServerError)
			return
		}
	}

	clientState, err := sess.GetString(clientStateKeyPrefix + state)
	s.log.Debugf("retrieved clientState %q", clientState)
	if err != nil {
		s.log.Debugf("bad session data (client state): %v", err)
		http.Error(w, "bad session data (client state)", http.StatusBadRequest)
		return
	}

	// remove used relay_state + client_state
	if err := sess.Remove(w, relayStateKeyPrefix+state); err != nil { // nolint: vetshadow
		s.log.Debugf("failed to remove relay state: %v", err)
		http.Error(w, errors.Wrap(err, "failed to remove relay state").Error(), http.StatusInternalServerError)
		return
	}
	if err := sess.Remove(w, clientStateKeyPrefix+state); err != nil { // nolint: vetshadow
		s.log.Debugf("failed to remove client state: %v", err)
		http.Error(w, errors.Wrap(err, "failed to remove client state").Error(), http.StatusInternalServerError)
		return
	}

	redirectURI, err := sess.GetString(redirectURIKey)
	s.log.Debugf("retrieved redirectURI %q", redirectURI)
	if err != nil {
		s.log.Debugf("bad session data (redirect uri): %v", err)
		http.Error(w, "bad session data (redirect uri)", http.StatusBadRequest)
		return
	}

	u := new(url.URL)
	// different responses based on if the auth request came from builder or automate
	if redirectURI != "" {
		*u = *s.bldrClient.SignInURL

		u.RawQuery = fmt.Sprintf("state=%s&code=%s", clientState, code2)

		// In this flow, we don't need to keep a session -- there's no refresh yet.
		err = sess.Destroy(w)
		if err != nil {
			s.log.Debugf("failed to destroy session: %v", err)
			http.Error(w, "failed to destroy session", http.StatusInternalServerError)
			return
		}
		http.Redirect(w, r, u.String(), http.StatusSeeOther)
	} else {
		*u = *s.signInURL

		u.Fragment = fmt.Sprintf("id_token=%s&state=%s", rawIDToken, clientState)
		http.Redirect(w, r, u.String(), http.StatusSeeOther)
	}
}

func (s *Server) tokenHandler(w http.ResponseWriter, r *http.Request) {
	// Need to perform all the required validation for the access token portion
	// of the Authorization Code Flow.
	//
	// TODO (tc): We'd also want to prevent replay attacks, but we'd need a database (or at least a cache) for that:
	// "Preventing Replay Attacks" https://www.oauth.com/oauth2-servers/access-tokens/authorization-code-request/

	// If bldr was not configured.
	if s.bldrClient == nil {
		http.Error(w,
			"no Oauth2 clients were configured; if you are trying to authenticate for builder, "+
				"configure session-service's bldr settings",
			http.StatusUnauthorized)
		return
	}

	// We currently only support the bldr client.
	if r.FormValue("client_id") != s.bldrClient.ClientID {
		http.Error(w,
			fmt.Sprintf("client_id %s was submitted to /token but /token currently only supports bldr Oauth2 clients. "+
				"submit the client_id %s with the proper secret.",
				r.FormValue("client_id"), s.bldrClient.ClientID),
			http.StatusUnauthorized)
		return
	}

	// Check that the redirect_uri passed is an exact match of the redirect URI that was used
	// when generating the authorization code. Since we asserted that the redirect_uri matched
	// our config for bldr on auth code generation, if we assert that again, we know they match.
	//
	// For more info, read "Granting Access Tokens" here:
	// https://www.oauth.com/oauth2-servers/redirect-uris/redirect-uri-validation/
	if redirectURI := r.FormValue("redirect_uri"); redirectURI != s.bldrClient.SignInURL.String() {
		s.invalidRedirectURIError(w, s.bldrClient.ClientID, redirectURI, s.bldrClient.SignInURL.String())
		return
	}

	// Validate the client secret. See "Step 4: Application Requests Access Token" for more info:
	// https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2
	// Do so while avoiding timing attacks (see safeCmpStrings).
	if !safeCmpStrings(r.FormValue("client_secret"), s.bldrClient.ClientSecret) {
		http.Error(w,
			fmt.Sprintf("client_secret passed for client %s does not match stored value", s.bldrClient.ClientID),
			http.StatusUnauthorized)
		return
	}

	// We are good to grant for the bldr case
	s.log.Debugf("redirect_uri and client_secret valid for client_id %s", s.bldrClient.ClientID)

	// need to grab 'code' from the request body, exchange for token, return token
	code := r.PostFormValue("code")
	if "" == code {
		http.Error(w, fmt.Sprint("no code in request"), http.StatusBadRequest)
		return
	}

	token, ok := s.tokenCache.Get(code)
	if !ok {
		s.log.Debugf("failed to get token %v from map", code)
		http.Error(w, fmt.Sprint("failed to get token"), http.StatusInternalServerError)
		return
	}

	rawIDToken, ok := token.(*oauth2.Token).Extra("id_token").(string)
	if !ok {
		s.log.Debug("no id_token in token response")
		http.Error(w, "no id_token in token response", http.StatusInternalServerError)
		return
	}

	returnData := struct {
		AccessToken string `json:"access_token"`
	}{rawIDToken}
	if err := json.NewEncoder(w).Encode(returnData); err != nil {
		http.Error(w, errors.Wrap(err, "failed to set access token").Error(),
			http.StatusInternalServerError)
		return
	}
}

func (s *Server) userinfoHandler(w http.ResponseWriter, r *http.Request) {
	token, err := util.ExtractBearerToken(r)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	idToken, err := s.client.Verify(r.Context(), token)
	if err != nil {
		http.Error(w, err.Error(), http.StatusUnauthorized)
		return
	}
	var claims struct {
		Username string `json:"email"`
	}
	err = idToken.Claims(&claims)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	returnData := struct {
		Sub               string `json:"sub"`
		PreferredUsername string `json:"preferred_username"`
	}{idToken.Subject, claims.Username}
	if err := json.NewEncoder(w).Encode(returnData); err != nil {
		http.Error(w, errors.Wrap(err, "failed to set user info").Error(),
			http.StatusInternalServerError)
		return
	}
}

// Note 2017/12/11 (sr): /refresh is called periodically by the UI, irregardless
// of whether we're using a connector (dex) that allows for refresh_tokens. To
// make that work, we default to returning the caller's id_token if it's still
// valid.
// That also ensures that our logic does not depend on client cooperation. The
// only (and correct) way to enforce session refresh is thus to hand out
// id_tokens with short expiry.
func (s *Server) refreshHandler(w http.ResponseWriter, r *http.Request) {
	sess := s.mgr.Load(r)
	refreshToken, err := sess.GetString(refreshTokenKey)
	if err != nil {
		http.Error(w, "bad session data", http.StatusBadRequest)
		return
	}
	// Note: refreshToken may be "" here (e.g. for SAML); we don't bother, since
	// as long as the idToken doesn't expire soon,
	// maybeExchangeRefreshTokenForIDToken will return the still-valid idToken.

	// Check if the session exists
	if alive, err := sess.GetBool(aliveKey); err != nil {
		s.log.Error("error retrieving session alive key")
		httpError(w, http.StatusInternalServerError)
		return
	} else if !alive {
		s.log.Info("no session found")
		httpError(w, http.StatusUnauthorized)
		return
	}

	idToken, err := util.ExtractBearerToken(r)
	if err != nil {
		s.log.Debug("no bearer token")
		httpError(w, http.StatusUnauthorized)
		return
	}

	// Renew session: This ensures the old session is gone and cannot be re-used
	if err := sess.RenewToken(w); err != nil { // nolint: vetshadow
		s.log.Error("failed to renew token for session") // TODO this is too unspecific
		// do we not know anything else to identify this request?
		httpError(w, http.StatusInternalServerError)
		return
	}

	// TODO 2017/12/11 (sr): should we kill the session on failure here?
	token, err := s.maybeExchangeRefreshTokenForIDToken(r.Context(), refreshToken, idToken)
	if err != nil {
		s.log.Debugf("failed to exchange token: %s", err)
		httpError(w, http.StatusUnauthorized)
		return
	}

	rawIDToken, ok := token.Extra("id_token").(string)
	if !ok {
		http.Error(w, "no id_token in token response", http.StatusInternalServerError)
		return
	}

	if token.RefreshToken != "" {
		if err := sess.PutString(w, refreshTokenKey, token.RefreshToken); err != nil {
			http.Error(w, "failed to set refresh_token", http.StatusInternalServerError)
			return
		}
	}

	returnData := struct {
		IDToken string `json:"id_token"`
	}{rawIDToken}
	if err := json.NewEncoder(w).Encode(returnData); err != nil {
		http.Error(w, errors.Wrap(err, "failed to set marshal id_token").Error(), http.StatusInternalServerError)
		return
	}
}

func (s *Server) healthHandler(w http.ResponseWriter, _ *http.Request) {
	returnData := struct {
		Status string `json:"status"`
	}{servingStatus}
	if err := json.NewEncoder(w).Encode(returnData); err != nil {
		http.Error(w, errors.Wrap(err, "failed to set marshal health status").Error(),
			http.StatusInternalServerError)
		return
	}
}

func (s *Server) newHandler(w http.ResponseWriter, r *http.Request) {
	sess := s.mgr.Load(r)
	// This could be a session token that has already been leaked. By renewing it
	// here, we ensure that the session token sent with this request cannot be
	// used afterwards.
	//!\\ This mitigates the "stolen session-cookie reused after logout" threat.
	if err := sess.RenewToken(w); err != nil { // nolint: vetshadow
		httpError(w, http.StatusInternalServerError)
		return
	}

	// TODO (tc): We should do all of this for Automate UI as well. But instead
	// of rolling our own, we should just use an Oauth2 library imo.
	//
	// If the request came from builder and if builder's configured, we should
	// 1) validate that the submitted redirect_url matches our config,
	// and 2) validate that it matches what they send into the /token handler.
	// We'll store what they sent to /new in the session so we can verify it in /token.
	// If bldr is not configured, the only valid redirect_uri will be hardcoded to be
	// automate UI, so bad actors at least can't maliciously redirect somewhere else, but
	// we need to also lock down Automate UI.
	//
	// Read more under "Authorization Request" here:
	// https://www.oauth.com/oauth2-servers/redirect-uris/redirect-uri-validation/
	if s.bldrClient != nil {
		if r.FormValue("client_id") != "" && r.FormValue("client_id") != s.bldrClient.ClientID {
			http.Error(w,
				fmt.Sprintf("currently only Habitat Builder is supported for Oauth2 integration. "+
					"expected bldr client_id: %s, actual client_id: %s",
					s.bldrClient.ClientID, r.FormValue("client_id")),
				http.StatusUnauthorized)
			return
		}
		if r.FormValue("client_id") == s.bldrClient.ClientID {
			redirectURI := r.FormValue("redirect_uri")
			if redirectURI != s.bldrClient.SignInURL.String() {
				s.invalidRedirectURIError(w, s.bldrClient.ClientID, s.bldrClient.SignInURL.String(), redirectURI)
				return
			}

			// TODO (tc) This is simply stored so we can determine if a request came from bldr or Automate UI
			// when it comes back from the dex request chain in callbackHandler (the actual stored request_uri isn't used).
			// It's not ideal but we currently don't have a way of threading redirect_uri back from the dex call chain.
			// Might want to consider just switching this to a boolean with a descriptive name like bldrRequest as the
			// key in the session.
			if err := sess.PutString(w, redirectURIKey, redirectURI); err != nil {
				http.Error(w, "failed to store redirect URI in session", http.StatusInternalServerError)
				return
			}
		}
	}

	relayState, err := generateRelayState()
	if err != nil {
		s.log.Errorf("couldn't generate random relay state: %s", err)
		httpError(w, http.StatusInternalServerError)
		return
	}
	// take state we've gotten from the client, store it
	if clientState := r.FormValue("state"); clientState != "" {
		s.log.Debugf("storing clientState %s", clientState)
		if err := sess.PutString(w, clientStateKeyPrefix+relayState, clientState); err != nil {
			http.Error(w, "failed to set client state", http.StatusInternalServerError)
			return
		}
	}

	// bind relay state to session
	if err := sess.PutBool(w, relayStateKeyPrefix+relayState, true); err != nil {
		s.log.Errorf("couldn't put relay state into session: %s", err)
		httpError(w, http.StatusInternalServerError)
		return
	}
	s.log.Debugf("stored relayState %s", relayState)

	opts := []oauth2.AuthCodeOption{}
	if connectorID := s.connectorIDFromRequest(r); connectorID != "" {
		opts = append(opts, oauth2.SetAuthURLParam("connector_id", connectorID))
	}
	authCodeURL := s.client.AuthCodeURL(relayState, opts...)

	if err := sess.PutBool(w, aliveKey, true); err != nil {
		s.log.Errorf("couldn't put aliveness state into session: %s", err)
		httpError(w, http.StatusInternalServerError)
		return
	}

	http.Redirect(w, r, authCodeURL, http.StatusSeeOther)
}

func (s *Server) connectorIDFromRequest(r *http.Request) string {
	token := r.FormValue("id_token_hint")
	if token == "" { // ignore errors, we just use this as a hint
		s.log.Error("no id_token_hint")
		return ""
	}
	// Note(sr): The way the verifier is set up, this allows for expired tokens --
	// so, this wouldn't fail unless we're given something that is not a JWT at all.
	idToken, err := s.client.Verify(r.Context(), token)
	if err != nil {
		s.log.Errorf("failed to verify id_token_hint: %v", err)
		return ""
	}
	var claims struct {
		Federated struct {
			ConnectorID string `json:"connector_id"`
		} `json:"federated_claims"`
	}
	err = idToken.Claims(&claims)
	if err != nil {
		s.log.Errorf("failed to read claims from id_token_hint: %v", err)
		return ""
	}
	return claims.Federated.ConnectorID
}

func httpError(w http.ResponseWriter, code int) {
	http.Error(w, http.StatusText(code), code)
}

func (s *Server) maybeExchangeRefreshTokenForIDToken(ctx context.Context,
	refreshToken, rawIDToken string) (*oauth2.Token, error) {
	idToken, err := s.client.Verify(ctx, rawIDToken)
	if err != nil {
		return nil, errors.Wrap(err, "verify id_token")
	}

	t, err := oidc.TokenFromIDToken(idToken, rawIDToken, refreshToken)
	if err != nil {
		return nil, errors.Wrap(err, "build token from id_token")
	}

	// This makes a refresh happen although it's not required "just yet" (but soon)
	t.Expiry = t.Expiry.Add(-s.remainingDuration)

	token, err := s.client.TokenSource(ctx, t).Token()
	if err != nil {
		return nil, errors.Wrap(err, "exchange refresh token")
	}
	return token, nil
}

func generateRelayState() (string, error) {
	bs := make([]byte, numRelayStateRandomBytes)
	n, err := rand.Read(bs)
	if err != nil || n != numRelayStateRandomBytes {
		return "", errors.Wrap(err, "read random bytes")
	}
	return base64.URLEncoding.EncodeToString(bs), nil
}

func createSCSManager(store scs.Store) *scs.Manager {
	manager := scs.NewManager(store)
	manager.Persist(true)
	manager.Secure(true)

	return manager
}

// To avoid timing attacks[1], we need to first check that the length of the two strings is
// the same (safe since len is a constant time operator for strings[2]) and then use
// subtle to compare the bytes in a constant time operation.
//
// [1] https://codahale.com/a-lesson-in-timing-attacks/
// [2] https://golang.org/ref/spec#Length_and_capacity
func safeCmpStrings(a, b string) bool {
	if len(a) != len(b) {
		return false
	}
	return subtle.ConstantTimeCompare([]byte(a), []byte(b)) == 1
}

func (s *Server) invalidRedirectURIError(w http.ResponseWriter, clientID, expected, actual string) {
	http.Error(w,
		fmt.Sprintf("failed to pass valid redirect_uri for client_id: %s, expected: %s, registered for client: %s",
			clientID, expected, actual),
		http.StatusUnauthorized)
}
