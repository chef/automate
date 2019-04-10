package oidc

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"github.com/gorilla/mux"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// This test covers the "local development" feature of the oidc authenticator:
// When the configured issuer contains "localhost", outgoing requests of the
// authenticator's http.Client are _rewritten_ to go to upstream instead.
func TestNewAuthenticatorRedirectsOIDCRequestsAtUpstream(t *testing.T) {

	// Note 2017/12/06 (sr): the "localhost" case is kept for regression
	// purposes -- there used to be special handling of "localhost", but
	// what once was special is the (only) default behavior now.
	for _, issuer := range []string{
		"https://localhost/dex",
		"https://a2-fqdn.example/dex",
	} {

		r := mux.NewRouter()
		r.HandleFunc("/dex/.well-known/openid-configuration", func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/plain; charset=utf-8")
			fmt.Fprintln(w, discoveryDocForIssuer(issuer))
		})
		up := httptest.NewServer(r)
		defer up.Close()

		clientID := "invisibility"
		upstream, _ := url.Parse(up.URL)

		// If this doesn't break, we've gotten discovery doc from upstream instead of
		// from the issuer (which would have failed)
		serviceCerts := helpers.LoadDevCerts(t, "authn-service")
		_, err := NewAuthenticator(issuer, clientID, upstream, false, 0, serviceCerts, logger)
		if err != nil {
			t.Fatalf("create authenticator: %v", err)
		}
	}
}

func TestDexIssuedTokenIsUnderstoodProperly(t *testing.T) {
	issuer := "https://a2-dev.test/dex"
	keys := `{
  "keys": [
    {
      "use": "sig",
      "kty": "RSA",
      "kid": "9a4fe2a0ef6575a3116af60b8c1f1f46ef1bc457",
      "alg": "RS256",
      "n": "vZgOh6JoBMwxvUoiGxucy-VsJLwti2-kAZwDuNVES1yY2KNJeqDol2RcVshgEQTEEaiXPGGc2xgE72RGKhnWIgEC9V-9cfV-619vMfH1v8Z7pKDnuvnU_ER-3oCe7_Y5Kj2NGb4ErEykk2-1eN6oHXPRavLpfclk0dx7P3lvQa5pqaTxz8pSOpREbrAY5Hrvr6OoAQR53l-FyNakuq0YKSiWRnTGn3T_jSTQ6QLqir7TThWn10iCYvTv-otts9ZdYzyTxfiZ9cVu258w789iPBHw9B1lULZej90TKFSmF7uVpJuV1UxbLKxqlkMn0aZ9RWI768gf-CSxzzLu1lwo2Q",
      "e": "AQAB"
    }
  ]
}`
	// Decode this real token at https://jwt.show or https://jwt.io
	token := `eyJhbGciOiJSUzI1NiIsImtpZCI6IjlhNGZlMmEwZWY2NTc1YTMxMTZhZjYwYjhjMWYxZjQ2ZWYxYmM0NTcifQ.eyJpc3MiOiJodHRwczovL2EyLWRldi50ZXN0L2RleCIsInN1YiI6IkNpUTVZams1TmpKak55MDNOR1JqTFRSbFpHWXRZVEl5Tnkxa1pqZ3hZVFpsTjJRM1lqQVNCV3h2WTJGcyIsImF1ZCI6ImF1dG9tYXRlLXNlc3Npb24iLCJleHAiOjE1MjA5NTA3NTksImlhdCI6MTUyMDg2NDM1OSwiYXRfaGFzaCI6ImE3dEh1aF9UX0ZFQUhrZTkwNXBGM2ciLCJlbWFpbCI6ImFkbWluQGV4YW1wbGUuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsIm5hbWUiOiJhZG1pbiIsImZlZGVyYXRlZF9jbGFpbXMiOnsiY29ubmVjdG9yX2lkIjoibG9jYWwiLCJ1c2VyX2lkIjoiOWI5OTYyYzctNzRkYy00ZWRmLWEyMjctZGY4MWE2ZTdkN2IwIn19.qrgreu5Jp1l_Xp4a2XaB_9osimgmba2ejqWppRajckbXJ1m5MbCdC2ssfO6DdhXQt2nwey4cI-OLdt9uK-pyta4FKD1jndFA2s4CT486-UJud-iidQC91UJu6FOdfK2NLGyVQTiqKssEQXZ2iZlA0QjNlO4bSAPDIdFlTMr9Ib2prkSrS1-lOQKYZ_3aLYEvWP9GsHhLuEPye4ugE85cylqyD405NbMF2-4imKp_qU-AWpZwKDWsXNgs1O457UVAp4JXk_SgZPWWQa-K_Vr9iiSYFrci-qN7OLROwebgWBdXA-JJ3yt4LwFoAiUlTly_IsDuhAbcvyb_Sz-Udcbslw`

	actual, err := authenticate(t, issuer, keys, token)
	require.NoError(t, err)
	assert.Equal(t, "user:local:admin@example.com", actual.Subject())
	assert.Empty(t, actual.Teams())
}

func TestDexIssuedSAMLTokenWithGroupsIsUnderstoodProperly(t *testing.T) {
	issuer := "https://a2-local-fresh-install-acceptance.cd.chef.co/dex"
	keys := `{
  "keys": [
    {
      "use": "sig",
      "kty": "RSA",
      "kid": "9dc85122dba51d3fde3e4494ffcb5fbfb3026515",
      "alg": "RS256",
      "n": "1mC7Xdw5cwjxCNsdSguMWdgrNUHeGuE96B3EzQEsLVfGxOXYtUbg9FOxpa-o3rdBxS5tGcKgDESBlA26NKrBkXCymb6maQglDvlIwvDbZJ1HWEhv7ddL_MehIQThYNoQ7Jq11Dm9O_hmVncL8J6OY71_JSdeBSm12_0LjwFIHLzB1mhyJMvYR9r4E41mgZtxSOGANCmfmQSrfRo-hsO2CBk_ie_ocqSC25uTJyGranavv_UIMdUgXgRePFDROTyIAWB3OUBdCEZVoyZ2ppTkU5ZjvuvJ6tlFydfG9v1CZyVKEj2grXVblKX1fzde9qEbpLCosHuNKX0sgf12sUkxaw",
      "e": "AQAB"
    }
  ]
}`
	// Decode this real token at https://jwt.show or https://jwt.io
	token := `eyJhbGciOiJSUzI1NiIsImtpZCI6IjlkYzg1MTIyZGJhNTFkM2ZkZTNlNDQ5NGZmY2I1ZmJmYjMwMjY1MTUifQ.eyJpc3MiOiJodHRwczovL2EyLWxvY2FsLWZyZXNoLWluc3RhbGwtYWNjZXB0YW5jZS5jZC5jaGVmLmNvL2RleCIsInN1YiI6IkNnaHpjbVZ1WVhSMWN4SUVjMkZ0YkEiLCJhdWQiOiJhdXRvbWF0ZS1zZXNzaW9uIiwiZXhwIjoxNTIxNjQ3MzY1LCJpYXQiOjE1MjE1NjA5NjUsImF0X2hhc2giOiJxeXd1VzZpU0daTjVuOTV3RUpsNC1RIiwiZW1haWwiOiJzcmVuYXR1c0BjaGVmLmlvIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImdyb3VwcyI6WyJFdmVyeW9uZSIsIk9mZmljZTM2NSBVc2VycyIsIkFsbCBEZXZlbG9wZXJzIiwiQ2hlZiBFTUVBIiwiQUQgSGVyb2t1IEJlcmtzaGVsZiIsIkVudGlyZSBDb21wYW55IiwiQUQgQ2hlZiBBV1MiLCJkZXZ0ZWFtIiwiQUQgQ2hlZiBEZXZlbG9wZXJzIl0sIm5hbWUiOiJzcmVuYXR1c0BjaGVmLmlvIiwiZmVkZXJhdGVkX2NsYWltcyI6eyJjb25uZWN0b3JfaWQiOiJzYW1sIiwidXNlcl9pZCI6InNyZW5hdHVzIn19.BBAOCFtpM_qQF3ygC3-4S74_lD45PH6Mv7wkkXoDdgnLLhRVpmzgjX4tcBPTJtpbV4ipjycY77oDfuYCF93gtUQPSCFMKTJh-tDJ3oCD5p8g_iR6Nlf2c8Ccs-FBg_9HJ6QxJmNkXL5Y6Re-IGHTtyprgZ7KLutEkWDGD2LUErsnPTctg0RF4ck_tZPCjIif0zWHSK9t9JVR8p3hQKBcJxxAU_Jn2GtSPllAeBEI_KIAq8M1QsCb4Obt3eWFLihT4BjGAk0I4KOYTiyMYv2kxmiEFMYh-aWPn5aAQOhpybmWczyGHC5qAzdyy2_wTmm0D8BLEqEelnF4Ka3Pdum6QQ`

	actual, err := authenticate(t, issuer, keys, token)
	require.NoError(t, err)
	assert.Equal(t, "user:saml:srenatus", actual.Subject())
	expectedTeams := []string{
		"team:saml:Everyone",
		"team:saml:Office365 Users",
		"team:saml:All Developers",
		"team:saml:Chef EMEA",
		"team:saml:AD Heroku Berkshelf",
		"team:saml:Entire Company",
		"team:saml:AD Chef AWS",
		"team:saml:devteam",
		"team:saml:AD Chef Developers",
	}
	assert.ElementsMatch(t, expectedTeams, actual.Teams())
}

func discoveryDocForIssuer(iss string) string {
	return `{
  "issuer": "` + iss + `",
  "authorization_endpoint": "` + iss + `/auth",
  "token_endpoint": "` + iss + `/token",
  "jwks_uri": "` + iss + `/keys",
  "response_types_supported": [
    "code",
    "id_token",
    "token"
  ],
  "subject_types_supported": [
    "public"
  ],
  "id_token_signing_alg_values_supported": [
    "RS256"
  ],
  "scopes_supported": [
    "openid",
    "email",
    "groups",
    "profile",
    "offline_access"
  ],
  "token_endpoint_auth_methods_supported": [
    "client_secret_basic"
  ],
  "claims_supported": [
    "aud",
    "email",
    "email_verified",
    "exp",
    "iat",
    "iss",
    "locale",
    "name",
    "sub"
  ]
}`
}

func authenticate(t *testing.T, issuer, keys, token string) (authenticator.Requestor, error) {
	r := mux.NewRouter()
	r.HandleFunc("/dex/.well-known/openid-configuration", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		fmt.Fprintln(w, discoveryDocForIssuer(issuer))
	})
	r.HandleFunc("/dex/keys", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		fmt.Fprintln(w, keys)
	})
	up := httptest.NewServer(r)
	defer up.Close()

	clientID := "automate-api"
	upstream, _ := url.Parse(up.URL)
	skipExpiry := true
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	authn, err := NewAuthenticator(issuer, clientID, upstream, skipExpiry, 0, serviceCerts, logger)
	if err != nil {
		t.Fatal(err)
	}

	req, err := http.NewRequest("GET", "/whatever", nil)
	if err != nil {
		t.Fatal(err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token))

	return authn.Authenticate(req)
}
