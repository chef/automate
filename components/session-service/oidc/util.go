package oidc

import (
	oidc "github.com/coreos/go-oidc"
	"golang.org/x/oauth2"
)

// TokenFromIDToken creates a synthetic oauth2.Token instance with which
// the standard TokenSource implementation can be used (for handing refresh).
func TokenFromIDToken(idToken *oidc.IDToken, rawIDToken, refreshToken string) (*oauth2.Token, error) {
	t := oauth2.Token{
		RefreshToken: refreshToken,
		// We don't use access tokens currently, but oauth2 requires it to be there
		// for its `(*oauth2.Token).Valid()` check, which drives the
		// refresh-when-expired functionality we're using for our purposes.
		AccessToken: "x-not-a-real-token-just-a-necessary-placeholder",
		Expiry:      idToken.Expiry,
	}
	return t.WithExtra(map[string]interface{}{"id_token": rawIDToken}), nil
}
