package oidc

import (
	"context"

	oidc "github.com/coreos/go-oidc"
)

// IDTokenVerifier abstracts oidc.IDTokenVerifier (which is a struct) so we can
// pass a mock verifier for tests
type IDTokenVerifier interface {
	Verify(ctx context.Context, rawIDToken string) (DexIDToken, error)
}

// DexIDToken has a set of claims, and a subject
type DexIDToken interface {
	Claims(interface{}) error
	Subject() string
}

// DexToken is embedding (or renaming) the type oidc.IDToken so we can add
// methods to it.
type dexToken struct {
	token *oidc.IDToken
}

// Subject returns the underlying ID token's subject
func (d *dexToken) Subject() string {
	return d.token.Subject
}

// Claims lets us read out the token's claims. `sink` would be something like
//     var sink struct {
//         Groups []string `json:"groups"`
//     }
func (d *dexToken) Claims(sink interface{}) error {
	return d.token.Claims(sink)
}

type verifier struct {
	underlying *oidc.IDTokenVerifier
}

// Verify is calling the verifier's underlying IDTokenVerifier and re-wraps the
// returned ID token into our DexToken struct
func (v *verifier) Verify(ctx context.Context, rawIDToken string) (DexIDToken, error) {
	tok, err := v.underlying.Verify(ctx, rawIDToken)
	if err != nil {
		return nil, err
	}
	return &dexToken{token: tok}, nil
}

func newVerifier(provider *oidc.Provider, clientID string, skipExpiry bool) IDTokenVerifier {
	return &verifier{underlying: provider.Verifier(
		&oidc.Config{
			ClientID: clientID,
			// Note 2017/12/05 (sr): there's two separate client_ids that could come
			// up as audience claims in A2: automate-api (the public client used by
			// inspec), and automate-session, used by automate-ui through
			// session-service.
			SkipClientIDCheck:    true,
			SkipExpiryCheck:      skipExpiry,
			SupportedSigningAlgs: []string{oidc.RS256},
		},
	)}
}
