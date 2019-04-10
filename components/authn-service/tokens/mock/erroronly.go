package mock

import (
	"context"

	"go.uber.org/zap"

	"github.com/pkg/errors"

	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/tls/certs"
)

// ErrorOnlyConfig is used to set up a mock adapter that only returns errors. To
// be used for testing.
type ErrorOnlyConfig struct {
	Msg string
}

type state struct {
	err error
}

// Open is for instantiating the error-only mock adapter
func (cfg *ErrorOnlyConfig) Open(_ *certs.ServiceCerts, logger *zap.Logger) (tokens.Storage, error) {
	return &state{err: errors.New(cfg.Msg)}, nil
}

func (s *state) CreateToken(context.Context, string, string, bool, []string) (*tokens.Token, error) {
	return nil, s.err
}
func (s *state) CreateTokenWithValue(context.Context,
	string, string, string, bool, []string) (*tokens.Token, error) {
	return nil, s.err
}
func (s *state) CreateLegacyTokenWithValue(context.Context, string) (*tokens.Token, error) {
	return nil, s.err
}
func (s *state) DeleteToken(context.Context, string) error {
	return s.err
}
func (s *state) UpdateToken(context.Context, string, string, bool, []string) (*tokens.Token, error) {
	return nil, s.err
}
func (s *state) GetToken(context.Context, string) (*tokens.Token, error) {
	return nil, s.err
}
func (s *state) GetTokenIDWithValue(context.Context, string) (string, error) {
	return "", s.err
}
func (s *state) GetTokens(context.Context) ([]*tokens.Token, error) {
	return nil, s.err
}
