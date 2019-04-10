// nolint: govet
package mock

import (
	"context"

	"go.uber.org/zap"

	"github.com/pkg/errors"

	users "github.com/chef/automate/components/local-user-service/users"
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
func (cfg *ErrorOnlyConfig) Open(*zap.Logger, *certs.ServiceCerts) (users.Adapter, error) {
	return &state{errors.New(cfg.Msg)}, nil
}
func (s *state) GetUsers(context.Context) (users.Users, error) {
	return nil, s.err
}
func (s *state) GetPassword(ctx context.Context, email string) (*users.UserWithHashedPass, error) {
	return nil, s.err
}
func (s *state) GetUser(context.Context, string) (*users.ShowUser, error) {
	return nil, s.err
}
func (s *state) CreateUser(context.Context, users.User) (*users.ShowUser, error) {
	return nil, s.err
}
func (s *state) CreateUserWithHashedPass(context.Context, users.UserWithHashedPass) (*users.ShowUser, error) {
	return nil, s.err
}
func (s *state) UpdateUser(context.Context, users.UserWithHashedPass) (*users.ShowUser, error) {
	return nil, s.err
}
func (s *state) DeleteUser(context.Context, string) (bool, error) {
	return false, s.err
}
func (s *state) HashPassword(password string) ([]byte, error) {
	return nil, s.err
}

func (s *state) ValidatePassword(_ context.Context, email, password string) (bool, error) {
	return false, s.err
}
