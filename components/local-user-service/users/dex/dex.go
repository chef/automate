package dex

import (
	"context"

	"github.com/dexidp/dex/api"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"golang.org/x/crypto/bcrypt"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is used for configuring dex adapters
type Config struct {
	GRPCHost string `json:"grpcHost"`
}

type state struct {
	c api.DexClient
	l *zap.Logger
}

// Open initializes the dex adapter
func (c *Config) Open(l *zap.Logger, cs *certs.ServiceCerts) (users.Adapter, error) {
	connFactory := secureconn.NewFactory(*cs)
	conn, err := connFactory.Dial("automate-dex", c.GRPCHost)
	if err != nil {
		return nil, err
	}
	//defer conn.Close()

	return &state{c: api.NewDexClient(conn), l: l}, nil
}

func (s *state) GetUsers(ctx context.Context) (users.Users, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get passwords")
	}

	// generates map of returned users
	usrs := make(map[string]users.ShowUser, len(resp.Passwords))
	for _, p := range resp.Passwords {
		usrs[p.Email] = users.ShowUser{
			ID:    p.UserId,
			Name:  p.Username,
			Email: p.Email,
		}
	}
	return usrs, nil
}

// NOTE: dex only has ListPasswords, no singular ListPassword
func (s *state) GetUser(ctx context.Context, email string) (*users.ShowUser, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get password")
	}

	// searches returned users for user
	for _, p := range resp.Passwords {
		if p.Email == email {
			usr := users.ShowUser{
				ID:    p.UserId,
				Name:  p.Username,
				Email: p.Email,
			}
			return &usr, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (s *state) GetPassword(ctx context.Context, email string) (*users.UserWithHashedPass, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get password")
	}

	// searches returned users for user
	for _, p := range resp.Passwords {
		if p.Email == email {
			editUser := users.UserWithHashedPass{
				ShowUser: users.ShowUser{ID: p.UserId,
					Name:  p.Username,
					Email: p.Email,
				},
				HashedPass: p.Hash,
			}
			return &editUser, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (s *state) CreateUser(ctx context.Context, user users.User) (*users.ShowUser, error) {
	// hashes user's password string
	hashedPass, err := s.HashPassword(user.Password)
	if err != nil {
		return nil, errors.Wrap(err, "create password")
	}

	// creates password request
	createReq := &api.CreatePasswordReq{
		Password: &api.Password{
			Email:    user.Email,
			UserId:   user.ID,
			Username: user.Name,
			Hash:     hashedPass,
		}}

	return s.createPassword(ctx, createReq)
}

func (s *state) CreateUserWithHashedPass(ctx context.Context, user users.UserWithHashedPass) (*users.ShowUser, error) {
	// creates password request
	createReq := &api.CreatePasswordReq{
		Password: &api.Password{
			Email:    user.Email,
			UserId:   user.ID,
			Username: user.Name,
			Hash:     user.HashedPass,
		}}

	return s.createPassword(ctx, createReq)
}

func (s *state) createPassword(ctx context.Context, req *api.CreatePasswordReq) (*users.ShowUser, error) {
	// GRPC call to create Password (User) in dex
	resp, err := s.c.CreatePassword(ctx, req)
	if err != nil {
		return nil, errors.Wrap(err, "create password")
	}
	if resp.AlreadyExists {
		return nil, &users.AlreadyExistsError{}
	}

	newUser := users.ShowUser{
		ID:    req.GetPassword().UserId,
		Email: req.GetPassword().Email,
		Name:  req.GetPassword().Username,
	}
	return &newUser, nil
}

// UpdateUser makes a GRPC call to update an existing user in dex
func (s *state) UpdateUser(ctx context.Context, user users.UserWithHashedPass) (*users.ShowUser, error) {
	// update request
	updateReq := &api.UpdatePasswordReq{
		Email:       user.Email,
		NewHash:     user.HashedPass,
		NewUsername: user.Name,
	}

	// GRPC call to update Password
	resp, err := s.c.UpdatePassword(ctx, updateReq)
	if err != nil {
		return nil, errors.Wrap(err, "update password")
	}
	if resp.NotFound {
		return nil, &users.NotFoundError{}
	}

	updatedUser := users.ShowUser{
		ID:    user.ID,
		Email: user.Email,
		Name:  user.Name,
	}
	return &updatedUser, nil
}

func (s *state) DeleteUser(ctx context.Context, email string) (bool, error) {
	// delete request
	deleteReq := &api.DeletePasswordReq{
		Email: email,
	}
	// GRPC call to delete a user in dex
	resp, err := s.c.DeletePassword(ctx, deleteReq)
	if err != nil {
		return false, errors.Wrap(err, "delete password")
	}
	if resp.NotFound {
		return false, &users.NotFoundError{}
	}

	return true, nil
}

func (s *state) HashPassword(password string) ([]byte, error) {
	if password == "" {
		return nil, errors.New("password cannot be nil")
	}

	cost := 10
	hashedPass, err := bcrypt.GenerateFromPassword([]byte(password), cost)
	if err != nil {
		return nil, errors.Wrap(err, "set password field")
	}

	return hashedPass, nil
}

func (s *state) ValidatePassword(ctx context.Context, username, password string) (bool, error) {
	resp, err := s.c.VerifyPassword(ctx, &api.VerifyPasswordReq{Email: username, Password: password})
	if err != nil {
		return false, errors.Wrap(err, "verify password")
	}
	if resp.NotFound {
		return false, nil
	}
	return resp.Verified, nil
}
