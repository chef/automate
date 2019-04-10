package mock

import (
	"context"
	"errors"

	"go.uber.org/zap"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is used for configuring mock adapters
type Config struct {
	Users          map[string]users.ShowUser `json:"users"` // only initial users, updates are in-memory
	FailUserDelete bool
	Validate       func(string) bool
	FailValidate   bool
}

// Open initializes the mock adapter
func (c *Config) Open(logger *zap.Logger, _ *certs.ServiceCerts) (users.Adapter, error) {
	var e error
	if c.FailValidate {
		e = errors.New("error in validation")
	}
	return &mock{users: c.Users, failUserDelete: c.FailUserDelete, validate: c.Validate, validateErr: e}, nil
}

type mock struct {
	failUserDelete bool
	users          users.Users
	validateErr    error
	validate       func(string) bool
}

func (m *mock) GetUsers(ctx context.Context) (users.Users, error) {
	return m.users, nil
}

func (m *mock) GetUser(ctx context.Context, email string) (*users.ShowUser, error) {
	for _, user := range m.users {
		if user.Email == email {
			return &user, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (m *mock) GetPassword(ctx context.Context, email string) (*users.UserWithHashedPass, error) {
	for _, user := range m.users {
		if user.Email == email {
			hash, _ := m.HashPassword("previous_password_for_user")
			usr := users.UserWithHashedPass{
				ShowUser: users.ShowUser{
					ID:    user.ID,
					Name:  user.Name,
					Email: user.Email,
				},
				HashedPass: hash,
			}
			return &usr, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (m *mock) CreateUser(ctx context.Context, user users.User) (*users.ShowUser, error) {
	if _, exists := m.users[user.Email]; !exists {
		if validUser(user) {
			newUser := users.ShowUser{
				ID:    user.ID,
				Email: user.Email,
				Name:  user.Name,
			}

			m.users[user.Email] = newUser
			return &newUser, nil
		}
	} else {
		return nil, &users.AlreadyExistsError{}
	}

	return nil, errors.New("Failed to create user")
}

func (m *mock) CreateUserWithHashedPass(ctx context.Context,
	user users.UserWithHashedPass) (*users.ShowUser, error) {
	return m.CreateUser(ctx, users.User{
		ID:       user.ID,
		Name:     user.Name,
		Email:    user.Email,
		Password: "mock adapter ignores this anyways, just checks it's non-empty",
	})
}

func (m *mock) UpdateUser(ctx context.Context, user users.UserWithHashedPass) (*users.ShowUser, error) {
	if _, userExists := m.users[user.Email]; userExists {
		if validUserWithHashedPass(user) {
			updatedUser := users.ShowUser{
				ID:    user.ID,
				Email: user.Email,
				Name:  user.Name,
			}

			m.users[user.Email] = updatedUser
			return &updatedUser, nil
		}
		return nil, errors.New("Must complete all fields")
	}

	return nil, &users.NotFoundError{}
}

func (m *mock) DeleteUser(ctx context.Context, email string) (bool, error) {
	if m.failUserDelete {
		return false, errors.New("Failed to delete user")
	}
	if _, userExists := m.users[email]; userExists {
		delete(m.users, email)
		return true, nil
	}

	return false, &users.NotFoundError{}
}

func (m *mock) HashPassword(password string) ([]byte, error) {
	return []byte("mocked_password_hash"), nil
}

func (m *mock) ValidatePassword(_ context.Context, email, password string) (bool, error) {
	return m.validate(email), m.validateErr
}

func validUser(user users.User) bool {
	return user.ID != "" && user.Email != "" && user.Password != "" && user.Name != ""
}

func validUserWithHashedPass(user users.UserWithHashedPass) bool {
	return user.ID != "" && user.Email != "" && user.Name != ""
}
