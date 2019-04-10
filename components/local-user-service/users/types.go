package users

import "context"

// NotFoundError is the error returned when the user wasn't found, to discern this
// from other errors -- failures to reach the backend services, etc.
type NotFoundError struct{}

func (e *NotFoundError) Error() string { return "No user record found" }

// AlreadyExistsError is returned when the user's email already exists in the database
type AlreadyExistsError struct{}

func (e *AlreadyExistsError) Error() string { return "User already exists" }

// User is the type of a struct of User data
type User struct {
	ID       string `json:"id"`
	Name     string `json:"name" binding:"required"`
	Email    string `json:"email" binding:"required"`
	Password string `json:"password" binding:"required"`
}

// ShowUser does not have the password field so we don't accidentally expose it
type ShowUser struct {
	ID    string `json:"id" binding:"required"`
	Name  string `json:"name" binding:"required"`
	Email string `json:"email" binding:"required"`
}

type UserWithHashedPass struct {
	ShowUser
	HashedPass []byte
}

// Users is the type of a list of Users
type Users map[string]ShowUser

// Adapter is the interface for various adapters
type Adapter interface {
	GetUsers(context.Context) (Users, error)
	GetUser(ctx context.Context, email string) (*ShowUser, error)
	GetPassword(ctx context.Context, email string) (*UserWithHashedPass, error)
	CreateUser(ctx context.Context, user User) (*ShowUser, error)
	UpdateUser(ctx context.Context, user UserWithHashedPass) (*ShowUser, error)
	DeleteUser(ctx context.Context, email string) (bool, error)
	HashPassword(password string) ([]byte, error)
	ValidatePassword(ctx context.Context, username, password string) (valid bool, err error)
	CreateUserWithHashedPass(ctx context.Context, user UserWithHashedPass) (*ShowUser, error)
}
