package usermgmt

import (
	"context"
)

// UserMgmt is the interface for clients that can manage local users and their team membership.
type UserMgmt interface {
	CreateUser(ctx context.Context, name, email, password string) (userID string, wasCreated bool, err error)
	AddUserToAdminTeam(ctx context.Context, userID string) error
}
