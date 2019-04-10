package handler

import (
	"context"

	// upstream definitions
	"github.com/chef/automate/api/interservice/local_user"

	localUserReq "github.com/chef/automate/components/automate-gateway/api/auth/users/request"
	localUserRes "github.com/chef/automate/components/automate-gateway/api/auth/users/response"
)

// UsersServer stores client to an upstream auth service
type UsersServer struct {
	client local_user.UsersMgmtClient
}

// NewUsersMgmtServer initializes Server with client
func NewUsersMgmtServer(client local_user.UsersMgmtClient) *UsersServer {
	return &UsersServer{
		client: client,
	}
}

// TODO 2018/04/23 (sr): We've decided to reject Dex's focus on users-identified
// -by-email, and instead call the field `username` in our system. So, while it
// would make much sense to do that translation, "what we call username, dex
// calls email" in local-user-service, we _now_ merely want to fix this in our
// API _quickly_.

// CreateUser posts a user upstream
func (a *UsersServer) CreateUser(ctx context.Context, r *localUserReq.CreateUser) (*localUserRes.User, error) {
	username, useEmail := emailUnlessUsername(r.Email, r.Username)
	req := &local_user.CreateUserReq{
		Id:       r.Id,
		Name:     r.Name,
		Email:    username,
		Password: r.Password,
	}
	res, err := a.client.CreateUser(ctx, req)
	if err != nil {
		return nil, err
	}
	return fromUpstreamUser(res, useEmail), nil
}

// UpdateUser updates an existing user.
func (a *UsersServer) UpdateUser(ctx context.Context, r *localUserReq.UpdateUser) (*localUserRes.User, error) {
	username, useEmail := emailUnlessUsername(r.Email, r.Username)
	req := &local_user.UpdateUserReq{
		Id:       r.Id,
		Name:     r.Name,
		Email:    username,
		Password: r.Password,
	}
	res, err := a.client.UpdateUser(ctx, req)
	if err != nil {
		return nil, err
	}
	return fromUpstreamUser(res, useEmail), nil
}

// UpdateSelf allows a user to update their own info,
// requiring the previous password if they want to change password.
func (a *UsersServer) UpdateSelf(ctx context.Context, r *localUserReq.UpdateSelf) (*localUserRes.User, error) {
	req := &local_user.UpdateSelfReq{
		Id:               r.Id,
		Name:             r.Name,
		Email:            r.Username,
		Password:         r.Password,
		PreviousPassword: r.PreviousPassword,
	}
	res, err := a.client.UpdateSelf(ctx, req)
	if err != nil {
		return nil, err
	}
	return fromUpstreamUser(res, false), nil
}

// DeleteUserByUsername deletes an existing user upstream
func (a *UsersServer) DeleteUserByUsername(ctx context.Context,
	r *localUserReq.Username) (*localUserRes.DeleteUserResp, error) {
	_, err := a.client.DeleteUser(ctx, &local_user.Email{Email: r.Username})
	return &localUserRes.DeleteUserResp{}, err
}

// GetUserByUsername fetches an existing user from upstream
func (a *UsersServer) GetUserByUsername(ctx context.Context,
	r *localUserReq.Username) (*localUserRes.User, error) {
	res, err := a.client.GetUser(ctx, &local_user.Email{Email: r.Username})
	if err != nil {
		return nil, err
	}
	return fromUpstreamUser(res, false), nil
}

// GetUsers fetches a map of existing users and extracts each user into an array
func (a *UsersServer) GetUsers(ctx context.Context, _ *localUserReq.GetUsersReq) (*localUserRes.Users, error) {
	res, err := a.client.GetUsers(ctx, &local_user.GetUsersReq{})
	if err != nil {
		return nil, err
	}
	us := []*localUserRes.User{}
	for _, u := range res.Users {
		us = append(us, fromUpstreamUser(u, true))
	}
	return &localUserRes.Users{Users: us}, nil
}

func fromUpstreamUser(c *local_user.User, useEmail bool) *localUserRes.User {
	res := localUserRes.User{
		Id:       c.Id,
		Name:     c.Name,
		Username: c.Email,
	}
	if useEmail {
		res.Email = c.Email
	}
	return &res
}

// emailUnlessUsername helps us support backward compatibility of the
// API. It returns username unless username is empty and email is
// non-empty. The boolean flag is true when the email value was
// returned.
func emailUnlessUsername(email, username string) (string, bool) {
	if username == "" && email != "" {
		return email, true
	}
	return username, false
}

// DeleteUser deletes an existing user from upstream
// Deprecated: 20180424200344
// -----------------------------------------------
func (a *UsersServer) DeleteUser(ctx context.Context, r *localUserReq.Email) (*localUserRes.DeleteUserResp, error) {
	_, err := a.client.DeleteUser(ctx, &local_user.Email{Email: r.Email})
	return &localUserRes.DeleteUserResp{}, err
}

// GetUser fetches an existing user from upstream
// Deprecated: 20180424200344
func (a *UsersServer) GetUser(ctx context.Context, r *localUserReq.Email) (*localUserRes.User, error) {
	res, err := a.client.GetUser(ctx, &local_user.Email{Email: r.Email})
	if err != nil {
		return nil, err
	}
	return fromUpstreamUser(res, true), nil
}

// -----------------------------------------------
