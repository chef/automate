package users

import (
	"context"

	"github.com/chef/automate/api/interservice/local_user"
	pb_common "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/common"
	pb_req "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/request"
	pb_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/response"
)

// Server is the server interface
type Server struct {
	users local_user.UsersMgmtClient
}

// NewServer creates a server with its client.
func NewServer(users local_user.UsersMgmtClient) *Server {
	return &Server{users: users}
}

// CreateUser creates a new user.
func (p *Server) CreateUser(
	ctx context.Context, in *pb_req.CreateUserReq) (*pb_resp.CreateUserResp, error) {

	// Note: This is where we're mapping data models: externally, an IAM v2
	// user has two properties: an ID (what they login with, equivalent to
	// their username), and a display name (called "name") here.
	// What l-u-s gets as Name will be the name, what it gets as Email will
	// be what the user logs in with. When processing, l-u-s will also give
	// user an ID (auto-generated UUID4), but that's kept internal.

	resp, err := p.users.CreateUser(ctx, &local_user.CreateUserReq{
		Name:     in.Name,
		Email:    in.Id,
		Password: in.Password,
	})
	if err != nil {
		return nil, err
	}

	// See note above -- the reverse mapping applies.
	return &pb_resp.CreateUserResp{User: convert(resp)}, nil
}

func (p *Server) ListUsers(
	ctx context.Context, _ *pb_req.ListUsersReq) (*pb_resp.ListUsersResp, error) {
	resp, err := p.users.GetUsers(ctx, &local_user.GetUsersReq{})
	if err != nil {
		return nil, err
	}
	users := make([]*pb_common.User, 0, len(resp.Users))
	for i, u := range resp.Users {
		users = append(users, &pb_common.User{Id: i, Name: u.Name, MembershipId: u.Id})
	}

	return &pb_resp.ListUsersResp{Users: users}, nil
}

func (p *Server) DeleteUser(
	ctx context.Context, req *pb_req.DeleteUserReq) (*pb_resp.DeleteUserResp, error) {
	_, err := p.users.DeleteUser(ctx, &local_user.Email{Email: req.Id})
	if err != nil {
		return nil, err
	}
	return &pb_resp.DeleteUserResp{}, nil
}

func (p *Server) GetUser(
	ctx context.Context, req *pb_req.GetUserReq) (*pb_resp.GetUserResp, error) {
	resp, err := p.users.GetUser(ctx, &local_user.Email{Email: req.Id})
	if err != nil {
		return nil, err
	}

	return &pb_resp.GetUserResp{User: convert(resp)}, nil
}

// Note: this isn't patchy: all fields need to be provided
func (p *Server) UpdateUser(
	ctx context.Context, req *pb_req.UpdateUserReq) (*pb_resp.UpdateUserResp, error) {
	resp, err := p.users.UpdateUser(ctx, &local_user.UpdateUserReq{
		Id:       req.Id, // this is required but ignored
		Email:    req.Id, // this is required but ignored
		Name:     req.Name,
		Password: req.Password,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.UpdateUserResp{User: convert(resp)}, nil
}

// UpdateSelf allows a user to update their own info,
// requiring the previous password if they want to change password.
func (p *Server) UpdateSelf(
	ctx context.Context, req *pb_req.UpdateSelfReq) (*pb_resp.UpdateSelfResp, error) {
	resp, err := p.users.UpdateSelf(ctx, &local_user.UpdateSelfReq{
		Id:               req.Id, // this is required but ignored
		Email:            req.Id, // this is required but ignored
		Name:             req.Name,
		Password:         req.Password,
		PreviousPassword: req.PreviousPassword,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.UpdateSelfResp{
		User: &pb_common.User{
			Id:           resp.Email,
			Name:         resp.Name,
			MembershipId: resp.Id,
		},
	}, nil
}

func convert(in *local_user.User) *pb_common.User {
	return &pb_common.User{
		Id:           in.Email,
		Name:         in.Name,
		MembershipId: in.Id,
	}
}
