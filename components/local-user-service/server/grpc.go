package server

import (
	"context"
	"fmt"
	"net"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/common"
	"github.com/chef/automate/api/interservice/local_user"
	teams "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/components/local-user-service/constants"
	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
	uuid "github.com/chef/automate/lib/uuid4"
)

// NewGRPCServer returns a server that provides our services: clients, users,
// and authentication requests.
func (s *Server) NewGRPCServer() *grpc.Server {
	g := s.connFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				inputValidationInterceptor(),
			),
		),
	)
	local_user.RegisterUsersMgmtServer(g, s)
	health.RegisterHealthServer(g, s.health)
	reflection.Register(g)
	return g
}

func inputValidationInterceptor() grpc.UnaryServerInterceptor {
	return func(ctx context.Context,
		req interface{},
		_ *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {
		if req, ok := req.(interface {
			Validate() error
		}); ok {
			if err := req.Validate(); err != nil {
				return nil, status.Error(codes.InvalidArgument, err.Error())
			}
		}
		return handler(ctx, req)
	}
}

// GRPC serves the GRPC API for all services. On success, it never returns.
func (s *Server) GRPC(addr string) error {
	list, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	return s.NewGRPCServer().Serve(list)
}

// GetUsers fetches hash of users, implemented without wrapping usersAPI struct
func (s *Server) GetUsers(ctx context.Context, _ *local_user.GetUsersReq) (*local_user.Users, error) {
	us, err := s.users.GetUsers(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	users := map[string]*local_user.User{}

	for _, user := range us {
		users[user.Email] = toUserResp(&user)
	}

	resp := local_user.Users{
		Users: users,
	}

	return &resp, nil
}

// GetUser fetches user by email, which dex uses as unique identifier
func (s *Server) GetUser(ctx context.Context, req *local_user.Email) (*local_user.User, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	us, err := s.users.GetUser(ctx, req.Email)
	if err != nil {
		if _, ok := errors.Cause(err).(*users.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toUserResp(us), nil
}

// CreateUser adds a user with a generated UUID to dex
func (s *Server) CreateUser(ctx context.Context, req *local_user.CreateUserReq) (*local_user.User, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if err := s.validator.Validate(req.Password); err != nil {
		return nil, status.Errorf(codes.InvalidArgument, constants.PasswordValidationErrorFormat, err)
	}

	id, err := uuid.NewV4()
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	newUser := users.User{
		ID:       id.String(),
		Name:     req.Name,
		Email:    req.Email,
		Password: req.Password,
	}

	us, err := s.users.CreateUser(ctx, newUser)
	if err != nil {
		if _, ok := errors.Cause(err).(*users.AlreadyExistsError); ok {
			return nil, status.Error(codes.AlreadyExists, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toUserResp(us), nil
}

// DeleteUser deletes user from dex
func (s *Server) DeleteUser(ctx context.Context, req *local_user.Email) (*local_user.DeleteUserResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	us, err := s.users.GetUser(ctx, req.Email)
	if err != nil {
		if _, ok := errors.Cause(err).(*users.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	_, err = s.users.DeleteUser(ctx, req.Email)
	if err != nil {
		if _, ok := errors.Cause(err).(*users.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	purgeReq := &teams.PurgeUserMembershipReq{
		UserId: us.ID,
	}

	_, err = s.teamsClient.PurgeUserMembership(ctx, purgeReq)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	authzPurgeReq := &authz.PurgeSubjectFromPoliciesReq{
		Subject: "user:local:" + us.Name,
	}

	_, err = s.authzClient.PurgeSubjectFromPolicies(ctx, authzPurgeReq)
	if err != nil {
		s.logger.Warn(fmt.Sprintf("failed to purge subjects on user delete: %s", err.Error()))
		return nil, status.Error(codes.Internal,
			fmt.Sprintf("the user named %s with id %s was successfully deleted but its "+
				"subject could not be purged from the policies: %s", us.Name, us.ID, err.Error()))
	}

	return &local_user.DeleteUserResp{}, nil
}

// UpdateSelf updates the user's name and, optionally, password.
// If password is updated, the user must provide the previous_password.
func (s *Server) UpdateSelf(ctx context.Context, req *local_user.UpdateSelfReq) (*local_user.User, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	updateUser := users.UserWithHashedPass{ShowUser: users.ShowUser{
		Name:  req.Name,
		Email: req.Email,
		ID:    req.Id,
	},
	}

	if req.Password != "" {
		if req.PreviousPassword == "" {
			return nil, status.Error(codes.InvalidArgument,
				"to update existing password, provide previous password")
		}

		// check if previous password is OK
		s.logger.Info(fmt.Sprintf("attempting to validate user %s by logging in so we can update their password", req.Email))
		ok, err := s.users.ValidatePassword(ctx, req.Email, req.PreviousPassword)
		if err != nil {
			return nil, status.Error(codes.Internal, "could not validate previous password")
		}
		if !ok {
			// Note: this could be used for guessing passwords; however, to guess a
			// password using this API, the sender needs some kind of authentication
			// already. You can also guess passwords completely unauthenticated by
			// just querying dex.
			return nil, status.Error(codes.InvalidArgument, "previous password does not match")
		}

		// If successful, we are set to update using the new hashed password.
		updatedHash, err := s.validateAndHashPassword(ctx, req.Password)
		if err != nil {
			return nil, err
		}
		updateUser.HashedPass = updatedHash
	}

	us, err := s.updateUserOrParseError(ctx, updateUser)
	if err != nil {
		return nil, err
	}

	return toUserResp(us), nil
}

// UpdateUser updates the user's name and, optionally, password.
func (s *Server) UpdateUser(ctx context.Context, req *local_user.UpdateUserReq) (*local_user.User, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	updateUser := users.UserWithHashedPass{ShowUser: users.ShowUser{
		Name:  req.Name,
		Email: req.Email,
		ID:    req.Id,
	},
	}

	// Update hash if we want a new password, otherwise pass empty hash to dex.
	if req.Password != "" {
		updatedHash, err := s.validateAndHashPassword(ctx, req.Password)
		s.logger.Warn(fmt.Sprintf("updated: %v", updatedHash))
		if err != nil {
			return nil, err
		}
		updateUser.HashedPass = updatedHash
	}

	us, err := s.updateUserOrParseError(ctx, updateUser)
	if err != nil {
		return nil, err
	}

	return toUserResp(us), nil
}

func (s *Server) validateAndHashPassword(_ context.Context, password string) ([]byte, error) {
	if err := s.validator.Validate(password); err != nil {
		return nil, status.Errorf(codes.InvalidArgument, constants.PasswordValidationErrorFormat, err)
	}
	hashedPass, err := s.users.HashPassword(password)
	if err != nil {
		return nil, status.Error(codes.Internal,
			errors.Wrap(err, "generate hashed password").Error())
	}
	return hashedPass, nil
}

func (s *Server) updateUserOrParseError(ctx context.Context,
	updateUser users.UserWithHashedPass) (*users.ShowUser, error) {
	us, err := s.users.UpdateUser(ctx, updateUser)
	if err != nil {
		if _, ok := errors.Cause(err).(*users.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}
	return us, nil
}

func toUserResp(u *users.ShowUser) *local_user.User {
	return &local_user.User{
		Id:    u.ID,
		Name:  u.Name,
		Email: u.Email,
	}
}
