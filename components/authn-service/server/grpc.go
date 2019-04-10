package server

import (
	"context"
	"time"

	"github.com/pkg/errors"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/authn"
	authz "github.com/chef/automate/api/interservice/authz/common"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
)

type tokenAPI struct {
	ts          tokens.Storage
	authzClient authz.SubjectPurgeClient
}

// NewGRPCServer returns a server that provides our services: token
// and authentication requests.
func (s *Server) NewGRPCServer(authzClient authz.SubjectPurgeClient) *grpc.Server {
	g := s.connFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				inputValidationInterceptor(),
			),
		),
	)
	api.RegisterTokensMgmtServer(g, newTokenAPI(s.token, authzClient))
	health.RegisterHealthServer(g, s.health)
	api.RegisterAuthenticationServer(g, s)
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

func newTokenAPI(ts tokens.Storage, authzClient authz.SubjectPurgeClient) api.TokensMgmtServer {
	return &tokenAPI{
		ts:          ts,
		authzClient: authzClient,
	}
}

func (a *tokenAPI) CreateToken(ctx context.Context, req *api.CreateTokenReq) (*api.Token, error) {
	t, err := a.ts.CreateToken(ctx, req.Id, req.Description, req.Active, req.Projects)
	if err != nil {
		if _, ok := err.(*tokens.ConflictError); ok {
			return nil, status.Error(codes.AlreadyExists, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func (a *tokenAPI) CreateTokenWithValue(
	ctx context.Context, req *api.CreateTokenWithValueReq) (*api.Token, error) {
	t, err := a.ts.CreateTokenWithValue(ctx, req.Id, req.Value, req.Description, req.Active, req.Projects)
	if err != nil {
		if _, ok := err.(*tokens.ConflictError); ok {
			return nil, status.Error(codes.AlreadyExists, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func (a *tokenAPI) GetToken(ctx context.Context, req *api.GetTokenReq) (*api.Token, error) {
	t, err := a.ts.GetToken(ctx, req.Id)
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func (a *tokenAPI) DeleteToken(ctx context.Context, req *api.DeleteTokenReq) (*api.DeleteTokenResp, error) {
	id := req.Id
	err := a.ts.DeleteToken(ctx, id)
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	tokenSubject := "token:" + id
	_, err = a.authzClient.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{
		Subject: tokenSubject,
	})
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"the token with id %s was successfully deleted but its "+
				"subject could not be purged from the policies: %s", id, err.Error())
	}

	return &api.DeleteTokenResp{}, nil
}

func (a *tokenAPI) GetTokens(ctx context.Context, req *api.GetTokensReq) (*api.Tokens, error) {
	ts, err := a.ts.GetTokens(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	var resp api.Tokens
	for _, t := range ts {
		resp.Tokens = append(resp.Tokens, toTokenResp(t))
	}

	return &resp, nil
}

func (a *tokenAPI) UpdateToken(ctx context.Context, req *api.UpdateTokenReq) (*api.Token, error) {
	t, err := a.ts.UpdateToken(ctx, req.Id, req.Description, req.Active, req.Projects)
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func toTokenResp(t *tokens.Token) *api.Token {
	return &api.Token{
		Id:          t.ID,
		Active:      t.Active,
		Value:       t.Value,
		Description: t.Description,
		Created:     t.Created.Format(time.RFC3339),
		Updated:     t.Updated.Format(time.RFC3339),
		Projects:    t.Projects,
	}
}
