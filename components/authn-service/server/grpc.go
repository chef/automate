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
	"github.com/chef/automate/api/interservice/authz"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
)

type tokenAPI struct {
	ts             tokens.Storage
	policiesClient authz.PoliciesServiceClient
	pv             tokens.ProjectValidator
}

// NewGRPCServer returns a server that provides our services: token
// and authentication requests.
func (s *Server) NewGRPCServer(policiesClient authz.PoliciesServiceClient, pv tokens.ProjectValidator) *grpc.Server {
	g := s.connFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				inputValidationInterceptor(),
			),
		),
	)
	api.RegisterTokensMgmtServiceServer(g, newTokenAPI(s.TokenStorage, policiesClient, pv))
	health.RegisterHealthServer(g, s.health)
	api.RegisterAuthenticationServiceServer(g, s)
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

func newTokenAPI(ts tokens.Storage, policiesClient authz.PoliciesServiceClient, pv tokens.ProjectValidator) api.TokensMgmtServiceServer {
	return &tokenAPI{
		ts:             ts,
		policiesClient: policiesClient,
		pv:             pv,
	}
}

func (a *tokenAPI) CreateToken(ctx context.Context, req *api.CreateTokenReq) (*api.Token, error) {
	t, err := a.ts.CreateToken(ctx, req.Id, req.Name, req.Active, req.Projects)
	if err != nil {
		// if the error is already a GRPC status code, return that directly.
		if _, ok := status.FromError(err); ok {
			return nil, err
		}
		if _, ok := err.(*tokens.ConflictError); ok {
			return nil, status.Error(codes.AlreadyExists, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func (a *tokenAPI) CreateTokenWithValue(
	ctx context.Context, req *api.CreateTokenWithValueReq) (*api.Token, error) {
	t, err := a.ts.CreateTokenWithValue(ctx, req.Id, req.Value, req.Name, req.Active, req.Projects)
	if err != nil {
		// if the error is already a GRPC status code, return that directly.
		if _, ok := status.FromError(err); ok {
			return nil, err
		}
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
	_, err = a.policiesClient.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{
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
	t, err := a.ts.UpdateToken(ctx, req.Id, req.Name, req.Active, req.Projects)
	if err != nil {
		// if the error is already a GRPC status code, return that directly.
		if _, ok := status.FromError(err); ok {
			return nil, err
		}
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return toTokenResp(t), nil
}

func toTokenResp(t *tokens.Token) *api.Token {
	return &api.Token{
		Id:       t.ID,
		Active:   t.Active,
		Value:    t.Value,
		Name:     t.Name,
		Created:  t.Created.Format(time.RFC3339),
		Updated:  t.Updated.Format(time.RFC3339),
		Projects: t.Projects,
	}
}
