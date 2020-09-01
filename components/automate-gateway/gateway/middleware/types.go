package middleware

import (
	"context"

	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/iam/v2/pairs"
)

// AuthorizationInterceptor abstracts the common logic that can be used for both
// interceptor types.
type AuthorizationInterceptor interface {
	UnaryServerInterceptor() grpc.UnaryServerInterceptor
	StreamServerInterceptor() grpc.StreamServerInterceptor
}

type DeploymentCertAuthOnly interface {
	MustUseDeploymentCertAuth()
}

type GRPCAuthorizationHandler interface {
	Handle(ctx context.Context, subjects []string, projects []string, req interface{}) (context.Context, error)
}

type HTTPAuthorizationHandler interface {
	IsAuthorized(ctx context.Context, subjects []string, resource, action string, projects []string) (context.Context, bool, error)
}

type IntrospectionHandler interface {
	FilterAuthorizedPairs(ctx context.Context, subjects []string, pairs []*pairs.Pair) ([]*pairs.Pair, error)
}

type AuthorizationHandler interface {
	HTTPAuthorizationHandler
	GRPCAuthorizationHandler
	IntrospectionHandler
}
