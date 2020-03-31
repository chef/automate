package middleware

import (
	"google.golang.org/grpc"
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
