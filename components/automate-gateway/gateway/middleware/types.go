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

// AuthContextReader is used to signal that a domain-services is interested in
// the middelware's auth context.
type AuthContextReader interface {
	AuthContextRead()
}

type DeploymentCertAuthOnly interface {
	MustUseDeploymentCertAuth()
}

// AuthMiddleware is an enum of valid auth middleware values.
type AuthMiddleware int

const (
	// Unknown is the zero value for the AuthMiddleware  enum
	Unknown AuthMiddleware = iota
	// AuthV1 is the AuthMiddleware enum value representing v1 of the middleware.
	AuthV1
	// AuthV2 is the AuthMiddleware enum value representing v2 of the middleware.
	AuthV2
)

func (middleware AuthMiddleware) String() string {
	switch middleware {
	case AuthV1:
		return "AuthV1"
	case AuthV2:
		return "AuthV2"
	default:
		return "Unknown"
	}
}

func ToAuthMiddleware(s string) AuthMiddleware {
	switch s {
	case "AuthV1":
		return AuthV1
	case "AuthV2":
		return AuthV2
	default:
		return Unknown
	}
}
