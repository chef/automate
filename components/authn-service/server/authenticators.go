package server

import (
	"context"
	"net/http"
	"net/url"
	"strings"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/components/authn-service/authenticator/mock"
	"github.com/chef/automate/components/authn-service/authenticator/oidc"
	"github.com/chef/automate/components/authn-service/authenticator/tokens"
	"github.com/chef/automate/lib/tls/certs"
)

// AuthenticatorConfig is a configuration that can open an authenticator.
type AuthenticatorConfig interface {
	Open(*url.URL, *certs.ServiceCerts, *zap.Logger) (authenticator.Authenticator, error)
}

// AuthenticatorsConfig variable provides an easy way to return a config struct
// depending on the authenticator type.
var AuthenticatorsConfig = map[string]func() AuthenticatorConfig{
	"mock-oidc":         func() AuthenticatorConfig { return new(mock.OidcConfig) },
	"mock-static":       func() AuthenticatorConfig { return new(mock.StaticConfig) },
	"mock-header-token": func() AuthenticatorConfig { return new(mock.HeaderTokenConfig) },
	"oidc":              func() AuthenticatorConfig { return new(oidc.Config) },
	"header-token":      func() AuthenticatorConfig { return new(tokens.HeaderTokenConfig) },
}

// Authenticate provides a quick and dirty api.AuthenticationServer
// implementation for *Server
func (s *Server) Authenticate(ctx context.Context, _ *api.AuthenticateRequest) (*api.AuthenticateResponse, error) {
	md, ok := metadata.FromIncomingContext(ctx)
	if !ok {
		return nil, status.Errorf(codes.Unauthenticated, "no metadata")
	}

	// TODO 2017/10/10 (sr): Refactor -- we're making up a request so we can use
	// the authenticators' common interface: Authenticate(*http.Request).
	// Either generalize that, or ditch the HTTP-based authentication all
	// together.
	req, err := reqFromMD(md)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to construct request: %v", err.Error())
	}
	requestor, err := s.authenticate(req)
	if err != nil {
		return nil, status.Error(codes.Unauthenticated, err.Error())
	}

	if user, ok := requestor.(authenticator.LocalUser); ok {
		teams, err := s.fetchLocalTeams(ctx, user.UserID())
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}
		user.AppendTeams(teams)
	}

	return &api.AuthenticateResponse{Subject: requestor.Subject(), Teams: requestor.Teams()}, nil
}

func reqFromMD(md metadata.MD) (*http.Request, error) {
	req, err := http.NewRequest("GET", "", nil)
	if err != nil {
		return nil, errors.Wrap(err, "create request")
	}
	for k, v := range md {
		req.Header.Set(stripHeaderPrefix(k), v[0])
	}
	return req, nil
}

// stripHeaderPrefix removes the prefix added by grpc-gateway.
// So, an incoming HTTP request with header x-data-collector-token or api-token will be added to
// GRPC metadata as grpc-gateway-x-data-collector-token or grpc-gateway-api-token. This function changes
// the name back to x-data-collector-token or api-token.
func stripHeaderPrefix(hdr string) string {
	if strings.HasPrefix(hdr, runtime.MetadataPrefix) {
		return hdr[len(runtime.MetadataPrefix):]
	}
	return hdr
}
