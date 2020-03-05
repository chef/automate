package server

import (
	"context"
	"net"
	"net/http"
	"net/textproto"
	"net/url"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"google.golang.org/grpc/grpclog"

	api "github.com/chef/automate/api/interservice/authn"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	"github.com/chef/automate/components/authn-service/authenticator"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	wrap "github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/version"
)

// Config holds the server's configuration options.
type Config struct {
	// These are the configs of the specified Authenticators, which will be
	// Open()ed when this config is passed to NewServer
	Authenticators           map[string]AuthenticatorConfig
	Upstream                 *url.URL
	Logger                   *zap.Logger
	Token                    tokens.TokenConfig
	ServiceCerts             *certs.ServiceCerts
	TeamsAddress             string // "ip:port"
	AuthzAddress             string
	LegacyDataCollectorToken string
}

// Server is the top level object.
type Server struct {
	// Map of authenticator IDs to authenticators.
	// Note: dex wraps this with a ResourceVersion, but that is due to storing
	// connectors in the database -- we don't do this
	TokenStorage       tokens.Storage
	authenticators     map[string]authenticator.Authenticator
	logger             *zap.Logger
	connFactory        *secureconn.Factory
	teamsClient        teams.TeamsV2Client
	authzSubjectClient authz.SubjectPurgeClient
	authzV2Client      authz_v2.AuthorizationClient
	health             *health.Service
}

// NewServer constructs a server from the provided config.
func NewServer(ctx context.Context, c Config, authzV2Client authz_v2.AuthorizationClient) (*Server, error) {
	return newServer(ctx, c, authzV2Client)
}

// Serve tells authn to start responding to GRPC and HTTP1 requests. On success, it never returns.
func (s *Server) Serve(grpcEndpoint, http1Endpoint string) error {
	list, err := net.Listen("tcp", grpcEndpoint)
	if err != nil {
		return err
	}
	server := s.NewGRPCServer(s.authzSubjectClient, s.authzV2Client)

	opts := []runtime.ServeMuxOption{
		runtime.WithIncomingHeaderMatcher(headerMatcher),
	}
	pbmux := runtime.NewServeMux(opts...)
	ctx := context.Background()
	err = api.RegisterAuthenticationHandlerFromEndpoint(
		ctx, pbmux, grpcEndpoint, s.connFactory.DialOptions("authn-service"))
	if err != nil {
		return err
	}

	// Note: we start both servers in a goroutine, and when one of them returns,
	// (with what must be an error), we (brutally) abort everything and return
	// that error.
	errc := make(chan error)
	if http1Endpoint != "" {
		go func() {
			err := s.ServeHTTP1(pbmux, http1Endpoint)
			errc <- errors.Wrap(err, "HTTP")
		}()
	} else {
		s.logger.Info("HTTP1 not configured")
	}
	go func() {
		errc <- errors.Wrap(server.Serve(list), "GRPC")
	}()

	return errors.Wrap(<-errc, "Serve")
}

func (s *Server) ServeHTTP1(pbmux *runtime.ServeMux, http1Endpoint string) error {
	httpmux := http.NewServeMux()
	httpmux.Handle("/", pbmux)
	return http.ListenAndServe(http1Endpoint, httpmux)
}

func newServer(ctx context.Context, c Config, authzV2Client authz_v2.AuthorizationClient) (*Server, error) {
	// Users shouldn't see this, but gives you a clearer error message if you
	// don't configure things correctly in testing.
	if c.ServiceCerts == nil {
		return nil, errors.New("config is missing required TLS settings")
	}

	factory := secureconn.NewFactory(*c.ServiceCerts, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	authenticators := make(map[string]authenticator.Authenticator)
	for authnID, authnCfg := range c.Authenticators {
		authn, err := authnCfg.Open(c.Upstream, c.ServiceCerts, c.Logger)
		if err != nil {
			return nil, errors.Wrapf(err, "initialize authenticator %s", authnID)
		}
		authenticators[authnID] = authn
	}

	teamsConn, err := factory.Dial("teams-service", c.TeamsAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "dial teams-service (%s)", c.TeamsAddress)
	}

	authzConn, err := factory.Dial("authz-service", c.AuthzAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "dial authz-service (%s)", c.AuthzAddress)
	}

	var ts tokens.Storage
	if c.Token != nil {
		ts, err = c.Token.Open(c.ServiceCerts, c.Logger, authzV2Client)
		if err != nil {
			return nil, errors.Wrap(err, "initialize tokens adapter")
		}
	} else {
		c.Logger.Debug("no tokens adapter defined")
	}

	// Add the legacy data collector token as a secret if it was defined in the config.
	if c.LegacyDataCollectorToken != "" {
		if _, err := ts.GetTokenIDWithValue(ctx, c.LegacyDataCollectorToken); err != nil {

			// If we couldn't find the legacy data collector token, create it.
			if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
				_, err = ts.CreateLegacyTokenWithValue(ctx, c.LegacyDataCollectorToken)
			}

			if err != nil {
				return nil, errors.Wrap(err,
					"could not populate the legacy data collector token")
			}
		}
	}

	s := &Server{
		TokenStorage:       ts,
		authzSubjectClient: authz.NewSubjectPurgeClient(authzConn),
		authzV2Client:      authzV2Client,
		authenticators:     authenticators,
		logger:             c.Logger,
		connFactory:        factory,
		teamsClient:        teams.NewTeamsV2Client(teamsConn),
		health:             health.NewService(),
	}

	// make grpc-go log through zap
	grpclog.SetLoggerV2(wrap.WrapZapGRPC(s.logger))

	return s, nil
}

func (s *Server) fetchLocalTeams(ctx context.Context, userID string) ([]string, error) {
	teamsResp, err := s.teamsClient.GetTeamsForMember(ctx, &teams.GetTeamsForMemberReq{UserId: userID})
	if err != nil {
		return nil, errors.Wrapf(err, "failed to fetch local teams for user %q", userID)
	}
	teams := make([]string, len(teamsResp.GetTeams()))
	for i, team := range teamsResp.GetTeams() {
		teams[i] = team.GetId()
	}

	return teams, nil
}

// headerMatcher extends runtime.DefaultHeaderMatcher by also injecting headers
// that are not-standard, but in use in Automate.
func headerMatcher(key string) (string, bool) {
	// default behavior
	if key, ok := runtime.DefaultHeaderMatcher(key); ok {
		return key, ok
	}
	if isAutomateHeader(key) {
		return runtime.MetadataPrefix + key, true
	}

	return "", false
}

func isAutomateHeader(hdr string) bool {
	switch textproto.CanonicalMIMEHeaderKey(hdr) {
	case "X-Data-Collector-Token", "Api-Token", "X-Client-Cert":
		return true
	}
	return false
}
