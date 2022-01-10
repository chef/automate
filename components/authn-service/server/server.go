package server

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"net/textproto"
	"net/url"
	"time"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"google.golang.org/grpc/grpclog"

	api "github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/id_token"
	"github.com/chef/automate/api/interservice/teams"
	"github.com/chef/automate/components/authn-service/authenticator"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	wrap "github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/version"
)

const (
	IngestPolicyID = "ingest-access"
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
	SessionAddress           string
}

// Server is the top level object.
type Server struct {
	// Map of authenticator IDs to authenticators.
	// Note: dex wraps this with a ResourceVersion, but that is due to storing
	// connectors in the database -- we don't do this
	TokenStorage   tokens.Storage
	authenticators map[string]authenticator.Authenticator
	logger         *zap.Logger
	connFactory    *secureconn.Factory
	teamsClient    teams.TeamsServiceClient
	policiesClient authz.PoliciesServiceClient
	authzClient    authz.AuthorizationServiceClient
	health         *health.Service
}

// NewServer constructs a server from the provided config.
func NewServer(ctx context.Context, c Config) (*Server, error) {

	return newServer(ctx, c)
}

// Serve tells authn to start responding to GRPC and HTTP1 requests. On success, it never returns.
func (s *Server) Serve(grpcEndpoint, http1Endpoint string) error {
	list, err := net.Listen("tcp", grpcEndpoint)
	if err != nil {
		return err
	}
	server := s.NewGRPCServer(s.policiesClient, s.authzClient)

	opts := []runtime.ServeMuxOption{
		runtime.WithIncomingHeaderMatcher(headerMatcher),
	}
	pbmux := runtime.NewServeMux(opts...)
	ctx := context.Background()
	err = api.RegisterAuthenticationServiceHandlerFromEndpoint(
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

func newServer(ctx context.Context, c Config) (*Server, error) {

	// Users shouldn't see this, but gives you a clearer error message if you
	// don't configure things correctly in testing.
	if c.ServiceCerts == nil {
		return nil, errors.New("config is missing required TLS settings")
	}

	factory := secureconn.NewFactory(*c.ServiceCerts, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	teamsConn, err := factory.Dial("teams-service", c.TeamsAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "dial teams-service (%s)", c.TeamsAddress)
	}

	sessionConn, err := factory.Dial("session-service", c.SessionAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "dial session-service (%s)", c.SessionAddress)
	}
	sessionClient := id_token.NewValidateIdTokenServiceClient(sessionConn)

	authzConn, err := factory.Dial("authz-service", c.AuthzAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "dial authz-service (%s)", c.AuthzAddress)
	}
	authzClient := authz.NewAuthorizationServiceClient(authzConn)
	policiesClient := authz.NewPoliciesServiceClient(authzConn)

	var ts tokens.Storage
	if c.Token != nil {
		ts, err = c.Token.Open(c.ServiceCerts, c.Logger, authzClient)
		if err != nil {
			return nil, errors.Wrap(err, "initialize tokens adapter")
		}
	} else {
		c.Logger.Debug("no tokens adapter defined")
	}

	authenticators := make(map[string]authenticator.Authenticator)
	for authnID, authnCfg := range c.Authenticators {
		authn, err := authnCfg.Open(c.Upstream, c.ServiceCerts, c.Logger, sessionClient)
		if err != nil {
			return nil, errors.Wrapf(err, "initialize authenticator %s", authnID)
		}
		authenticators[authnID] = authn
	}

	s := &Server{
		TokenStorage:   ts,
		authzClient:    authzClient,
		policiesClient: policiesClient,
		authenticators: authenticators,
		logger:         c.Logger,
		connFactory:    factory,
		teamsClient:    teams.NewTeamsServiceClient(teamsConn),
		health:         health.NewService(),
	}

	// make grpc-go log through zap
	grpclog.SetLoggerV2(wrap.WrapZapGRPC(s.logger))

	// Add the legacy data collector token as a secret if it was defined in the config.
	if c.LegacyDataCollectorToken != "" {
		var tokenID string
		if existingID, err := ts.GetTokenIDWithValue(ctx, c.LegacyDataCollectorToken); err != nil {
			// If we couldn't find the legacy data collector token, create it.
			if _, ok := errors.Cause(err).(*tokens.NotFoundError); ok {
				var token *tokens.Token
				token, err = ts.CreateLegacyTokenWithValue(ctx, c.LegacyDataCollectorToken)
				tokenID = token.ID
			}

			if err != nil {
				return nil, errors.Wrap(err,
					"could not populate the legacy data collector token")
			}
		} else {
			tokenID = existingID
		}

		/*
			Below code is only execute when you add token in config at the time of deployment
			Adding the sleep, is just the tempory solution, this might work
		*/
		var policyAssigned bool = false
		for i := 4; i > 0; i-- {
			_, err = policiesClient.AddPolicyMembers(ctx, &authz.AddPolicyMembersReq{
				Id:      IngestPolicyID,
				Members: []string{fmt.Sprintf("token:%s", tokenID)},
			})
			if err != nil {
				policyAssigned = false
			} else {
				s.logger.Debug("exiting for loop")
				policyAssigned = true
				break
			}
			s.logger.Debug(fmt.Sprintf(" Adding the sleep for %d seconds ", i))
			time.Sleep(time.Duration(3) * time.Second)
		}
		if !policyAssigned {
			s.logger.Warn(errors.Wrap(err, "there was an error granting the legacy data collector token ingest access").Error())
			s.logger.Warn(fmt.Sprintf("please manually add token with ID %q to the policy with ID %q", tokenID, IngestPolicyID))
		}
	}
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
