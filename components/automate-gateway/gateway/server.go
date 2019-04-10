package gateway

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"net"
	"net/http"
	"net/url"
	"strings"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	grpc_ctxtags "github.com/grpc-ecosystem/go-grpc-middleware/tags"
	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware/authv1"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware/authv2"
	"github.com/chef/automate/components/automate-gateway/pkg/authorizer"
	"github.com/chef/automate/lib/grpc/debug/debug_api"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
)

type Config struct {
	ExternalFqdn string       `mapstructure:"external_fqdn" toml:"external_fqdn"`
	GrpcClients  ClientConfig `mapstructure:"grpc_clients" toml:"grpc_clients"`
	GRPCPort     int          `mapstructure:"grpc_port" toml:"grpc_port"`
	Hostname     string       `mapstructure:"host" toml:"host"`
	Log          struct {
		Level string `mapstructure:"level" toml:"level"`
	} `mapstructure:"log" toml:"log"`
	OpenAPIUIDir      string `mapstructure:"open_api_ui_dir" toml:"open_api_ui_dir"`
	Port              int    `mapstructure:"port" toml:"port"`
	ServiceCerts      *certs.ServiceCerts
	TLSConfig         certs.TLSConfig `mapstructure:"tls" toml:"tls"`
	TrialLicenseURL   string          `mapstructure:"trial_license_url" toml:"trial_license_url"`
	EnableAppsFeature bool            `mapstructure:"enable_apps_feature" toml:"enable_apps_feature"`
}

type gwRouteFeatureFlags map[string]bool

// Server holds the state of an instance of this service
type Server struct {
	grpcListenHost string
	grpcListenPort int
	httpListenHost string
	httpListenPort int

	clientsFactory  ClientsFactory
	connFactory     *secureconn.Factory
	serviceKeyPair  *tls.Certificate
	rootCerts       *x509.CertPool
	automateURL     *url.URL
	openapiUIDir    string
	trialLicenseURL *url.URL
	authorizer      middleware.SwitchingAuthorizationHandler

	gwRouteFeatureFlags gwRouteFeatureFlags
}

func (c *Config) gwRouteFeatureFlags() gwRouteFeatureFlags {
	g := make(gwRouteFeatureFlags)
	g["applications"] = c.EnableAppsFeature
	return g
}

// ConfigFromViper returns a Gateway config from the services configuration
// file and the viper CLI arguments.
func ConfigFromViper() (*Config, error) {
	config := &Config{}

	// Unmarshall the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to marshall config options to server config")
		return config, err
	}

	// Fix any relative paths that might be in the config file
	config.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := config.TLSConfig.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Failed to loading x509 key pair and/or root CA certificate")
		return config, err
	}
	config.ServiceCerts = serviceCerts

	// When the gRPC server initializes it'll attempt to create gRPC clients to
	// to all possible upstreams, even if the targets don't exist. This allows
	// the gateway to run without any hard binds. Here we take our configured
	// gRPC client endpoints and add any missing defaults that we might need
	// in order to initialize those clients.
	config.GrpcClients.configureDefaultEndpoints()

	return config, nil
}

// NewFromConfig initializes a Server given a Config
func NewFromConfig(cfg *Config) (*Server, error) {
	connFact := secureconn.NewFactory(*cfg.ServiceCerts, secureconn.DisableDebugServer())
	externalURL, err := url.Parse(cfg.ExternalFqdn)
	if err != nil {
		return nil, errors.Wrapf(err, "parse external FQDN from config: %q", cfg.ExternalFqdn)
	}

	clientsFactory := NewClientsFactory(cfg.GrpcClients, connFact)
	authzClientV1, err := clientsFactory.AuthorizationClient()
	if err != nil {
		return nil, errors.Wrap(err, "create authz client")
	}
	authzClientV2, err := clientsFactory.AuthorizationV2Client()
	if err != nil {
		return nil, errors.Wrap(err, "create authz_v2 client")
	}

	opts := []Opts{
		WithConnectionsFactory(connFact),
		WithClientsFactory(clientsFactory),
		WithAuthorizer(authorizer.NewAuthorizer(authv1.AuthorizationHandler(authzClientV1),
			authv2.AuthorizationHandler(authzClientV2))),
		WithURI(cfg.Hostname, cfg.Port),
		WithGRPCURI(cfg.Hostname, cfg.GRPCPort),
		WithServiceKeyPair(cfg.ServiceCerts.ServiceKeyPair, cfg.ServiceCerts.NewCertPool()),
		WithAutomateURL(externalURL),
		WithOpenAPIUIDir(cfg.OpenAPIUIDir),
		WithLogLevel(cfg.Log.Level),
		WithRouteFeatureToggles(cfg),
	}
	if tlsURL := cfg.TrialLicenseURL; tlsURL != "" {
		trialLicenseURL, err := url.Parse(tlsURL)
		if err != nil {
			return nil, errors.Wrapf(err, "parse trial license URL from config: %q", cfg.TrialLicenseURL)
		}
		opts = append(opts, WithTrialLicenseURL(trialLicenseURL))
	}
	return New(opts...), nil
}

// New initializes a *Server from the passed options
func New(opts ...Opts) *Server {
	s := &Server{
		openapiUIDir: "./third_party/swagger-ui/",
	}
	for _, opt := range opts {
		opt(s)
	}
	return s
}

// Opts is for supporting functional options like WithClientsFactory,
// WithConnectionFactory, ..., passed to New()
type Opts func(*Server)

// WithClientsFactory allows setting the ClientsFactory to use
func WithClientsFactory(fy ClientsFactory) Opts {
	return func(s *Server) {
		s.clientsFactory = fy
	}
}

// WithAuthorizer allows setting the Authorizer to use
func WithAuthorizer(a middleware.SwitchingAuthorizationHandler) Opts {
	return func(s *Server) {
		s.authorizer = a
	}
}

// WithConnectionsFactory allows setting the ConnectionsFactory to use
func WithConnectionsFactory(fy *secureconn.Factory) Opts {
	return func(s *Server) {
		s.connFactory = fy
	}
}

// WithURI allows setting the URI to use from hostname and port
func WithURI(hostname string, port int) Opts {
	return func(s *Server) {
		s.httpListenHost = hostname
		s.httpListenPort = port
	}
}

// WithGRPCURI allows setting the internal GRPC URI to use from hostname/port
func WithGRPCURI(hostname string, port int) Opts {
	return func(s *Server) {
		s.grpcListenHost = hostname
		s.grpcListenPort = port
	}
}

// WithAutomateURL allows setting the URL used externally
func WithAutomateURL(u *url.URL) Opts {
	return func(s *Server) {
		s.automateURL = u
	}
}

// WithTrialLicenseURL allows setting the URL used for trial-license-service
func WithTrialLicenseURL(u *url.URL) Opts {
	return func(s *Server) {
		s.trialLicenseURL = u
	}
}

// WithServiceKeyPair allows setting the ServiceKeyPair to use
func WithServiceKeyPair(cert *tls.Certificate, root *x509.CertPool) Opts {
	return func(s *Server) {
		s.serviceKeyPair = cert
		s.rootCerts = root
	}
}

// WithOpenAPIUIDir sets the swagger ui directory
func WithOpenAPIUIDir(dir string) Opts {
	return func(s *Server) {
		s.openapiUIDir = dir
	}
}

// WithLogLevel sets the log level to use.
//
// Note that we keep the "functional options" style here, although we end up
// changing the global logger's settings, not the Server. This could become
// confusing, but it's also paving a way for having the Server control _its own
// logger_, instead of using the global one.
// The potential confusing scenario is unlikely: spinning up multiple Server
// instances with different log levels.
func WithLogLevel(lvl string) Opts {
	return func(*Server) {
		l, err := log.ParseLevel(lvl)
		if err != nil {
			log.Warnf("unknown log level %q, using default (info)", lvl)
			l = log.InfoLevel
		}
		log.SetLevel(l)
	}
}

func WithRouteFeatureToggles(c *Config) Opts {
	return func(s *Server) {
		s.gwRouteFeatureFlags = c.gwRouteFeatureFlags()
	}
}

// handle GRPC returns
func (s *Server) grpcHandlerFunc(grpcServer *grpc.Server, muxHandler http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// partial check of https://github.com/grpc/grpc-go/blob/master/transport/handler_server.go#L50
		if r.ProtoMajor >= 2 && strings.Contains(r.Header.Get("Content-Type"), "application/grpc") {
			// TODO: proxy to s.grpcURI instead
			grpcServer.ServeHTTP(w, r)
		} else {
			muxHandler.ServeHTTP(w, r)
		}
	})
}

// NewGRPCServer returns a *grpc.Server instance
func (s *Server) NewGRPCServer() (*grpc.Server, error) {
	authClient, err := s.clientsFactory.AuthenticationClient()
	if err != nil {
		return nil, errors.Wrap(err, "create auth client")
	}

	authInterceptor := middleware.NewAuthInterceptor(authClient, s.authorizer)

	logrusEntry := log.NewEntry(log.StandardLogger())

	// This function determines the log level based on the returned status code:
	// We divert from the default only by pushing non-error-returns into debug
	// logs. See DefaultCodeToLevel for the rest of the mapping.
	levelFunc := func(c codes.Code) log.Level {
		switch c {
		case codes.OK:
			return log.DebugLevel
		default:
			return grpc_logrus.DefaultCodeToLevel(c)
		}
	}

	logrusOpts := []grpc_logrus.Option{
		// don't log the gateway's health check (it's currently satisfied if the
		// response is code.Unauthenticated)
		grpc_logrus.WithDecider(func(m string, err error) bool {
			return m != "/chef.automate.api.Gateway/GetHealth"
		}),

		grpc_logrus.WithLevels(levelFunc),
	}

	// log grpc standard log stuff through logrus
	grpc_logrus.ReplaceGrpcLogger(logrusEntry)

	// Enable prometheus timing data gathering
	//
	// Note (2019/01/15) sr: these come with a cost, but given that we already have
	// the metrics endpoint here, and these numbers are exactly what we need, I'd
	// propose we try rolling with this for a while.
	// See https://github.com/grpc-ecosystem/go-grpc-prometheus/tree/68e3a13e411#histograms
	// for rationale on why this is disabled by default.
	grpc_prometheus.EnableHandlingTimeHistogram()

	// Note re: Ordering
	// Any middleware trying add to the logging fields needs to come AFTER
	// grpc_logrus. Also, grpc_logrus itself has to come after grpc_ctxtags, as it
	// depends on that.

	opts := []grpc.ServerOption{
		grpc.StreamInterceptor(grpc_middleware.ChainStreamServer(
			grpc_ctxtags.StreamServerInterceptor(),
			grpc_logrus.StreamServerInterceptor(logrusEntry, logrusOpts...),
			authInterceptor.StreamServerInterceptor(),
			grpc_prometheus.StreamServerInterceptor,
		)),
		grpc.UnaryInterceptor(grpc_middleware.ChainUnaryServer(
			grpc_ctxtags.UnaryServerInterceptor(),
			grpc_logrus.UnaryServerInterceptor(logrusEntry, logrusOpts...),
			tracing.ServerInterceptor(tracing.GlobalTracer()),
			authInterceptor.UnaryServerInterceptor(),
			grpc_prometheus.UnaryServerInterceptor,
		)),
	}
	grpcServer := s.connFactory.NewServer(opts...)

	debug_api.RegisterDebugServer(grpcServer, NewDebugServer())

	return grpcServer, nil
}

// Serve finalizes the Server setup, and listens for connections. Only
// returns if something went wrong, with a non-nil error.
func (s *Server) Serve() error {
	log.WithFields(log.Fields{
		"http-host": s.httpListenHost,
		"http-port": s.httpListenPort,
	}).Info("Starting server")

	// http 2.0 grpc server
	grpcServer, err := s.NewGRPCServer()
	if err != nil {
		return errors.Wrap(err, "init GRPC server")
	}
	if err := s.RegisterGRPCServices(grpcServer); err != nil {
		return errors.Wrap(err, "registering GRPC services")
	}

	// After all your registrations, make sure all of the Prometheus metrics are initialized.
	grpc_prometheus.Register(grpcServer)

	// http 1.1 server
	mux := http.NewServeMux()

	// dial in options for rest gateway to GRPC
	// register http 1.1 protobuf gateway services
	grpcURILocal := fmt.Sprintf("127.0.0.1:%d", s.grpcListenPort)
	v0Mux, err := unversionedRESTMux(grpcURILocal, s.connFactory.DialOptions("automate-gateway"))
	if err != nil {
		return errors.Wrap(err, "registering v0 REST gateway services")
	}
	mux.Handle("/", v0Mux)

	versionedMux, err := versionedRESTMux(grpcURILocal, s.connFactory.DialOptions("automate-gateway"), s.gwRouteFeatureFlags)
	if err != nil {
		return errors.Wrap(err, "registering versioned REST gateway services")
	}
	mux.Handle("/apis/", http.StripPrefix("/apis", versionedMux))

	// custom mux route for data-collector
	// Note: automate-load-balancer rewrites
	//   /data-collector/v0(.*) => /events/data-collector
	// and drops everything that is matched -- there will be no trailing
	// slashes, and no further paths
	dataCollectorForPOST := func(w http.ResponseWriter, r *http.Request) {
		if r.Method == http.MethodPost {
			s.dataCollectorHandler(w, r)
			return
		}
		v0Mux.ServeHTTP(w, r)
	}
	mux.HandleFunc("/events/data-collector", dataCollectorForPOST)

	// "GET /events/data-collector/" is used by erchef's /_status endpoint
	// Note: erchef's data_collector:ping/0 uses data_collector_http:get("/"),
	// which ends up requesting /events/data-collector/ (trailing slash) AND
	// queries automate-gateway directly, so nginx doesn't help us.
	mux.HandleFunc("/events/data-collector/", func(w http.ResponseWriter, r *http.Request) {
		// grpc-gateway-generated handler code doesn't match this with a trailing /
		r.URL.Path = "/events/data-collector"
		dataCollectorForPOST(w, r)
	})

	// register custom route for profile upload; corresponds to:
	// https://github.com/chef/automate/blob/master/components/automate-gateway/api/compliance/profiles/profiles.proto
	// `rpc Create (stream ProfilePostRequest) returns (CheckResult) {};`
	mux.HandleFunc("/compliance/profiles", s.ProfileCreateHandler)

	profileTarHandlerUnlessDELETE := func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodDelete {
			s.ProfileTarHandler(w, r)
			return
		}
		v0Mux.ServeHTTP(w, r)
	}

	// custom mux route for profile tar download; corresponds to:
	// https://github.com/chef/automate/blob/master/components/automate-gateway/api/compliance/profiles/profiles.proto
	// `rpc ReadTar(ProfileDetails) returns (stream ProfileData) {};`
	mux.HandleFunc("/compliance/profiles/tar", profileTarHandlerUnlessDELETE)

	// for legacy endpoints:
	// compliance/profiles/{owner}/{name}/tar,
	// compliance/profiles/{owner}/{name}/version/{version}/tar
	// these are used by the audit cookbook/inspec/chef-server
	mux.HandleFunc("/compliance/profiles/", profileTarHandlerUnlessDELETE) // legacy route

	// redirect profiles/search to the v0Mux (needed b/c of above mux on `/compliance/profiles/`)
	mux.Handle("/compliance/profiles/search", v0Mux)

	// redirect profiles/read to the v0Mux (needed b/c of above mux on `/compliance/profiles/`)
	mux.Handle("/compliance/profiles/read/", v0Mux)

	// custom mux route for export (ignores its request method)
	// needed b/c gateway does not support stream; corresponds to
	// https://github.com/chef/automate/blob/master/components/automate-gateway/api/compliance/reporting/reporting.proto
	// `rpc Export(Query) returns (stream ExportData) {};`
	mux.HandleFunc("/compliance/reporting/export", s.ReportExportHandler)

	// custom mux route for export (ignores its request method)
	// needed b/c gateway does not support stream; corresponds to
	// https://github.com/chef/automate/blob/master/api/interservice/cfgmgmt/service/cfgmgmt.proto
	// `rpc NodeExport(NodeExport) returns (stream ExportData) {};`
	mux.HandleFunc("/cfgmgmt/nodes/export", s.configMgmtNodeExportHandler)

	// register open api endpoint definitions
	mux.Handle("/openapi/", http.StripPrefix("/openapi", openAPIServicesHandler()))

	// attach open api ui
	mux.Handle("/openapi/ui/", http.StripPrefix("/openapi/ui", serveOpenAPIUI(s.openapiUIDir)))

	// Register Prometheus metrics handler.
	mux.Handle("/metrics", promhttp.Handler())

	handler := s.grpcHandlerFunc(grpcServer, mux)

	// start server
	uri := fmt.Sprintf("%s:%d", s.httpListenHost, s.httpListenPort)
	srv := &http.Server{
		Addr:    uri,
		Handler: handler,
		TLSConfig: &tls.Config{
			Certificates: []tls.Certificate{*s.serviceKeyPair},
			NextProtos:   []string{"h2"},
			ClientAuth:   tls.VerifyClientCertIfGiven,
			ClientCAs:    s.rootCerts,
			MinVersion:   tls.VersionTLS12,
			// We're the boss.
			PreferServerCipherSuites: true,
			CipherSuites:             secureconn.DefaultCipherSuites(),
		},
	}

	conn, err := net.Listen("tcp", uri)
	if err != nil {
		return errors.Wrapf(err, "listen on %s", uri)
	}
	log.WithFields(log.Fields{
		"uri": uri,
	}).Info("Serve gRPC/REST")

	grpcURI := fmt.Sprintf("%s:%d", s.grpcListenHost, s.grpcListenPort)
	grpcConn, err := net.Listen("tcp", grpcURI)
	if err != nil {
		return errors.Wrapf(err, "listen on %s", grpcURI)
	}
	log.WithFields(log.Fields{
		"grpc_uri": grpcURI,
	}).Info("Serve gRPC")

	// Note: we start both servers in a goroutine, and when one of them returns,
	// (with what must be an error), we (brutally) abort everything and return
	// that error.
	errc := make(chan error)
	go func() {
		err := srv.Serve(tls.NewListener(conn, srv.TLSConfig))
		errc <- errors.Wrap(err, "HTTP")
	}()
	go func() {
		errc <- errors.Wrap(grpcServer.Serve(grpcConn), "GRPC")
	}()

	return errors.Wrap(<-errc, "Serve")
}
