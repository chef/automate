package gateway

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	grpc_ctxtags "github.com/grpc-ecosystem/go-grpc-middleware/tags"
	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	auth "github.com/chef/automate/components/automate-gateway/gateway/middleware/authv2"
	"github.com/chef/automate/components/automate-gateway/pkg/authorizer"
	"github.com/chef/automate/components/automate-gateway/pkg/nullbackend"
	"github.com/chef/automate/lib/grpc/debug/debug_api"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// Server holds the state of an instance of this service
type Server struct {
	Config
	clientsFactory ClientsFactory
	connFactory    *secureconn.Factory
	serviceKeyPair *tls.Certificate
	rootCerts      *x509.CertPool
	authorizer     middleware.SwitchingAuthorizationHandler

	httpServer        *http.Server
	httpMuxConnCancel func()
	grpcServer        *grpc.Server
	nullBackendServer *grpc.Server

	errC chan error
	sigC chan os.Signal

	logger *log.Entry
}

// newGRPCServer returns a *grpc.Server instance
func (s *Server) newGRPCServer() (*grpc.Server, error) {
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

func (s *Server) Start() error {
	s.errC = make(chan error, 5)
	s.sigC = make(chan os.Signal, 2)
	signal.Notify(s.sigC, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT, syscall.SIGUSR1)

	s.setLogLevel()
	s.logger.Info("starting automate-gateway")
	s.loadConnFactory()
	s.loadServiceCerts()

	err := s.startNullBackendServer()
	if err != nil {
		return errors.Wrap(err, "starting null backend")
	}

	err = s.loadClients()
	if err != nil {
		return errors.Wrap(err, "loading backend gRPC clients")
	}

	err = s.loadAuthorizer()
	if err != nil {
		return errors.Wrap(err, "loading authorizer")
	}

	err = s.startGRPCServer()
	if err != nil {
		return errors.Wrap(err, "starting gateway gRPC server")
	}

	err = s.startHTTPServer()
	if err != nil {
		return errors.Wrap(err, "starting gateway HTTPS server")
	}

	return s.startSignalHandler()
}

func (s *Server) loadConnFactory() {
	s.logger.Debug("loading gRPC connection factory")
	s.connFactory = secureconn.NewFactory(*s.Config.ServiceCerts, secureconn.DisableDebugServer())

	return
}

func (s *Server) loadClients() error {
	var err error

	s.logger.Info("loading backend gRPC clients")

	s.clientsFactory, err = NewClientsFactory(s.Config.GrpcClients, s.connFactory)

	return err
}

func (s *Server) loadServiceCerts() {
	s.logger.Debug("loading automate-gateway service certs")

	s.serviceKeyPair = s.Config.ServiceCerts.ServiceKeyPair
	s.rootCerts = s.Config.ServiceCerts.NewCertPool()

	return
}

func (s *Server) setLogLevel() {
	l, err := log.ParseLevel(s.Config.Log.Level)
	if err != nil {
		log.Warnf("unknown log level %q, using default (info)", l)
		l = log.InfoLevel
	}
	log.SetLevel(l)

	return
}

func (s *Server) loadAuthorizer() error {
	s.logger.Info("loading authorizer")

	authzClient, err := s.clientsFactory.AuthorizationClient()
	if err != nil {
		return errors.Wrap(err, "create authz client")
	}

	s.authorizer = authorizer.NewAuthorizer(auth.AuthorizationHandler(authzClient))

	return nil
}

// startNullBackendServer starts the unimplemented backend server
func (s *Server) startNullBackendServer() error {
	var err error

	s.logger.Info("starting null backend server")

	nullBackendListener, err := net.Listen("unix", s.Config.GrpcClients.NullBackendSock)
	if err != nil {
		return errors.Wrapf(err, "listen on %s", s.Config.GrpcClients.NullBackendSock)
	}

	s.nullBackendServer = nullbackend.NewServer()

	// Start all servers in goroutines and start watching for user signals. If
	// an error is returned from a server or an exit signal is received, try to
	// gracefully stop servers and exit.
	go func() {
		err := s.nullBackendServer.Serve(nullBackendListener)
		if err != nil {
			s.errC <- errors.Wrap(err, "serve null backend")
		}
		return
	}()

	return nil
}

func (s *Server) stopNullBackendServer() {
	s.logger.Info("stopping null backend server")

	s.nullBackendServer.GracefulStop()
	_ = os.Remove(s.Config.GrpcClients.NullBackendSock)

	return
}

func (s *Server) startSignalHandler() error {
	s.logger.Info("starting signal handlers")

	for {
		select {
		case err := <-s.errC:
			switch errors.Cause(err) {
			// ErrServerClosed is returned if an HTTP server is shutdown, which
			// can only happen if it's triggered by a shutdown signal or a
			// reconfigure signal, in which case the server shutting down is
			// intended. The is only returned nothing goes wrong when shutting
			// down, therefore we'll log it and move on.
			case http.ErrServerClosed:
				s.logger.Debug("HTTP server was recently shutdown")
			default:
				s.logger.WithError(err).Error("exiting")
				err1 := s.stop()
				if err1 != nil {
					return errors.Wrap(err, err1.Error())
				}
				return err
			}
		case sig := <-s.sigC:
			switch sig {
			case syscall.SIGUSR1, syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP:
				s.logger.WithField("signal", sig).Info("handling received signal")
				return s.stop()
			default:
				s.logger.WithField("signal", sig).Warn("unable to handle received signal")
			}
		}
	}
}

func (s *Server) stop() error {
	s.logger.Info("stopping automate-gateway")

	err := s.stopHTTPServer()

	s.stopGRPCServer()
	s.stopNullBackendServer()

	return err
}

func (s *Server) startGRPCServer() error {
	var err error

	s.logger.Info("starting gRPC server")

	s.grpcServer, err = s.newGRPCServer()
	if err != nil {
		return errors.Wrap(err, "init gRPC server")
	}

	if err := s.RegisterGRPCServices(s.grpcServer); err != nil {
		return errors.Wrap(err, "registering gRPC services")
	}

	grpcURI := fmt.Sprintf("%s:%d", s.Config.Hostname, s.Config.GRPCPort)
	grpcListener, err := net.Listen("tcp", grpcURI)
	if err != nil {
		return errors.Wrapf(err, "listen on %s", grpcURI)
	}

	go func() {
		err := s.grpcServer.Serve(grpcListener)
		if err != nil {
			s.errC <- errors.Wrap(err, "serve gRPC")
		}
		return
	}()

	return nil
}

func (s *Server) stopGRPCServer() {
	s.logger.Info("stopping gRPC server")
	s.grpcServer.GracefulStop()

	return
}

func (s *Server) startHTTPServer() error {
	s.logger.Info("starting HTTPS server")
	// http 1.1 server
	mux := http.NewServeMux()

	// dial in options for rest gateway to GRPC
	// register http 1.1 protobuf gateway services
	grpcURILocal := fmt.Sprintf("127.0.0.1:%d", s.Config.GRPCPort)
	v0Mux, cancelUnversioned, err := unversionedRESTMux(grpcURILocal, s.connFactory.DialOptions("automate-gateway"))
	if err != nil {
		return errors.Wrap(err, "registering v0 REST gateway services")
	}
	mux.Handle("/", prettifier(v0Mux))

	versionedMux, cancelVersioned, err := versionedRESTMux(
		grpcURILocal,
		s.connFactory.DialOptions("automate-gateway"),
		s.Config.gwRouteFeatureFlags(),
	)
	if err != nil {
		return errors.Wrap(err, "registering versioned REST gateway services")
	}

	s.httpMuxConnCancel = func() {
		cancelUnversioned()
		cancelVersioned()
		return
	}
	mux.Handle("/apis/", http.StripPrefix("/apis", prettifier(versionedMux)))

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

	// redirect profiles/search to the v0Mux (needed b/c of above mux on `/compliance/profiles/`)
	mux.Handle("/compliance/profiles/metasearch", v0Mux)

	// redirect profiles/read to the v0Mux (needed b/c of above mux on `/compliance/profiles/`)
	mux.Handle("/compliance/profiles/read/", v0Mux)

	// custom mux route for export (ignores its request method)
	// needed b/c gateway does not support stream; corresponds to
	// https://github.com/chef/automate/blob/master/components/automate-gateway/api/compliance/reporting/reporting.proto
	// `rpc Export(Query) returns (stream ExportData) {};`
	mux.HandleFunc("/compliance/reporting/export", s.ReportExportHandler)

	// custom mux route for export of all reports for a single node
	mux.HandleFunc("/compliance/reporting/node/export", s.NodeExportHandler)

	// custom mux route for export (ignores its request method)
	// needed b/c gateway does not support stream; corresponds to
	// https://github.com/chef/automate/blob/master/api/interservice/cfgmgmt/service/cfgmgmt.proto
	// `rpc NodeExport(NodeExport) returns (stream ExportData) {};`
	mux.HandleFunc("/cfgmgmt/nodes/export", s.configMgmtNodeExportHandler)

	// rpc ReportExport(ReportExport) returns (stream ReportExportData);
	mux.HandleFunc("/cfgmgmt/reports/export", s.configMgmtReportExportHandler)

	// "GET /status" is used for monitoring
	// We made it a custom handler in order to be able to return 500 when some services are down.
	mux.HandleFunc("/status", s.DeploymentStatusHandler)

	// register open api endpoint definitions
	mux.Handle("/openapi/", http.StripPrefix("/openapi", openAPIServicesHandler()))

	// attach open api ui
	mux.Handle("/openapi/ui/", http.StripPrefix("/openapi/ui", serveOpenAPIUI(s.Config.OpenAPIUIDir)))

	// Register Prometheus metrics handler.
	mux.Handle("/metrics", promhttp.Handler())

	// start https server
	uri := fmt.Sprintf("%s:%d", s.Config.Hostname, s.Config.Port)
	s.httpServer = &http.Server{
		Addr:    uri,
		Handler: mux,
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

	tcpListener, err := net.Listen("tcp", uri)
	if err != nil {
		return errors.Wrapf(err, "listen on %s", uri)
	}

	httpListener := tls.NewListener(tcpListener, s.httpServer.TLSConfig)

	go func() {
		err := s.httpServer.Serve(httpListener)
		if err != nil {
			s.errC <- errors.Wrap(err, "serve HTTPS")
		}
		return
	}()

	return nil
}

func (s *Server) stopHTTPServer() error {
	s.logger.Info("stopping HTTPS server")
	s.httpMuxConnCancel()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	err := s.httpServer.Shutdown(ctx)
	if err != nil {
		return errors.Wrap(err, "shutting down https server")
	}

	return nil
}

// prettifier strips the ?pretty query argument, and uses it to indicate that
// grpc-gateway should use a pretty-printing marshaller (outbound) instead, by
// changing the request's "Accept" header to "application/json+pretty".
// If a specific "Accept" header is already provided (i.e. not "*/*"), this will
// not touch it. Note that we leave the query params intact, so the handlers
// will still see the ?pretty. Let's deal with removing that if we have to.
func prettifier(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer h.ServeHTTP(w, r)

		// if the "Accept" is set and NOT */* (sent by curl)
		if accept := r.Header.Get("Accept"); accept != "" && accept != "*/*" {
			return
		}

		// checking Values as map[string][]string also catches ?pretty and ?pretty=
		// r.URL.Query().Get("pretty") would not.
		if _, ok := r.URL.Query()["pretty"]; ok {
			r.Header.Set("Accept", "application/json+pretty")
		}
	})
}
