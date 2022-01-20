package server

import (
	"context"
	"crypto/tls"
	"fmt"
	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"
	"net"
	"net/http"

	grpc_s1 "github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
	grpc_s "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"

	log "github.com/sirupsen/logrus"
)

//StatusChecker should have all the functions required to check the status of Chef Infra Server
type StatusChecker interface {
	GetInfraServerStatus(string) (*http.Response, error)
}

//InfraServerStatusChecker implements StatusChecker
type InfraServerStatusChecker struct {
}

//GetInfraServerStatus gets the status of the Chef Infra Server
// from https://<server>/_status route
func (s *InfraServerStatusChecker) GetInfraServerStatus(serverHost string) (*http.Response, error) {

	// make http request to get the status
	transCfg := &http.Transport{
		TLSClientConfig: &tls.Config{
			InsecureSkipVerify: true,
			MinVersion:         tls.VersionTLS12,
		}, // ignore expired SSL certificates
	}
	client := &http.Client{Transport: transCfg}

	res, err := client.Get("https://" + serverHost + "/_status")

	if err != nil {
		log.Warnf("Failed to connect to host %s: %s", serverHost, err.Error())
		return nil, errors.Wrap(err, "Not able to connect to the server")
	}

	if res.StatusCode != 200 {
		return nil, errors.Wrap(fmt.Errorf("response status is not 200"), "Not able to connect to the server")
	}

	return res, err
}

// Server is an infra-proxy server
type Server struct {
	service                  *service.Service
	infraServerStatusChecker StatusChecker
}

// NewServer returns an infra-proxy server
func NewServer(service *service.Service) *Server {
	var st StatusChecker = &InfraServerStatusChecker{}
	return &Server{
		service:                  service,
		infraServerStatusChecker: st,
	}
}

// NewGRPCServer creates a grpc server that serves all infra-proxy-service GRPC APIs
func NewGRPCServer(s *service.Service) *grpc.Server {
	g := s.ConnFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				InputValidationInterceptor(),
			),
		),
	)
	health.RegisterHealthServer(g, health.NewService())
	grpc_s.RegisterInfraProxyServiceServer(g, NewServer(s))
	grpc_s1.RegisterMigrationDataServiceServer(g, NewServer(s))
	reflection.Register(g)
	return g
}

// GRPC creates and listens on grpc server
func GRPC(addr string, s *service.Service) error {
	list, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	serv := NewGRPCServer(s).Serve(list)
	s.Logger.Debugf("Infra proxy GRPC API listening on %v", addr)
	return serv
}

// InputValidationInterceptor is a middleware for running the protobuf validation.
func InputValidationInterceptor() grpc.UnaryServerInterceptor {
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
