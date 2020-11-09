package server

import (
	"context"
	"net"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/sample-data-service/service"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
)

// Server is a sample-data server
type Server struct {
	service *service.Service
}

// NewServer returns a sample-data server
func NewServer(service *service.Service) *Server {
	return &Server{service: service}
}

// NewGRPCServer creates a grpc server that serves all sample-data GRPC APIs
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
	s.Logger.Debugf("Sample data GRPC API listening on %v", addr)
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
