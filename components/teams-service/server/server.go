package server

import (
	"context"
	"net"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/grpclog"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	grpc_v1 "github.com/chef/automate/api/interservice/teams/v1"
	grpc_v2 "github.com/chef/automate/api/interservice/teams/v2"
	v1 "github.com/chef/automate/components/teams-service/server/v1"
	v2 "github.com/chef/automate/components/teams-service/server/v2"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPCServer creates a grpc server that serves all Teams GRPC APIs
func NewGRPCServer(s *service.Service) *grpc.Server {
	grpclog.SetLoggerV2(s.Logger)
	g := s.ConnFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				InputValidationInterceptor(),
			),
		),
	)
	health.RegisterHealthServer(g, health.NewService())
	grpc_v1.RegisterTeamsV1Server(g, v1.NewServer(s))
	grpc_v2.RegisterTeamsV2Server(g, v2.NewServer(s))
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
	s.Logger.Debugf("Teams GRPC API listening on %v", addr)
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
