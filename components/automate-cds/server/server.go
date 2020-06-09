package server

import (
	"net"

	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	grpc_s "github.com/chef/automate/api/interservice/cds/service"
	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPCServer creates a grpc server that serves all Teams GRPC APIs
func NewGRPCServer(s *service.Service) *grpc.Server {
	grpcServer := s.ConnFactory.NewServer(tracing.GlobalServerInterceptor())

	health.RegisterHealthServer(grpcServer, health.NewService())
	grpc_s.RegisterAutomateCdsServer(grpcServer, NewServer(s))
	reflection.Register(grpcServer)

	return grpcServer
}

// GRPC creates and listens on grpc server
func GRPC(addr string, s *service.Service) error {
	list, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	serv := NewGRPCServer(s).Serve(list)
	s.Logger.Debugf("Automate CDS GRPC API listening on %v", addr)
	return serv
}
