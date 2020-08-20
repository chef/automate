package server

import (
	"net"

	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	grpc_s "github.com/chef/automate/api/interservice/cds/service"
	profiles "github.com/chef/automate/api/interservice/compliance/profiles"
	"github.com/chef/automate/components/automate-cds/config"
	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/tracing"
	log "github.com/sirupsen/logrus"
)

// NewGRPCServer creates a grpc server that serves all Teams GRPC APIs
func NewGRPCServer(s *service.Service,
	profilesServiceClient profiles.ProfilesServiceClient) *grpc.Server {
	grpcServer := s.ConnFactory.NewServer(tracing.GlobalServerInterceptor())

	health.RegisterHealthServer(grpcServer, health.NewService())
	grpc_s.RegisterAutomateCdsServiceServer(grpcServer, NewServer(s, profilesServiceClient))
	reflection.Register(grpcServer)

	return grpcServer
}

// GRPC creates and listens on grpc server
func GRPC(config config.AutomateCdsConfig, s *service.Service) error {
	list, err := net.Listen("tcp", config.GRPC)
	if err != nil {
		return err
	}

	// compliance-service Interface
	complianceConn, err := s.ConnFactory.Dial("compliance-service", config.ComplianceService.Address)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create compliance-service connection")
		return err
	}
	defer complianceConn.Close() // nolint: errcheck

	profilesServiceClient := profiles.NewProfilesServiceClient(complianceConn)

	serv := NewGRPCServer(s, profilesServiceClient).Serve(list)
	s.Logger.Debugf("Automate CDS GRPC API listening on %v", config.GRPC)
	return serv
}
