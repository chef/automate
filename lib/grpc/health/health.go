package health

import (
	"google.golang.org/grpc"
	grpc_health "google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
)

// Service implements grpc_health_v1 and allows changing the (sub-)services'
// statuses in the same way google.golang.org/grpc/health does.
type Service struct {
	*grpc_health.Server
}

// NewService returns a new health.Service
func NewService() *Service {
	return &Service{grpc_health.NewServer()} // nolint: govet
}

// RegisterHealthServer adds the health.Service definition to the passed GRPC
// server's services
func RegisterHealthServer(g *grpc.Server, s *Service) {
	healthpb.RegisterHealthServer(g, s)
}
