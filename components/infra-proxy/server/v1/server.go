package v1

import (
	"github.com/chef/automate/components/infra-proxy/service"
)

// Server is a V1 infra-proxy server
type Server struct {
	service *service.Service
}

// NewServer returns a V1 infra-proxy server
func NewServer(service *service.Service) *Server {
	return &Server{service: service}
}
