package server

import (
	"context"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
)

// RegenerateRoot generates a new root certificate for the certificate authority
func (s *server) RegenerateRoot(context.Context, *api.RegenerateRootRequest) (*api.RegenerateRootResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	s.deployment.Lock()
	err := s.deployment.CA().RegenerateRoot()
	if err != nil {
		s.deployment.Unlock()
		return nil, status.Errorf(codes.Internal, "failed to regenerate CA: %s", err.Error())
	}
	s.deployment.Unlock()

	operation := func(s *server) error {
		err = s.deployment.EnsureCerts()
		if err != nil {
			return errors.Wrap(err, "failed to ensure all services have TLS certificates")
		}
		return s.persistDeployment()
	}

	sender := s.newEventSender()
	errHandler := deployErrorHandler(sender)
	sink := newEventAdapter(sender)
	_, err = s.doConverge(operation, sender, sink, errHandler)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to converge deployment: %s", err.Error())
	}

	return &api.RegenerateRootResponse{}, nil
}

// GetRootCert returns the current root certificate for the certificate authority
func (s *server) GetRootCert(context.Context, *api.RootCertRequest) (*api.RootCertResponse, error) {
	cert := s.deployment.CA().RootCert()
	return &api.RootCertResponse{Cert: cert}, nil
}
