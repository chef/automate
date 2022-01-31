package server

import (
	"context"

	"github.com/pkg/errors"

	secrets "github.com/chef/automate/api/external/secrets"
)

// getChefClient: creates the chef client
func (s *Server) getChefClient(ctx context.Context, serverId string) (*ChefClient, error) {
	// Get the credential ID from servers table
	server, err := s.service.Storage.GetServer(ctx, serverId)
	if err != nil {
		return nil, err
	}
	if server.CredentialID == "" {
		return nil, errors.New("webui key is not available with server")
	}
	// Get web ui key from secrets service
	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: server.CredentialID})
	if err != nil {
		return nil, err
	}

	c, err := s.createChefServerClient(ctx, serverId, GetAdminKeyFrom(secret), "pivotal", true)
	if err != nil {
		return nil, err
	}
	return c, nil
}
