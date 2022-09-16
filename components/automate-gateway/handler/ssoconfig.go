package handler

import (
	"context"
	"fmt"

	deployment "github.com/chef/automate/api/interservice/deployment"

	"github.com/chef/automate/api/external/sso"
)

// SsoConfig - the ssoconfig service data structure
type Server struct {
	client deployment.DeploymentClient
}

// NewSsoConfigHandler - create a new ssoconfig service handler
func NewServer(client deployment.DeploymentClient) *Server {
	return &Server{
		client: client,
	}
}

func (a *Server) GetSsoConfig(ctx context.Context, _ *sso.GetSsoConfigRequest) (*sso.GetSsoConfigResponse, error) {
	req := &deployment.GetAutomateConfigRequest{}

	res, err := a.client.GetAutomateConfig(ctx, req)
	if err != nil {
		return nil, err
	}

	new := res.Config.Dex.V1.Sys.Connectors.Saml
	fmt.Println(new)
	return &sso.GetSsoConfigResponse{}, nil
}
