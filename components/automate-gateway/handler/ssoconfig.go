package handler

import (
	"context"

	deployment "github.com/chef/automate/api/interservice/deployment"

	"github.com/chef/automate/api/external/sso"
	"github.com/golang/protobuf/ptypes/empty"
)

// SsoConfig - the ssoconfig service data structure
type SsoConfig struct {
	client deployment.DeploymentClient
}

// NewSsoConfigHandler - create a new ssoconfig service handler
func NewSsoConfigHandler(client deployment.DeploymentClient) *SsoConfig {
	return &SsoConfig{
		client: client,
	}
}

func (a *SsoConfig) GetSsoConfig(ctx context.Context, in *empty.Empty) (*sso.GetSsoConfigResponse, error) {

	req := &deployment.GetAutomateConfigRequest{}

	res, err := a.client.GetAutomateConfig(ctx, req)
	if err != nil {
		return nil, err
	}

	new := res.Config.Dex.V1.Sys.Connectors.Saml

	return &sso.GetSsoConfigResponse{
		CaContents:         new.CaContents.GetValue(),
		SsoUrl:             new.SsoUrl.GetValue(),
		EmailAttr:          new.EmailAttr.GetValue(),
		UsernameAttr:       new.UsernameAttr.GetValue(),
		GroupsAttr:         new.GroupsAttr.GetValue(),
		AllowedGroups:      new.AllowedGroups,
		EntityIssuer:       new.GroupsAttr.GetValue(),
		NameIdPolicyFormat: new.NameIdPolicyFormat.GetValue(),
	}, nil
}
