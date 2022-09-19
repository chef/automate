package handler

import (
	"context"

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

	ca_contents := new.CaContents.GetValue()
	sso_url := new.SsoUrl.GetValue()
	email_attr := new.EmailAttr.GetValue()
	username_attr := new.UsernameAttr.GetValue()
	entity_issuer := new.EntityIssuer.GetValue()
	group_attr := new.GroupsAttr.GetValue()
	allowed_group := new.AllowedGroups
	name_id_policy_format := new.NameIdPolicyFormat.GetValue()

	return &sso.GetSsoConfigResponse{
		CaContents:         ca_contents,
		SsoUrl:             sso_url,
		EmailAttr:          email_attr,
		UsernameAttr:       username_attr,
		GroupsAttr:         group_attr,
		AllowedGroups:      allowed_group,
		EntityIssuer:       entity_issuer,
		NameIdPolicyFormat: name_id_policy_format,
	}, nil
}
