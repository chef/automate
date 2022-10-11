package handler

import (
	"context"
	"github.com/chef/automate/api/external/sso"
	deployment "github.com/chef/automate/api/interservice/deployment"
	license_control "github.com/chef/automate/api/interservice/license_control"
	"github.com/golang/protobuf/ptypes/empty"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// SsoConfig - the ssoconfig service data structure
type SsoConfig struct {
	license_client license_control.LicenseControlServiceClient
	client         deployment.DeploymentClient
}

// NewSsoConfigHandler - create a new ssoconfig service handler
func NewSsoConfigHandler(license_client license_control.LicenseControlServiceClient, client deployment.DeploymentClient) *SsoConfig {
	return &SsoConfig{
		license_client: license_client,
		client:         client,
	}
}

func (a *SsoConfig) GetSsoConfig(ctx context.Context, in *empty.Empty) (*sso.GetSsoConfigResponse, error) {
	err := a.validateDeploymentType(ctx)
	if err != nil {
		return nil, err
	}

	res, err := a.getConfigData(ctx)
	if err !=nil {
		return nil, err
	}

	if res.Config.Dex != nil {
		ssoConfig := res.Config.Dex.V1.Sys.Connectors.Saml

		return &sso.GetSsoConfigResponse{
			CaContents:         ssoConfig.CaContents.GetValue(),
			SsoUrl:             ssoConfig.SsoUrl.GetValue(),
			EmailAttr:          ssoConfig.EmailAttr.GetValue(),
			UsernameAttr:       ssoConfig.UsernameAttr.GetValue(),
			GroupsAttr:         ssoConfig.GroupsAttr.GetValue(),
			AllowedGroups:      ssoConfig.AllowedGroups,
			EntityIssuer:       ssoConfig.EntityIssuer.GetValue(),
			NameIdPolicyFormat: ssoConfig.NameIdPolicyFormat.GetValue(),
		}, nil
	}

	return &sso.GetSsoConfigResponse{
		CaContents:         "",
		SsoUrl:             "",
		EmailAttr:          "",
		UsernameAttr:       "",
		GroupsAttr:         "",
		AllowedGroups:      []string{},
		EntityIssuer:       "",
		NameIdPolicyFormat: "",
	}, nil
}

func (a *SsoConfig) DeleteSsoConfig(ctx context.Context, in *empty.Empty) (*sso.DeleteSsoConfigResponse, error) {
	err := a.validateDeploymentType(ctx)
	if err != nil {
		return nil, err
	}

	log.Printf("Received request to delete sso config")
	res, err := a.getConfigData(ctx)

	if err !=nil {
		return nil, err
	}
	
	if res.Config.Dex != nil {
		return &sso.DeleteSsoConfigResponse{
			Message: "SSO Configuration disabled successfully",
		}, nil
	}

	return &sso.DeleteSsoConfigResponse{
		Message: "SSO Configuration not disabled successfully",
	}, nil
}

func(a *SsoConfig) validateDeploymentType(ctx context.Context) error {
	deploymentType, err := a.getDeploymentDetails(ctx)
	if err != nil {
		return err
	}

	if deploymentType != "SAAS" {
		msg := "Unauthorized: Deployment type is not SAAS"
		return status.Error(codes.PermissionDenied, msg)
	}
	return nil
}

func (a *SsoConfig) getDeploymentDetails(ctx context.Context) (string, error) {
	deployIDResponse, err := a.license_client.GetDeploymentID(ctx, &license_control.GetDeploymentIDRequest{})
	if err != nil {
		if isServiceDownError(err) {
			log.WithFields(log.Fields{
				"err":  err,
				"func": "getDeploymentDetails",
			}).Error("connecting to the license client")
			return "", nil
		}
		return "", err
	}

	log.Debugf("deployIDResponse.DeploymentType: %s ", deployIDResponse.DeploymentType)

	return deployIDResponse.DeploymentType, nil
}


func (a *SsoConfig) getConfigData(ctx context.Context) (*deployment.GetAutomateConfigResponse, error) {
	req := &deployment.GetAutomateConfigRequest{}

	return a.client.GetAutomateConfig(ctx, req)
}
