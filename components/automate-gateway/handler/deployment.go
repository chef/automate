package handler

import (
	"context"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-gateway/api/deployment"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
)

type DeploymentServer struct {
	deploymentClient api.DeploymentClient
}

func NewDeploymentServer(dc api.DeploymentClient) *DeploymentServer {
	return &DeploymentServer{
		deploymentClient: dc,
	}
}

func (s *DeploymentServer) GetVersion(ctx context.Context, _ *gp.Empty) (*deployment.Version, error) {
	response, err := s.deploymentClient.ManifestVersion(ctx, &api.ManifestVersionRequest{})
	if err != nil {
		logrus.WithError(err).Error("failed to get manifest version")
		return nil, err
	}
	return &deployment.Version{BuildTimestamp: response.BuildTimestamp}, nil
}

func (s *DeploymentServer) ServiceVersions(ctx context.Context, _ *deployment.ServiceVersionsRequest) (*deployment.ServiceVersionsResponse, error) {
	serviceVersions, err := s.deploymentClient.ServiceVersions(ctx, &api.ServiceVersionsRequest{})
	if err != nil {
		logrus.WithError(err).Error("failed to get ServiceVersions")
		return nil, err
	}

	gatewayServices := make([]*deployment.ServiceVersion, len(serviceVersions.Services))

	for index, serviceVersion := range serviceVersions.Services {
		gatewayServices[index] = &deployment.ServiceVersion{
			Name:    serviceVersion.Name,
			Origin:  serviceVersion.Origin,
			Version: serviceVersion.Version,
			Release: serviceVersion.Release,
		}
	}

	return &deployment.ServiceVersionsResponse{Services: gatewayServices}, nil
}
