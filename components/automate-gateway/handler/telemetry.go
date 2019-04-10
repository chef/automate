//
//  Author:: Joshua Padgett <jpadgett@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package handler

import (
	"context"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	log "github.com/sirupsen/logrus"

	deployment_service "github.com/chef/automate/api/interservice/deployment"
	license_control "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/automate-gateway/api/telemetry"

	"github.com/gofrs/uuid"
)

type TelemetryServer struct {
	client        license_control.LicenseControlClient
	deploy_client deployment_service.DeploymentClient
}

func NewTelemetryServer(client license_control.LicenseControlClient, dclient deployment_service.DeploymentClient) *TelemetryServer {
	return &TelemetryServer{client: client, deploy_client: dclient}
}

// GetTelemetryConfiguration returns the license and telemetry information
func (t *TelemetryServer) GetTelemetryConfiguration(ctx context.Context, request *telemetry.TelemetryRequest) (*telemetry.TelemetryResponse, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	tel, err := t.client.Telemetry(ctx, &license_control.TelemetryRequest{})
	if err != nil {
		return nil, err
	}

	r, err := t.client.License(ctx, &license_control.LicenseRequest{})
	if err != nil {
		return nil, err
	}

	deploymentID, err := t.getDeploymentID(ctx)
	if err != nil {
		return nil, err
	}

	deploymentUUID := ToUUID(deploymentID)

	//if deployment id is empty or a UUID don't turn it into a UUID

	if r.License != nil {
		//Licenses can have more than one entitlement
		//We can add them together to see what the maximum entitlement is
		var maxNodes int64
		now := time.Now() // We only want currently active entitlements
		for _, entitlement := range r.License.Entitlements {
			if entitlement.Measure == "nodes" &&
				now.Before(time.Unix(entitlement.End.GetSeconds(), 0)) && // no nanos for you!
				now.After(time.Unix(entitlement.Start.GetSeconds(), 0)) {
				maxNodes += entitlement.Limit
			}
		}

		return &telemetry.TelemetryResponse{
			LicenseId:        r.License.Id,
			CustomerName:     r.License.Customer,
			CustomerId:       r.License.CustomerId,
			LicenseType:      r.License.Type,
			TelemetryEnabled: tel.TelemetryEnabled,
			TelemetryUrl:     tel.TelemetryUrl,
			MaxNodes:         maxNodes,
			DeploymentId:     deploymentUUID,
		}, nil
	}

	return &telemetry.TelemetryResponse{
		TelemetryEnabled: tel.TelemetryEnabled,
		TelemetryUrl:     tel.TelemetryUrl,
		DeploymentId:     deploymentUUID}, nil
}

func (t *TelemetryServer) getDeploymentID(ctx context.Context) (string, error) {
	deployIDResponse, err := t.deploy_client.DeployID(ctx, &deployment_service.DeployIDRequest{})
	if err != nil {
		if isServiceDownError(err) {
			log.WithFields(log.Fields{
				"err":  err,
				"func": "getDeploymentID",
			}).Error("connecting to the deployment client")
			return "", nil
		}
		return "", err
	}

	log.Debugf("deployIDResponse.DeploymentId: %s", deployIDResponse.DeploymentId)

	return deployIDResponse.DeploymentId, nil
}

func isServiceDownError(err error) bool {
	return status.Code(err) == codes.Unavailable
}

func ToUUID(id string) string {
	if id != "" {
		deploymentUUID, err := uuid.FromString(id)
		if err != nil { // Too short to be a valid uuid, make new one
			deploymentUUID = uuid.NewV5(uuid.NamespaceOID, id)
		}
		return deploymentUUID.String()
	}
	return id //if there is no deployment id we can just move along and let the consumer pick a default
}
