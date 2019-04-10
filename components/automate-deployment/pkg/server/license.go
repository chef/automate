package server

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/lib/license"
)

func (s *server) newLicenseControlServiceConnection(ctx context.Context,
	timeout time.Duration) (*grpc.ClientConn, error) {

	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	connection, err := s.connFactory.DialContext(
		ctx,
		"license-control-service",
		s.AddressForService("license-control-service"),
		grpc.WithBlock(),
	)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to connect to license-control-service")
	}

	return connection, nil
}

func (s *server) licenseControlClient(ctx context.Context) (lc.LicenseControlClient, error) {
	if s.lcClient != nil {
		return s.lcClient, nil
	}

	connection, err := s.newLicenseControlServiceConnection(ctx, 5*time.Second)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to create connection to license-control-service")
	}

	s.lcClient = lc.NewLicenseControlClient(connection)
	return s.lcClient, nil
}

// LicenseApply attempts to set the license provided by the user
func (s *server) LicenseApply(ctx context.Context, req *api.LicenseApplyRequest) (*api.LicenseApplyResponse, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	response, err := s.UpdateLicense(ctx, req.License, req.Force)
	if err != nil {
		return nil, err
	}

	return &api.LicenseApplyResponse{
		Updated:   response.Updated,
		Message:   response.Message,
		Duplicate: response.Duplicate,
	}, nil
}

// UpdateLicense sends a given license to be updated
func (s *server) UpdateLicense(ctx context.Context, license string, force bool) (*lc.UpdateResponse, error) {
	lcClient, err := s.licenseControlClient(ctx)
	if err != nil {
		logrus.WithError(err).Error("Could not set license")
		return nil, err
	}

	response, err := lcClient.Update(ctx, &lc.UpdateRequest{LicenseData: license, Force: force})
	if err != nil {
		logrus.WithError(err).Error("Could not set license")

		return nil, err
	}

	return response, nil
}

// LicenseStatus gets the current state of the Automate License from license-control-service
func (s *server) LicenseStatus(ctx context.Context, req *api.LicenseStatusRequest) (*api.LicenseStatusResponse, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	lcClient, err := s.licenseControlClient(ctx)
	if err != nil {
		logrus.WithError(err).Error("Could not set license")
		return nil, err
	}

	response, err := lcClient.Status(ctx, &lc.StatusRequest{})
	if err != nil {
		logrus.WithError(err).Error("Could not retrieve status")
		return nil, errors.WithMessage(err, "Failed to retrieve status.")
	}

	// No license currently set
	if response.LicenseId == "" {
		return &api.LicenseStatusResponse{Set: false}, nil
	}

	// Return current license information
	return &api.LicenseStatusResponse{
		Set:            true,
		ExpirationDate: response.LicensedPeriod.End,
		LicenseId:      response.LicenseId,
		CustomerName:   response.CustomerName,
	}, nil
}

// License gets the Automate License from license-control-service
func (s *server) License(ctx context.Context) (*license.License, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	lcClient, err := s.licenseControlClient(ctx)
	if err != nil {
		return nil, err
	}

	response, err := lcClient.License(ctx, &lc.LicenseRequest{})
	if err != nil {
		logrus.WithError(err).Error("Failed to retrieve license")
		return nil, status.Error(codes.Internal, err.Error())
	}

	if response.License == nil {
		return nil, status.Error(codes.NotFound, "No License Set")
	}

	return response.License, nil
}
