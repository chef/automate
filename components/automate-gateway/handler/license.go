package handler

import (
	"context"
	"net"
	"net/url"
	"syscall"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	deployment_service "github.com/chef/automate/api/interservice/deployment"
	license_control "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/automate-gateway/api/license"
	tlc "github.com/chef/automate/components/trial-license-service/pkg/client"
)

type LicenseServer struct {
	client       license_control.LicenseControlClient
	deployClient deployment_service.DeploymentClient
	tlClient     tlc.Client
}

// NewLicenseServer returns the initialized state of the license handlers: a
// client to license-control-service and the URL of trial-license-service
func NewLicenseServer(
	client license_control.LicenseControlClient,
	deployClient deployment_service.DeploymentClient,
	trialLicenseURL *url.URL) *LicenseServer {
	var tlClient tlc.Client
	if trialLicenseURL != nil {
		tlClient = tlc.New(trialLicenseURL)
	} else {
		tlClient = nil
	}
	return &LicenseServer{client, deployClient, tlClient}
}

// ApplyLicense applies the provided license token (provided as string
// JWT) to the system.
func (t *LicenseServer) ApplyLicense(ctx context.Context,
	req *license.ApplyLicenseReq) (*license.ApplyLicenseResp, error) {

	err := t.applyLicense(ctx, req.License)
	if err != nil {
		return nil, err
	}

	st, err := t.GetStatus(ctx, &license.GetStatusReq{})
	if err != nil {
		return nil, err
	}

	status := license.GetStatusResp{
		LicenseId:      st.LicenseId,
		ConfiguredAt:   st.ConfiguredAt,
		CustomerName:   st.CustomerName,
		LicensedPeriod: st.LicensedPeriod,
	}
	return &license.ApplyLicenseResp{Status: &status}, nil
}

// GetStatus queries l-c-s for the status of the license currently in use
func (t *LicenseServer) GetStatus(ctx context.Context,
	req *license.GetStatusReq) (*license.GetStatusResp, error) {

	lcResp, err := t.client.Status(ctx, &license_control.StatusRequest{})
	if err != nil {
		return nil, err
	}
	if lcResp.GetLicenseId() == "" {
		return nil, status.Errorf(codes.NotFound, "no license")
	}

	resp := license.GetStatusResp{
		LicenseId:      lcResp.LicenseId,
		ConfiguredAt:   lcResp.ConfiguredAt,
		CustomerName:   lcResp.CustomerName,
		LicensedPeriod: (*license.GetStatusResp_DateRange)(lcResp.LicensedPeriod),
	}
	return &resp, nil
}

func (t *LicenseServer) RequestLicense(ctx context.Context,
	req *license.RequestLicenseReq) (*license.RequestLicenseResp, error) {

	// not configured, fail request
	if t.tlClient == nil {
		return nil, status.Error(codes.Internal, "not configured to acquire a trial license")
	}

	// we probably want to return the same error statuses here (like "service unavailable")
	chefAutomateVersion, err := t.getAutomateVersion(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	deploymentID, err := t.getDeploymentID(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, "couldn't identify the requesting Automate installation")
	}

	l, err := t.requestLicense(ctx, req, deploymentID, chefAutomateVersion)
	if err != nil {
		return nil, err
	}

	err = t.applyLicense(ctx, l)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	resp := license.RequestLicenseResp{
		License: l,
	}

	st, err := t.GetStatus(ctx, &license.GetStatusReq{})
	if err != nil {
		return nil, err
	}

	resp.Status = &license.GetStatusResp{
		LicenseId:      st.LicenseId,
		ConfiguredAt:   st.ConfiguredAt,
		CustomerName:   st.CustomerName,
		LicensedPeriod: st.LicensedPeriod,
	}
	return &resp, nil
}

func (t *LicenseServer) getDeploymentID(ctx context.Context) (string, error) {
	deployIDResponse, err := t.deployClient.DeployID(ctx, &deployment_service.DeployIDRequest{})
	if err != nil {
		return "", err
	}

	return deployIDResponse.DeploymentId, nil
}

func (t *LicenseServer) getAutomateVersion(ctx context.Context) (string, error) {
	resp, err := t.deployClient.ManifestVersion(ctx, &deployment_service.ManifestVersionRequest{})
	if err != nil {
		return "", err
	}
	return resp.BuildTimestamp, nil
}

func (t *LicenseServer) applyLicense(ctx context.Context, license string) error {

	log := ctxlogrus.Extract(ctx)
	log.Info("applying license via License Control Service")

	lcReq := &license_control.UpdateRequest{
		LicenseData: license,
		// force determines behavior when the same license is
		// re-applied. When false, the license won't be
		// reapplied, but the call returns successfully.
		Force: false,
	}
	_, err := t.client.Update(ctx, lcReq)
	result := "successful"
	if err != nil {
		result = err.Error()
	}
	log.Info("applying license--result: " + result)
	return err
}

func (t *LicenseServer) requestLicense(
	ctx context.Context,
	req *license.RequestLicenseReq,
	deploymentID string,
	chefAutomateVersion string) (string, error) {

	log := ctxlogrus.Extract(ctx)
	log.Info("requesting license via Trial License Service")

	license, err := t.tlClient.RequestTrialLicense(
		ctx,
		req.FirstName,
		req.LastName,
		req.Email,
		req.GdprAgree,
		deploymentID,
		chefAutomateVersion)

	result := "successful"
	if err != nil {
		result = err.Error()
	}
	log.Info("requesting license--result: " + result)

	if err != nil {
		// is this a networking issue?
		if op, ok := errors.Cause(err).(*net.OpError); ok {
			err = op.Err // unwrap
			if dns, ok := err.(*net.DNSError); ok {
				return "", status.Error(codes.FailedPrecondition, dns.Error())
			}
			if errno, ok := err.(syscall.Errno); ok {
				return "", status.Errorf(codes.FailedPrecondition, "%s (errno %d)", err.Error(), errno)
			}
		}
		if u, ok := errors.Cause(err).(*url.Error); ok {
			return "", status.Error(codes.FailedPrecondition, u.Error())
		}

		// differentiate "bad argument"-type errors
		if e, ok := err.(*tlc.UnexpectedStatusError); ok && e.Code() >= 400 && e.Code() < 500 {
			return "", status.Error(codes.InvalidArgument, e.Error())
		}
		return "", status.Error(codes.Internal, err.Error())
	}
	return license, nil
}
