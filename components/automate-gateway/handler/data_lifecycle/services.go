package datalifecycle

import (
	"context"
	"fmt"
	"strings"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	a "github.com/chef/automate/api/external/applications"
	api "github.com/chef/automate/api/external/data_lifecycle"
	"github.com/golang/protobuf/ptypes/wrappers"
)

// if a new one is added here, add it to ValidServicesJobNames below so our
// error message stays correct
const (
	DisconnectedServicesJobName       = "disconnected_services"
	DeleteDisconnectedServicesJobName = "delete_disconnected_services"
)

var ValidServicesJobNames = []string{
	DisconnectedServicesJobName,
	DeleteDisconnectedServicesJobName,
}

// The applications service is still under the beta flag. We'll leave these
// unimplemented until those APIs become stable and we can integrate them.

// GetServicesStatus returns the applications data lifecycle status
func (s *Server) GetServicesStatus(ctx context.Context, req *api.GetServicesStatusRequest) (*api.GetServicesStatusResponse, error) {
	dsc, err := s.appsClient.GetDisconnectedServicesConfig(ctx, &a.GetDisconnectedServicesConfigReq{})
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}
	ddsc, err := s.appsClient.GetDeleteDisconnectedServicesConfig(ctx, &a.GetDeleteDisconnectedServicesConfigReq{})
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}
	res := api.GetServicesStatusResponse{
		Jobs: []*api.JobStatus{
			{
				Name:           DisconnectedServicesJobName,
				Disabled:       !dsc.Running.GetValue(),
				Recurrence:     dsc.Recurrence,
				Threshold:      dsc.Threshold,
				LastEnqueuedAt: dsc.JobInfo.LastEnqueuedAt,
				NextDueAt:      dsc.JobInfo.NextDueAt,
				LastStartedAt:  dsc.JobInfo.LastStartedAt,
				LastEndedAt:    dsc.JobInfo.LastEndedAt,
				LastElapsed:    dsc.JobInfo.LastElapsed,
			},
			{
				Name:           DeleteDisconnectedServicesJobName,
				Disabled:       !ddsc.Running,
				Recurrence:     ddsc.Recurrence,
				Threshold:      ddsc.Threshold,
				LastEnqueuedAt: ddsc.JobInfo.LastEnqueuedAt,
				NextDueAt:      ddsc.JobInfo.NextDueAt,
				LastStartedAt:  ddsc.JobInfo.LastStartedAt,
				LastEndedAt:    ddsc.JobInfo.LastEndedAt,
				LastElapsed:    ddsc.JobInfo.LastElapsed,
			},
		},
	}

	return &res, nil
}

// RunServices runs the applications data life cycle jobs
func (s *Server) RunServices(ctx context.Context, req *api.RunServicesRequest) (*api.RunServicesResponse, error) {
	var err error
	_, err = s.appsClient.RunDisconnectedServicesJob(ctx, &a.RunDisconnectedServicesJobReq{})
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}
	_, err = s.appsClient.RunDeleteDisconnectedServicesJob(ctx, &a.RunDeleteDisconnectedServicesJobReq{})
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}
	return &api.RunServicesResponse{}, nil
}

// SetServicesConfig configures the applications data lifecycle jobs
func (s *Server) SetServicesConfig(ctx context.Context, req *api.SetServicesConfigRequest) (*api.SetServicesConfigResponse, error) {
	var err error
	for _, job := range req.JobSettings {
		switch job.Name {
		case DisconnectedServicesJobName:
			if err = s.configureDisconnedServices(ctx, job); err != nil {
				return nil, status.Errorf(codes.Internal, err.Error())
			}
		case DeleteDisconnectedServicesJobName:
			if err = s.configureDeleteDisconnedServices(ctx, job); err != nil {
				return nil, status.Errorf(codes.Internal, err.Error())
			}
		default:
			errMsg := fmt.Sprintf("invalid job name %q; valid names are '%s'", job.Name, strings.Join(ValidServicesJobNames, "', '"))
			return nil, status.Errorf(codes.InvalidArgument, errMsg)
		}
	}
	return &api.SetServicesConfigResponse{}, nil
}

func (s *Server) configureDisconnedServices(ctx context.Context, req *api.JobSettings) error {
	_, err := s.appsClient.UpdateDisconnectedServicesConfig(ctx,
		&a.PeriodicMandatoryJobConfig{
			Threshold:  req.Threshold,
			Recurrence: req.Recurrence,
			Running:    &wrappers.BoolValue{Value: !req.Disabled},
		})
	return err
}

func (s *Server) configureDeleteDisconnedServices(ctx context.Context, req *api.JobSettings) error {
	_, err := s.appsClient.UpdateDeleteDisconnectedServicesConfig(ctx,
		&a.PeriodicJobConfig{
			Threshold:  req.Threshold,
			Recurrence: req.Recurrence,
			Running:    !req.Disabled,
		})
	return err
}
