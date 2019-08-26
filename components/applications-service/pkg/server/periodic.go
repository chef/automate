package server

import (
	"context"
	"fmt"
	"time"

	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/lib/cereal"
	cerealRPC "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/secureconn"
	certs "github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/version"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
)

type JobScheduler struct {
	CerealSvc *cereal.Manager
}

type DisconnectedServicesParamsV0 struct {
	ThresholdSeconds int `mapstructure:"threshold_seconds"`
}

const (
	cerealServiceMutualTLSName = "cereal-service"

	DisconnectedServicesJobIntervalSeconds = 60
	DisconnectedServicesJobName            = "disconnected_services"
	DisconnectedServicesScheduleName       = "periodic_disconnected_services"
)

func ConnectToJobsManager(jobCfg *config.Jobs, certs *certs.ServiceCerts) (*cereal.Manager, error) {
	svcURL := fmt.Sprintf("%s:%d", jobCfg.Host, jobCfg.Port)

	connFactory := secureconn.NewFactory(*certs,
		secureconn.WithVersionInfo(version.Version, version.GitSHA))
	conn, err := connFactory.Dial(cerealServiceMutualTLSName, svcURL)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to connect to upstream job service %q at %q", cerealServiceMutualTLSName, svcURL)
	}

	be := cerealRPC.NewGrpcBackendFromConn("applications", conn)
	cerealSvc, err := cereal.NewManager(be)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create cereal job manager from gRPC connection")
	}
	return cerealSvc, nil
}

func NewJobScheduler(cerealSvc *cereal.Manager) *JobScheduler {
	return &JobScheduler{CerealSvc: cerealSvc}
}

// SetupScheduler ensures all our jobs exist in the cereal service backend.
func (j *JobScheduler) Setup() error {
	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: DisconnectedServicesJobIntervalSeconds,
		Dtstart:  time.Now(),
	})
	if err != nil {
		return errors.Wrap(err, "failed to create job scheduler configuration")
	}

	params := defaultDisconnectedServicesJobParams()

	logCtx := log.WithFields(log.Fields{
		"scheduleName": DisconnectedServicesScheduleName,
		"jobName":      DisconnectedServicesJobName,
		"jobParams":    params,
		"recurrance":   r,
	})

	err = j.CerealSvc.CreateWorkflowSchedule(
		DisconnectedServicesScheduleName,
		DisconnectedServicesJobName,
		defaultDisconnectedServicesJobParams(),
		true,
		r,
	)
	if err == cereal.ErrWorkflowScheduleExists {
		logCtx.Debug("workflow already created")
	} else if err != nil {
		logCtx.Error("failed to create workflow schedule")
		return errors.Wrap(err, "failed to create workflow schedule")
	} else {
		logCtx.Info("created workflow schedule")
	}

	return nil
}

func (j *JobScheduler) UpdateDisconnectedServicesJobParams(ctx context.Context, params *DisconnectedServicesParamsV0) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesJobName,
		cereal.UpdateParameters(params))
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) EnableDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesJobName,
		cereal.UpdateEnabled(true))
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) DisableDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesJobName,
		cereal.UpdateEnabled(false))
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to disabled")
	}
	return nil

}

func defaultDisconnectedServicesJobParams() *DisconnectedServicesParamsV0 {
	return &DisconnectedServicesParamsV0{ThresholdSeconds: 300}
}

func StartJobRunners(applicationsServer *ApplicationsServer, cerealSvc *cereal.Manager) error {
	err := cerealSvc.RegisterTaskExecutor(
		DisconnectedServicesJobName,
		&markDisconnectedServicesExecutor{ApplicationsServer: applicationsServer},
		cereal.TaskExecutorOpts{},
	)
	if err != nil {
		return errors.Wrap(err, "failed to register as task exector to mark disconnected services")
	}

	wfX := patterns.NewSingleTaskWorkflowExecutor(DisconnectedServicesJobName, false)
	err = cerealSvc.RegisterWorkflowExecutor(DisconnectedServicesJobName, wfX)
	if err != nil {
		return errors.Wrap(err, "failed to register as workflow exector to mark disconnected services")
	}

	// TODO: set a timeout
	ctx := context.Background()

	err = cerealSvc.Start(ctx)
	if err != nil {
		return errors.Wrap(err, "failed to start workflow/job exector")
	}

	return nil
}

type markDisconnectedServicesExecutor struct {
	ApplicationsServer *ApplicationsServer
}

func (m *markDisconnectedServicesExecutor) Run(ctx context.Context, t cereal.Task) (interface{}, error) {
	var params DisconnectedServicesParamsV0
	if err := t.GetParameters(&params); err != nil {
		return nil, errors.Wrap(err, "failed to load parameters for disconnected_services job")
	}
	_, err := m.ApplicationsServer.MarkDisconnectedServices(int32(params.ThresholdSeconds))
	if err != nil {
		return nil, errors.Wrap(err, "failed periodic disconnected_services job")
	}
	return nil, nil
}
