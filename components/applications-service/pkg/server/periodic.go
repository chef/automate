package server

import (
	"context"
	"fmt"
	"sync/atomic"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/lib/cereal"
	cerealRPC "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/simpledatemath"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
)

type JobScheduler struct {
	CerealSvc *cereal.Manager
}

type DisconnectedServicesConfigV0 struct {
	Enabled bool
	Params  *DisconnectedServicesParamsV0
}

type DisconnectedServicesParamsV0 struct {
	ThresholdDuration string `mapstructure:"threshold_duration"`
}

const (
	cerealServiceMutualTLSName = "cereal-service"

	DefaultJobIntervalSeconds = 60

	DisconnectedServicesJobName      = "disconnected_services"
	DisconnectedServicesScheduleName = "periodic_disconnected_services"

	DeleteDisconnectedServicesJobName      = "delete_disconnected_services"
	DeleteDisconnectedServicesScheduleName = "periodic_delete_disconnected_services"
)

func ConnectToJobsManager(jobCfg *config.Jobs, connFactory *secureconn.Factory) (*cereal.Manager, error) {
	svcURL := fmt.Sprintf("%s:%d", jobCfg.Host, jobCfg.Port)
	conn, err := connFactory.Dial(cerealServiceMutualTLSName, svcURL)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to connect to upstream job service %q at %q", cerealServiceMutualTLSName, svcURL)
	}

	be := cerealRPC.NewGrpcBackendFromConn("applications", conn)
	cerealSvc, err := cereal.NewManager(be, cereal.WithTaskPollInterval(1*time.Second), cereal.WithTaskPollIntervalMaxJitter(100*time.Millisecond))
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
		Interval: DefaultJobIntervalSeconds,
		Dtstart:  time.Now().UTC(),
	})
	if err != nil {
		return errors.Wrap(err, "failed to create job scheduler configuration")
	}

	err = j.createWorkflowIfMissing(
		context.Background(),
		DisconnectedServicesScheduleName,
		DisconnectedServicesJobName,
		defaultDisconnectedServicesJobParams(),
		r,
	)

	if err != nil {
		return err
	}

	r, err = rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: DefaultJobIntervalSeconds,
		Dtstart:  time.Now().UTC(),
	})
	if err != nil {
		return errors.Wrap(err, "failed to create job scheduler configuration")
	}

	err = j.createWorkflowIfMissing(
		context.Background(),
		DeleteDisconnectedServicesScheduleName,
		DeleteDisconnectedServicesJobName,
		defaultDeleteDisconnectedServicesJobParams(),
		r,
	)

	if err != nil {
		return err
	}

	return nil
}

func (j *JobScheduler) ResetParams() error {
	ctx := context.Background()

	if err := j.UpdateDisconnectedServicesJobParams(ctx, defaultDisconnectedServicesJobParams()); err != nil {
		return err
	}
	if err := j.UpdateDeleteDisconnectedServicesJobParams(ctx, defaultDeleteDisconnectedServicesJobParams()); err != nil {
		return err
	}

	return nil
}

func (j *JobScheduler) createWorkflowIfMissing(
	ctx context.Context,
	scheduleName string,
	jobName string,
	jobParams interface{},
	recurrence *rrule.RRule,
) error {
	logCtx := log.WithFields(log.Fields{
		"scheduleName": scheduleName,
		"jobName":      jobName,
		"jobParams":    jobParams,
		"recurrence":   recurrence,
	})

	err := j.CerealSvc.CreateWorkflowSchedule(
		ctx,
		scheduleName,
		jobName,
		jobParams,
		true,
		recurrence,
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

func (j *JobScheduler) GetDisconnectedServicesJobConfig(ctx context.Context) (*DisconnectedServicesConfigV0, error) {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesJobName)
	if err != nil {
		return nil, errors.Wrap(err, "failed to retrieve schedule and config for disconnected_services job")
	}

	var returnedParams DisconnectedServicesParamsV0
	if err := sched.GetParameters(&returnedParams); err != nil {
		return nil, errors.Wrap(err, "unable to load disconnected_services job parameters")
	}

	return &DisconnectedServicesConfigV0{
		Enabled: sched.Enabled,
		Params:  &returnedParams,
	}, nil
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

func (j *JobScheduler) GetDeleteDisconnectedServicesJobConfig(ctx context.Context) (*DisconnectedServicesConfigV0, error) {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesJobName)
	if err != nil {
		return nil, errors.Wrap(err, "failed to retrieve schedule and config for delete_disconnected_services job")
	}

	var returnedParams DisconnectedServicesParamsV0
	if err := sched.GetParameters(&returnedParams); err != nil {
		return nil, errors.Wrap(err, "unable to load delete_disconnected_services job parameters")
	}

	return &DisconnectedServicesConfigV0{
		Enabled: sched.Enabled,
		Params:  &returnedParams,
	}, nil
}

func (j *JobScheduler) UpdateDeleteDisconnectedServicesJobParams(ctx context.Context, params *DisconnectedServicesParamsV0) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesJobName,
		cereal.UpdateParameters(params))
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to enabled")
	}
	log.WithFields(log.Fields{"new_params": params}).Info("Updated delete_disconnected_services params")
	return nil
}

func (j *JobScheduler) EnableDeleteDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesJobName,
		cereal.UpdateEnabled(true))
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) DisableDeleteDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesJobName,
		cereal.UpdateEnabled(false))
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to disabled")
	}
	return nil

}

// RunAllJobsConstantly sets all the jobs to run every 1 second. Intended for
// use in testing scenarios.
func (j *JobScheduler) RunAllJobsConstantly(ctx context.Context) error {
	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 1,
		Dtstart:  time.Now(),
	})
	if err != nil {
		return errors.Wrap(err, "could not construct new recurrence rule")
	}

	err = j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesJobName,
		cereal.UpdateRecurrence(r))
	if err != nil {
		return errors.Wrap(err, "failed to update recurrence of disconnected_services schedule")
	}

	err = j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesJobName,
		cereal.UpdateRecurrence(r))
	if err != nil {
		return errors.Wrap(err, "failed to update recurrence of delete_disconnected_services schedule")
	}
	return nil
}

func defaultDisconnectedServicesJobParams() *DisconnectedServicesParamsV0 {
	return &DisconnectedServicesParamsV0{ThresholdDuration: "5m"}
}

func defaultDeleteDisconnectedServicesJobParams() *DisconnectedServicesParamsV0 {
	return &DisconnectedServicesParamsV0{ThresholdDuration: "7d"}
}

type JobRunnerSet struct {
	MarkDisconnectedServicesExecutor   *MarkDisconnectedServicesExecutor
	DeleteDisconnectedServicesExecutor *DeleteDisconnectedServicesExecutor
}

func NewJobRunnerSet(applicationsServer *ApplicationsServer) *JobRunnerSet {
	return &JobRunnerSet{
		MarkDisconnectedServicesExecutor: &MarkDisconnectedServicesExecutor{
			ApplicationsServer: applicationsServer,
		},
		DeleteDisconnectedServicesExecutor: &DeleteDisconnectedServicesExecutor{
			ApplicationsServer: applicationsServer,
		},
	}
}

func (j *JobRunnerSet) Start(cerealSvc *cereal.Manager) error {
	err := cerealSvc.RegisterTaskExecutor(
		DisconnectedServicesJobName,
		j.MarkDisconnectedServicesExecutor,
		cereal.TaskExecutorOpts{},
	)
	if err != nil {
		return errors.Wrap(err, "failed to register as task executor to mark disconnected services")
	}

	wfX := patterns.NewSingleTaskWorkflowExecutor(DisconnectedServicesJobName, false)
	err = cerealSvc.RegisterWorkflowExecutor(DisconnectedServicesJobName, wfX)
	if err != nil {
		return errors.Wrap(err, "failed to register as workflow executor to mark disconnected services")
	}

	err = cerealSvc.RegisterTaskExecutor(
		DeleteDisconnectedServicesJobName,
		j.DeleteDisconnectedServicesExecutor,
		cereal.TaskExecutorOpts{},
	)

	if err != nil {
		return errors.Wrap(err, "failed to register as task executor to delete disconnected services")
	}

	wfX = patterns.NewSingleTaskWorkflowExecutor(DeleteDisconnectedServicesJobName, false)
	err = cerealSvc.RegisterWorkflowExecutor(DeleteDisconnectedServicesJobName, wfX)
	if err != nil {
		return errors.Wrap(err, "failed to register as workflow executor to mark disconnected services")
	}

	// TODO: set a timeout
	ctx := context.Background()

	err = cerealSvc.Start(ctx)
	if err != nil {
		return errors.Wrap(err, "failed to start workflow/job executor")
	}

	return nil
}

type MarkDisconnectedServicesExecutor struct {
	ApplicationsServer  *ApplicationsServer
	totalRuns           int64
	totalRunsFailed     int64
	totalRunsSuccessful int64
}

func (m *MarkDisconnectedServicesExecutor) Run(ctx context.Context, t cereal.Task) (interface{}, error) {
	err := m.runWithoutStats(t)
	atomic.AddInt64(&m.totalRuns, 1)
	if err != nil {
		atomic.AddInt64(&m.totalRunsFailed, 1)
	} else {
		atomic.AddInt64(&m.totalRunsSuccessful, 1)
	}
	return nil, err
}

func (m *MarkDisconnectedServicesExecutor) runWithoutStats(t cereal.Task) error {
	var params DisconnectedServicesParamsV0
	if err := t.GetParameters(&params); err != nil {
		return errors.Wrap(err, "failed to load parameters for disconnected_services job")
	}
	threshold, err := simpledatemath.Parse(params.ThresholdDuration)
	if err != nil {
		return errors.Wrapf(err, "duration setting %q for disconnected_services in database is invalid", params.ThresholdDuration)
	}
	_, err = m.ApplicationsServer.MarkDisconnectedServices(int32(threshold.Seconds()))
	if err != nil {
		return errors.Wrap(err, "failed periodic disconnected_services job")
	}
	return nil
}

func (m *MarkDisconnectedServicesExecutor) TotalRuns() int64 {
	return atomic.LoadInt64(&m.totalRuns)
}

type DeleteDisconnectedServicesExecutor struct {
	ApplicationsServer  *ApplicationsServer
	totalRuns           int64
	totalRunsFailed     int64
	totalRunsSuccessful int64
}

func (d *DeleteDisconnectedServicesExecutor) Run(ctx context.Context, t cereal.Task) (interface{}, error) {
	err := d.runWithoutStats(ctx, t)
	atomic.AddInt64(&d.totalRuns, 1)
	if err != nil {
		atomic.AddInt64(&d.totalRunsFailed, 1)
	} else {
		atomic.AddInt64(&d.totalRunsSuccessful, 1)
	}
	return nil, err
}

func (d *DeleteDisconnectedServicesExecutor) runWithoutStats(ctx context.Context, t cereal.Task) error {
	logCtx := log.WithFields(log.Fields{"task_name": DeleteDisconnectedServicesJobName})
	var params DisconnectedServicesParamsV0
	if err := t.GetParameters(&params); err != nil {
		return errors.Wrap(err, "failed to load parameters for disconnected_services job")
	}
	threshold, err := simpledatemath.Parse(params.ThresholdDuration)
	if err != nil {
		return errors.Wrapf(err, "duration setting %q for disconnected_services in database is invalid", params.ThresholdDuration)
	}
	logCtx = logCtx.WithFields(log.Fields{"threshold_duration": threshold})

	req := applications.DisconnectedServicesReq{
		ThresholdSeconds: int32(threshold.Seconds()),
	}

	svcRemovedRes, err := d.ApplicationsServer.DeleteDisconnectedServices(ctx, &req)
	if err != nil {
		logCtx.WithError(err).Error("periodic delete_disconnected_services failed")
		return err
	}

	logCtx.WithFields(log.Fields{"svcs_removed": len(svcRemovedRes.Services)}).Info("periodic task succeeded")
	return nil
}

func (d *DeleteDisconnectedServicesExecutor) TotalRuns() int64 {
	return atomic.LoadInt64(&d.totalRuns)
}
