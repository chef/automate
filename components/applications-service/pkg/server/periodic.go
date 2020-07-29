package server

import (
	"context"
	"fmt"
	"strings"
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

type DisconnectedServicesConfigAndInfo struct {
	*DisconnectedServicesConfigV0
	*DisconnectedServicesInfo
}

// + enabled/disabled for both jobs, not just the deleter
// + recurrence for both
type DisconnectedServicesConfigV0 struct {
	Enabled    bool
	Params     *DisconnectedServicesParamsV0
	Recurrence string // string representation of rrule
	rr         *rrule.RRule
	validated  bool
}

type DisconnectedServicesInfo struct {
	LastEnqueuedAt *time.Time
	LastStartedAt  *time.Time
	LastEndedAt    *time.Time
	LastElapsed    *time.Duration
	NextDueAt      *time.Time
}

type DisconnectedServicesParamsV0 struct {
	ThresholdDuration string `mapstructure:"threshold_duration"`
}

func (c *DisconnectedServicesConfigV0) Validate() (bool, string) {
	if c.validated {
		return true, ""
	}

	var messages []string

	d := c.Params.ThresholdDuration
	if d != "" {
		err := simpledatemath.Validate(d)
		if err != nil {
			err := errors.Wrapf(err, "unable to parse threshold value %q", d)
			messages = append(messages, err.Error())
		}
	}
	if c.Recurrence != "" {
		rr, err := rrule.StrToRRule(c.Recurrence)
		if err != nil {
			err := errors.Wrapf(err, "unable to parse recurrence value %q", c.Recurrence)
			messages = append(messages, err.Error())
		}
		c.rr = rr
	}
	if len(messages) != 0 {
		return false, strings.Join(messages, "; ")
	}
	c.validated = true
	return true, ""
}

func (c *DisconnectedServicesConfigV0) Verror() error {
	if valid, msg := c.Validate(); !valid {
		return errors.New(msg)
	}
	return nil
}

const (
	cerealServiceMutualTLSName = "cereal-service"
	DefaultRecurrence          = "FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=60"
	DefaultJobIntervalSeconds  = 60
)

var (
	DisconnectedServicesWorkflowName = cereal.NewWorkflowName("disconnected_services")
	DisconnectedServicesScheduleName = "periodic_disconnected_services"

	DeleteDisconnectedServicesWorkflowName = cereal.NewWorkflowName("delete_disconnected_services")
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
	defaultConfig := defaultDisconnectedServicesJobConfig()
	r, err := rrule.StrToRRule(defaultConfig.Recurrence)
	if err != nil {
		return errors.Wrap(err, "failed to create job scheduler configuration")
	}

	err = j.createWorkflowIfMissing(
		context.Background(),
		DisconnectedServicesScheduleName,
		DisconnectedServicesWorkflowName,
		defaultConfig.Params,
		r,
	)

	if err != nil {
		return err
	}

	deleteConf := defaultDeleteDisconnectedServicesJobConfig()
	r2, err := rrule.StrToRRule(deleteConf.Recurrence)
	if err != nil {
		return errors.Wrap(err, "failed to create job scheduler configuration")
	}

	err = j.createWorkflowIfMissing(
		context.Background(),
		DeleteDisconnectedServicesScheduleName,
		DeleteDisconnectedServicesWorkflowName,
		deleteConf.Params,
		r2,
	)

	if err != nil {
		return err
	}

	return nil
}

// FIXME: should this be changed to do a full reset?
func (j *JobScheduler) ResetParams() error {
	ctx := context.Background()

	conf := defaultDisconnectedServicesJobConfig()

	if err := j.UpdateDisconnectedServicesJobConfig(ctx, conf); err != nil {
		return err
	}

	if err := j.EnableDisconnectedServicesJob(ctx); err != nil {
		return err
	}

	deleteConf := defaultDeleteDisconnectedServicesJobConfig()
	if err := j.UpdateDeleteDisconnectedServicesJobConfig(ctx, deleteConf); err != nil {
		return err
	}
	if err := j.EnableDeleteDisconnectedServicesJob(ctx); err != nil {
		return err
	}

	return nil
}

func (j *JobScheduler) createWorkflowIfMissing(
	ctx context.Context,
	scheduleName string,
	workflowName cereal.WorkflowName,
	jobParams interface{},
	recurrence *rrule.RRule,
) error {
	logCtx := log.WithFields(log.Fields{
		"scheduleName": scheduleName,
		"workflowName": workflowName,
		"jobParams":    jobParams,
		"recurrence":   recurrence,
	})

	err := j.CerealSvc.CreateWorkflowSchedule(
		ctx,
		scheduleName,
		workflowName,
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

func (j *JobScheduler) GetDisconnectedServicesJobConfig(ctx context.Context) (*DisconnectedServicesConfigAndInfo, error) {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName)
	if err != nil {
		return nil, errors.Wrap(err, "failed to retrieve schedule and config for disconnected_services job")
	}

	var returnedParams DisconnectedServicesParamsV0
	if err := sched.GetParameters(&returnedParams); err != nil {
		return nil, errors.Wrap(err, "unable to load disconnected_services job parameters")
	}

	ret := &DisconnectedServicesConfigAndInfo{
		DisconnectedServicesConfigV0: &DisconnectedServicesConfigV0{
			Enabled:    sched.Enabled,
			Recurrence: sched.Recurrence,
			Params:     &returnedParams,
		},
		DisconnectedServicesInfo: &DisconnectedServicesInfo{
			LastEnqueuedAt: &sched.LastEnqueuedAt,
			LastStartedAt:  sched.LastStart,
			LastEndedAt:    sched.LastEnd,
			NextDueAt:      &sched.NextDueAt,
		},
	}

	if (sched.LastStart != nil) && (sched.LastEnd != nil) {
		e := (*sched.LastEnd).Sub(*sched.LastStart)
		ret.DisconnectedServicesInfo.LastElapsed = &e
	}

	return ret, nil
}

func (j *JobScheduler) UpdateDisconnectedServicesJobConfig(ctx context.Context, conf *DisconnectedServicesConfigV0) error {
	var thingsToUpdate []cereal.WorkflowScheduleUpdateOpt

	if conf.Params.ThresholdDuration != "" {
		thingsToUpdate = append(thingsToUpdate, cereal.UpdateParameters(conf.Params))
	}

	if conf.Recurrence != "" {
		newRecurrence, err := rrule.StrToRRule(conf.Recurrence)
		if err != nil {
			return errors.Wrapf(err, "invalid rrule syntax for 'recurrence' in %q", conf.Recurrence)
		}
		thingsToUpdate = append(thingsToUpdate, cereal.UpdateRecurrence(newRecurrence))
	}

	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName,
		thingsToUpdate...,
	)
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) RunDisconnectedServicesJob(ctx context.Context) error {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName)
	if err != nil {
		return err
	}

	var threshold DisconnectedServicesParamsV0
	if err := sched.GetParameters(&threshold); err != nil {
		return errors.Wrap(err, "failed to load saved parameters for delete_disconnected_services from cereal")
	}

	err = j.CerealSvc.EnqueueWorkflow(ctx, DisconnectedServicesWorkflowName, DisconnectedServicesScheduleName, threshold)
	if err == cereal.ErrWorkflowInstanceExists {
		log.WithFields(log.Fields{
			"workflow_name": DisconnectedServicesWorkflowName,
			"schedule_name": DisconnectedServicesScheduleName,
		}).Warn("Periodic job already running, cannot enqueue it again")
		return nil
	}
	return err
}

func (j *JobScheduler) GetDeleteDisconnectedServicesJobConfig(ctx context.Context) (*DisconnectedServicesConfigAndInfo, error) {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName)
	if err != nil {
		return nil, errors.Wrap(err, "failed to retrieve schedule and config for delete_disconnected_services job")
	}

	var returnedParams DisconnectedServicesParamsV0
	if err := sched.GetParameters(&returnedParams); err != nil {
		return nil, errors.Wrap(err, "unable to load delete_disconnected_services job parameters")
	}

	ret := &DisconnectedServicesConfigAndInfo{
		DisconnectedServicesConfigV0: &DisconnectedServicesConfigV0{
			Enabled:    sched.Enabled,
			Recurrence: sched.Recurrence,
			Params:     &returnedParams,
		},
		DisconnectedServicesInfo: &DisconnectedServicesInfo{
			LastEnqueuedAt: &sched.LastEnqueuedAt,
			LastStartedAt:  sched.LastStart,
			LastEndedAt:    sched.LastEnd,
			NextDueAt:      &sched.NextDueAt,
		},
	}

	if (sched.LastStart != nil) && (sched.LastEnd != nil) {
		e := (*sched.LastEnd).Sub(*sched.LastStart)
		ret.DisconnectedServicesInfo.LastElapsed = &e
	}

	return ret, nil
}

func (j *JobScheduler) UpdateDeleteDisconnectedServicesJobConfig(ctx context.Context, conf *DisconnectedServicesConfigV0) error {
	var thingsToUpdate []cereal.WorkflowScheduleUpdateOpt

	if conf.Params.ThresholdDuration != "" {
		thingsToUpdate = append(thingsToUpdate, cereal.UpdateParameters(conf.Params))
	}

	if conf.Recurrence != "" {
		newRecurrence, err := rrule.StrToRRule(conf.Recurrence)
		if err != nil {
			return errors.Wrapf(err, "invalid rrule syntax for 'recurrence' in %q", conf.Recurrence)
		}
		thingsToUpdate = append(thingsToUpdate, cereal.UpdateRecurrence(newRecurrence))
	}

	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName,
		thingsToUpdate...)
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to enabled")
	}
	log.WithFields(log.Fields{"new_params": conf.Params}).Info("Updated delete_disconnected_services params")
	return nil
}

func (j *JobScheduler) RunDeleteDisconnectedServicesJob(ctx context.Context) error {
	sched, err := j.CerealSvc.GetWorkflowScheduleByName(ctx, DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName)
	if err != nil {
		return err
	}

	var threshold DisconnectedServicesParamsV0
	if err := sched.GetParameters(&threshold); err != nil {
		return errors.Wrap(err, "failed to load saved parameters for delete_disconnected_services from cereal")
	}

	err = j.CerealSvc.EnqueueWorkflow(ctx, DeleteDisconnectedServicesWorkflowName, DeleteDisconnectedServicesScheduleName, threshold)
	if err == cereal.ErrWorkflowInstanceExists {
		log.WithFields(log.Fields{
			"workflow_name": DeleteDisconnectedServicesWorkflowName,
			"schedule_name": DeleteDisconnectedServicesScheduleName,
		}).Warn("Periodic job already running, cannot enqueue it again")
		return nil
	}
	return err
}

func (j *JobScheduler) EnableDeleteDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName,
		cereal.UpdateEnabled(true))
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) DisableDeleteDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName,
		cereal.UpdateEnabled(false))
	if err != nil {
		return errors.Wrap(err, "failed to set delete_disconnected_services job to disabled")
	}
	return nil

}

func (j *JobScheduler) EnableDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName,
		cereal.UpdateEnabled(true))
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to enabled")
	}
	return nil
}

func (j *JobScheduler) DisableDisconnectedServicesJob(ctx context.Context) error {
	err := j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName,
		cereal.UpdateEnabled(false))
	if err != nil {
		return errors.Wrap(err, "failed to set disconnected_services job to disabled")
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
		DisconnectedServicesScheduleName, DisconnectedServicesWorkflowName,
		cereal.UpdateRecurrence(r))
	if err != nil {
		return errors.Wrap(err, "failed to update recurrence of disconnected_services schedule")
	}

	err = j.CerealSvc.UpdateWorkflowScheduleByName(
		ctx,
		DeleteDisconnectedServicesScheduleName, DeleteDisconnectedServicesWorkflowName,
		cereal.UpdateRecurrence(r))
	if err != nil {
		return errors.Wrap(err, "failed to update recurrence of delete_disconnected_services schedule")
	}
	return nil
}

func defaultDisconnectedServicesJobConfig() *DisconnectedServicesConfigV0 {
	return &DisconnectedServicesConfigV0{
		Enabled:    true,
		Recurrence: DefaultRecurrence,
		Params:     &DisconnectedServicesParamsV0{ThresholdDuration: "5m"},
	}
}

func defaultDeleteDisconnectedServicesJobConfig() *DisconnectedServicesConfigV0 {
	return &DisconnectedServicesConfigV0{
		Enabled:    true,
		Recurrence: DefaultRecurrence,
		Params:     &DisconnectedServicesParamsV0{ThresholdDuration: "7d"},
	}
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
	err := patterns.RegisterSingleTaskWorkflowExecutor(
		cerealSvc,
		DisconnectedServicesWorkflowName,
		false,
		j.MarkDisconnectedServicesExecutor,
		cereal.TaskExecutorOpts{})
	if err != nil {
		return errors.Wrap(err, "failed to register marked disconnected services executors")
	}

	err = patterns.RegisterSingleTaskWorkflowExecutor(
		cerealSvc,
		DeleteDisconnectedServicesWorkflowName,
		false,
		j.DeleteDisconnectedServicesExecutor,
		cereal.TaskExecutorOpts{},
	)
	if err != nil {
		return errors.Wrap(err, "failed to register delete disconnected services executors")
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
	logCtx := log.WithFields(log.Fields{
		"workflow_name": DisconnectedServicesWorkflowName,
		"schedule_name": DisconnectedServicesScheduleName,
		"executor":      "MarkDisconnectedServicesExecutor",
	})
	logCtx.Debug("Starting periodic task")
	err := m.runWithoutStats(t)
	atomic.AddInt64(&m.totalRuns, 1)
	if err != nil {
		atomic.AddInt64(&m.totalRunsFailed, 1)
		logCtx.WithFields(m.statsLogFields()).Error("periodic task failed")
	} else {
		atomic.AddInt64(&m.totalRunsSuccessful, 1)
		logCtx.WithFields(m.statsLogFields()).Info("periodic task succeeded")
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

func (m *MarkDisconnectedServicesExecutor) statsLogFields() log.Fields {
	return log.Fields{
		"lifetime_runs":            m.totalRuns,
		"lifetime_successful_runs": m.totalRunsSuccessful,
		"lifetime_failed_runs":     m.totalRunsFailed,
	}
}

type DeleteDisconnectedServicesExecutor struct {
	ApplicationsServer  *ApplicationsServer
	totalRuns           int64
	totalRunsFailed     int64
	totalRunsSuccessful int64
}

func (d *DeleteDisconnectedServicesExecutor) Run(ctx context.Context, t cereal.Task) (interface{}, error) {
	logCtx := log.WithFields(log.Fields{
		"workflow_name": DeleteDisconnectedServicesWorkflowName,
		"schedule_name": DeleteDisconnectedServicesScheduleName,
		"executor":      "DeleteDisconnectedServicesExecutor",
	})
	logCtx.Debug("Starting periodic task")
	err := d.runWithoutStats(ctx, t)
	atomic.AddInt64(&d.totalRuns, 1)
	if err != nil {
		atomic.AddInt64(&d.totalRunsFailed, 1)
		logCtx.WithFields(d.statsLogFields()).Error("periodic task failed")
	} else {
		atomic.AddInt64(&d.totalRunsSuccessful, 1)
		logCtx.WithFields(d.statsLogFields()).Info("periodic task succeeded")
	}
	return nil, err
}

func (d *DeleteDisconnectedServicesExecutor) runWithoutStats(ctx context.Context, t cereal.Task) error {
	logCtx := log.WithFields(log.Fields{"task_name": DeleteDisconnectedServicesWorkflowName})
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

	svcsRemoved := len(svcRemovedRes.Services)
	logCtx = logCtx.WithFields(log.Fields{"svcs_removed": svcsRemoved})
	if svcsRemoved == 0 {
		logCtx.Debug("task succeeded")
	} else {
		logCtx.Info("task succeeded")
	}
	return nil
}

func (d *DeleteDisconnectedServicesExecutor) TotalRuns() int64 {
	return atomic.LoadInt64(&d.totalRuns)
}

func (d *DeleteDisconnectedServicesExecutor) statsLogFields() log.Fields {
	return log.Fields{
		"lifetime_runs":            d.totalRuns,
		"lifetime_successful_runs": d.totalRunsSuccessful,
		"lifetime_failed_runs":     d.totalRunsFailed,
	}
}
