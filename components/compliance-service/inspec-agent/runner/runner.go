package runner

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net"
	neturl "net/url"
	"os"
	"strings"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/jsonpb"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"sort"

	"github.com/chef/automate/api/external/secrets"
	ingest_events_compliance_api "github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	ingest_inspec "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/remote"
	"github.com/chef/automate/components/compliance-service/inspec-agent/resolver"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/cereal"
)

var ListenPort int = 2133

// ControlResultsLimit used for configuring inspec exec command, passed in via config flag
var ControlResultsLimit int = 50

// RunTimeLimit used for report post processing to reduce the size of the report.
var RunTimeLimit float32 = 1.0

// Set from compliance.go to call the ElasticSearch backend
var ESClient *ingestic.ESClient

var (
	ScanJobWorkflowName = cereal.NewWorkflowName("scan-job-workflow")

	CreateChildTaskName    = cereal.NewTaskName("create-child")
	ResolveJobTaskName     = cereal.NewTaskName("resolve-job")
	ScanJobTaskName        = cereal.NewTaskName("scan-job")
	ScanJobSummaryTaskName = cereal.NewTaskName("scan-job-summary")
)

func InitCerealManager(m *cereal.Manager, workerCount int, ingestClient ingest.ComplianceIngesterServiceClient,
	scanner *scanner.Scanner, resolver *resolver.Resolver, remoteInspecVersion string) error {
	err := m.RegisterWorkflowExecutor(ScanJobWorkflowName, &ScanJobWorkflow{})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor(CreateChildTaskName, &CreateChildTask{
		scanner,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor(ResolveJobTaskName, &ResolveTask{
		remoteInspecVersion,
		scanner,
		resolver,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor(ScanJobTaskName, &InspecJobTask{
		ingestClient,
		scanner,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
	if err != nil {
		return err
	}

	return m.RegisterTaskExecutor(ScanJobSummaryTaskName, &InspecJobSummaryTask{
		scanner,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
}

type LostJob struct {
	JobID  string
	NodeID string
}
type ScanJobWorkflow struct{}
type ScanJobWorkflowPayload struct {
	OutstandingJobs  int
	ParentJobID      string
	ChildJobID       string
	OverallJobStatus string
	LostJobs         []LostJob
	StartTime        time.Time
}

func (p *ScanJobWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	var job jobs.Job
	err := w.GetParameters(&job)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal job from parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	taskName := CreateChildTaskName
	if job.Recurrence == "" {
		// If we don't have a recurrence, it means this job isn't a
		// scheduled job and thus doesn't need a child job created in
		// the database.
		taskName = ResolveJobTaskName
	}
	err = w.EnqueueTask(taskName, job)
	if err != nil {
		err = errors.Wrapf(err, "failed to enqueue task %q", taskName)
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	return w.Continue(&ScanJobWorkflowPayload{
		OutstandingJobs:  0,
		ParentJobID:      job.Id,
		ChildJobID:       "",
		OverallJobStatus: types.StatusRunning,
		LostJobs:         nil,
		StartTime:        time.Now(),
	})
}

func (p *ScanJobWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload ScanJobWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal scan-job payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("Entered ScanJobWorkflow > OnTaskComplete with payload %+v", payload)
	switch ev.TaskName {
	case CreateChildTaskName:
		if ev.Result.Err() != nil {
			logrus.WithError(ev.Result.Err()).Error("create-child failed with error")
			return w.Fail(ev.Result.Err())
		}

		var childJob jobs.Job
		err := ev.Result.Get(&childJob)
		if err != nil {
			err = errors.Wrap(err, "failed to unmarshal child job")
			logrus.WithError(err).Error()
			return w.Fail(err)
		}

		payload.ChildJobID = childJob.Id

		err = w.EnqueueTask(ResolveJobTaskName, childJob)
		if err != nil {
			err = errors.Wrap(err, "failed to enqueue resolve-job task")
			logrus.WithError(err)
			return w.Fail(err)
		}

		return w.Continue(&payload)
	case ResolveJobTaskName:
		if ev.Result.Err() != nil {
			logrus.WithError(ev.Result.Err()).Error("resolve-job failed with error")
			return w.Fail(ev.Result.Err())
		}

		jobs := []*types.InspecJob{}
		err := ev.Result.Get(&jobs)
		if err != nil {
			err = errors.Wrap(err, "failed to unmarshal resolved jobs")
			logrus.WithError(err).Error("")
			return w.Fail(err)
		}
		logrus.Debugf("resolve-job returned %d job(s)", len(jobs))

		if len(jobs) == 0 {
			return w.Complete()
		}

		for _, job := range jobs {
			if payload.ChildJobID != "" {
				logrus.Debugf("Enqueueing individual scan job %s for %s (child of %s)", job.JobID, payload.ChildJobID, payload.ParentJobID)
			} else {
				logrus.Debugf("Enqueueing individual scan job %s for %s", job.JobID, payload.ParentJobID)
			}
			err = w.EnqueueTask(ScanJobTaskName, job)
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue scan-job task")
				logrus.WithError(err)
				return w.Fail(err)
			}
		}

		payload.OutstandingJobs = len(jobs)
		return w.Continue(&payload)
	case ScanJobTaskName:
		payload.OutstandingJobs--

		var childJobStatus string
		var job *types.InspecJob
		jobName := "unknown"
		jobID := "unknown"
		nodeID := "unknown"

		if err := ev.Result.GetParameters(&job); err != nil {
			logrus.WithError(err).Warn("could not read scan job parameters")
		}

		if job != nil {
			jobID = job.JobID
			jobName = job.JobName
			nodeID = job.NodeID
		}

		logctx := logrus.WithFields(logrus.Fields{
			"jobID":   jobID,
			"jobName": jobName,
			"nodeID":  nodeID,
		})
		if err := ev.Result.Err(); err != nil {
			logctx.WithError(ev.Result.Err()).Error("scan-job failed with abnormal error")
			childJobStatus = types.StatusFailed
			if err == cereal.ErrTaskLost && jobID != "" {
				payload.LostJobs = append(payload.LostJobs, LostJob{
					JobID:  jobID,
					NodeID: nodeID,
				})
			}
		} else {
			if err := ev.Result.Get(&childJobStatus); err != nil {
				logctx.WithError(err).Error("could not decode scan-job result, marking as failed")
				childJobStatus = types.StatusFailed
			}
		}

		logctx.Debugf("ScanJobWorkflow > OnTaskComplete with %d outstanding jobs and childJobStatus of %s", payload.OutstandingJobs, childJobStatus)
		switch childJobStatus {
		case types.StatusFailed:
			payload.OverallJobStatus = types.StatusFailed
		case types.StatusAborted:
			if payload.OverallJobStatus != types.StatusFailed {
				payload.OverallJobStatus = types.StatusAborted
			}
		}

		if payload.OutstandingJobs <= 0 {
			// No more jobs left to run, if status hasn't changed from the initial Running, we change it to Completed
			if payload.OverallJobStatus == types.StatusRunning {
				payload.OverallJobStatus = types.StatusCompleted
			}
			err := w.EnqueueTask(ScanJobSummaryTaskName, payload)
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue scan-job-summary task")
				logctx.WithError(err).Error("failed to enqueue scan-job-summary")
				return w.Fail(err)
			}
		}
		return w.Continue(&payload)
	case ScanJobSummaryTaskName:
		// We only want to complete after processing the summary task
		// This task is designed to conclude the overall status of a job that is resolved in child jobs
		return w.Complete()
	}

	return w.Continue(&payload)
}

func (s *ScanJobWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ScanJobWorkflow got OnCancel")
	return w.Complete()
}

type CreateChildTask struct {
	scanner *scanner.Scanner
}

type InspecJobTask struct {
	ingestClient  ingest.ComplianceIngesterServiceClient
	scannerServer *scanner.Scanner
}

type InspecJobSummaryTask struct {
	scannerServer *scanner.Scanner
}

type ResolveTask struct {
	remoteInspecVersion string
	scanner             *scanner.Scanner
	resolver            *resolver.Resolver
}

func (t *ResolveTask) handleEmptyNodeJobs(ctx context.Context, job *jobs.Job) error {
	now := time.Now()
	nodeJob := &types.InspecJob{
		InspecBaseJob: types.InspecBaseJob{
			JobID: job.Id,
		},
		StartTime:  &now,
		EndTime:    &now,
		NodeStatus: types.StatusAborted,
	}
	err := t.scanner.UpdateJobStatus(nodeJob.JobID, "failed", nodeJob.StartTime, nodeJob.EndTime)
	if err != nil {
		logrus.Errorf("error updating job status for job %s (%s): %s", job.Name, job.Id, err.Error())
	}
	err = t.scanner.UpdateResult(ctx, nodeJob, nil, &inspec.Error{Message: "no nodes found"}, "")
	if err != nil {
		logrus.Errorf("error updating job results for job %s (%s): %s", job.Name, job.Id, err.Error())
	}
	return errors.New("No nodes found for job")
}

func (t *ResolveTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job jobs.Job
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal job to resolve")
	}

	nodeJobs, err := t.resolver.ResolveJob(ctx, &job)
	if err != nil {
		// TODO(ssd) 2019-06-07: As far as I can tell
		// the current code just returns, but it seems
		// like we should update the job status with
		// this failure?
		return nil, errors.Wrapf(err, "Failed to resolve job %s", job.Id)
	}

	if len(nodeJobs) == 0 {
		return nil, t.handleEmptyNodeJobs(ctx, &job)
	}

	for _, job := range nodeJobs {
		if job == nil {
			return nil, errors.New("nil job returned from ResolveJob")
		}

		job.Status = types.StatusScheduled
		job.NodeStatus = types.StatusScheduled
		if job.SSM {
			job.RemoteInspecVersion = t.remoteInspecVersion
		}

		err := t.scanner.UpdateJobStatus(job.JobID, job.Status, nil, nil)
		if err != nil {
			logrus.Errorf("error updating status for job %s (%s): %s", job.JobName, job.JobID, err.Error())
		}
		job.InternalProfiles, job.ProfilesOwner = updateComplianceURLs(job.Profiles)
	}

	return nodeJobs, nil
}

type jobCompletionInfo struct {
	InspecErr  *inspec.Error
	ExecInfo   []byte
	DetectInfo *inspec.OSInfo
	ReportID   string
}

func (t *InspecJobTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job types.InspecJob
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal inspec job")
	}

	logrus.Debugf("working on job %s (%s) for node %s (%s)", job.JobName, job.JobID, job.NodeName, job.NodeID)

	if !t.validateJob(&job) {
		return types.StatusAborted, nil
	}

	if job.Retries > 0 && job.RetriesLeft == 0 {
		job.RetriesLeft = job.Retries
	}

	job.StartTime = timeNowRef()
	job.NodeStatus = types.StatusRunning

	err := t.scannerServer.UpdateJobStatus(job.JobID, job.NodeStatus, job.StartTime, nil)
	if err != nil {
		logrus.Errorf("error trying to update job status during scan job %s: %s", job.JobName, err.Error())
	}

	currentJobSummary := job.JobType + " " + job.TargetConfig.Backend + " " + job.TargetConfig.Hostname

	if job.JobType != types.JobTypeDetect && job.JobType != types.JobTypeExec {
		return types.StatusFailed, errors.Errorf("Invalid job type %q", job.JobType)
	}

	var jobInfo jobCompletionInfo
	// retrying for certain error types
	job.RetriesLeft++ // adding the implicit try
	for job.RetriesLeft > 0 && job.NodeStatus == types.StatusRunning {
		if job.SSM {
			switch job.JobType {
			case types.JobTypeDetect:
				// ssm ping is online, so we set node to reachable by setting node status to completed
				// this is b/c we don't actually run an inspec detect, b/c ssm jobs need to report back to automate, and
				// detect doesn't do this. so here we do nothing
				job.NodeStatus = types.StatusCompleted
			case types.JobTypeExec:
				// call out to do the ssm job
				jobInfo.InspecErr = remote.RunSSMJob(ctx, &job)
			}
		} else if nodeHasSecrets(&job.TargetConfig) {
			switch job.JobType {
			case types.JobTypeDetect:
				jobInfo.DetectInfo, jobInfo.InspecErr = doDetect(&job)
			case types.JobTypeExec:
				jobInfo.ExecInfo, jobInfo.InspecErr = doExec(&job)
			}
		} else {
			job.NodeStatus = types.StatusFailed
			jobInfo.InspecErr = inspec.NewInspecError(inspec.NO_CREDS_PROVIDED, "insufficient information for ssh or winrm scan")
		}
		job.RetriesLeft--
		if job.NodeStatus == types.StatusRunning &&
			(jobInfo.InspecErr.Type == inspec.CONN_TIMEOUT || jobInfo.InspecErr.Type == inspec.UNREACHABLE_HOST) &&
			job.RetriesLeft > 0 {
			logrus.Debugf("retrying(%d) job %s(%s) for node %s", job.RetriesLeft, job.JobID, currentJobSummary, job.NodeID)
		}
	}

	cleanupKeys(job.TargetConfig.KeyFiles)
	logrus.Debugf("job '%s' finished", job.JobID)
	if job.NodeStatus == types.StatusRunning {
		job.NodeStatus = types.StatusFailed
	}

	job.EndTime = timeNowRef()

	return t.handleCompletedJob(ctx, job, jobInfo)
}

func (t *InspecJobTask) handleCompletedJob(ctx context.Context, job types.InspecJob, jobInfo jobCompletionInfo) (string, error) {
	if job.SSM {
		switch job.JobType {
		case types.JobTypeDetect:
			// ssm ping is online, so we set node to reachable up above by setting node status to completed
			// but we don't actually run an inspec detect, b/c ssm jobs need to report back to automate, and
			// detect doesn't do this. so here we do nothing. we'll update the node further down.
			jobInfo.DetectInfo = &inspec.OSInfo{}
		case types.JobTypeExec:
			// ssm jobs report directly to automate, so we attached a report id to the
			// reporter config when we assembled the script
			err := t.scannerServer.UpdateResult(ctx, &job, nil, jobInfo.InspecErr, job.Reporter.ReportUUID)
			if err != nil {
				logrus.Errorf("error trying to update node '%s' (%s) during scan job '%s' with job results: %s", job.NodeName, job.NodeID, job.JobName, err.Error())
			}
		default:
			return types.StatusFailed, errors.Errorf("unknown job type: %s", job.JobType)
		}
	} else {
		switch job.JobType {
		case types.JobTypeDetect:
			detectInfoByte, err := json.Marshal(jobInfo.DetectInfo)
			if err != nil {
				logrus.Errorf("error trying to marshal detectInfo for job '%s'", job.JobID)
				job.NodeStatus = types.StatusFailed
				jobInfo.InspecErr = inspec.NewInspecError(inspec.INVALID_OUTPUT, err.Error())
			}
			err = t.scannerServer.UpdateResult(ctx, &job, detectInfoByte, jobInfo.InspecErr, "")
			if err != nil {
				logrus.Errorf("error trying to update node '%s' (%s) during scan job '%s' with job results: %s", job.NodeName, job.NodeID, job.JobName, err.Error())
			}
		case types.JobTypeExec:
			if job.NodeStatus == types.StatusCompleted {
				jobInfo.ReportID = uuid.Must(uuid.NewV4()).String()
				err := t.reportIt(ctx, &job, jobInfo.ExecInfo, jobInfo.ReportID)
				if err != nil {
					logrus.Errorf("worker error reporting on node '%s' (%s) during scan job '%s': %s", job.NodeName, job.NodeID, job.JobName, err.Error())
					job.NodeStatus = types.StatusFailed
					jobInfo.InspecErr = inspec.NewInspecError(inspec.INVALID_OUTPUT, err.Error())
				}
			}
			err := t.scannerServer.UpdateResult(ctx, &job, nil, jobInfo.InspecErr, jobInfo.ReportID)
			if err != nil {
				logrus.Errorf("error trying to update node '%s' (%s) during scan job '%s' with job results: %s", job.NodeName, job.NodeID, job.JobName, err.Error())
			}
		default:
			return types.StatusFailed, errors.Errorf("unknown job type: %+v", job.JobType)
		}
	}
	err := t.scannerServer.UpdateNode(ctx, &job, jobInfo.DetectInfo)
	if err != nil {
		logrus.Errorf("error trying to update node '%s' (%s) during scan job '%s': %s", job.NodeName, job.NodeID, job.JobName, err.Error())
	}
	logrus.Debugf("finished job '%s' (%s) with status '%s'", job.JobName, job.JobID, job.NodeStatus)
	return job.NodeStatus, nil
}

func (t *InspecJobTask) reportIt(ctx context.Context, job *types.InspecJob, content []byte, reportID string) error {
	var report ingest_events_compliance_api.Report
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	if err := unmarshaler.Unmarshal(bytes.NewReader(content), &report); err != nil {
		return errors.Wrap(err, "reportIt was unable to unmarshal the report output into a compliance.Report struct")
	}

	report.Environment = job.InspecBaseJob.NodeEnv
	if report.Environment == "" {
		report.Environment = "unknown"
	}
	report.Type = mappings.DocType
	report.NodeName = job.InspecBaseJob.NodeName
	report.NodeUuid = job.InspecBaseJob.NodeID
	report.ReportUuid = reportID
	report.JobUuid = job.JobID
	report.EndTime = time.Now().UTC().Format(time.RFC3339)
	report.SourceId = job.SourceID
	report.SourceRegion = job.TargetConfig.TargetBaseConfig.Region
	report.SourceAccountId = job.SourceAccountID
	report.AutomateManagerId = job.ManagerID
	report.AutomateManagerType = job.ManagerType
	ipAddress := net.ParseIP(job.TargetConfig.TargetBaseConfig.Hostname)
	if ipAddress != nil {
		report.Ipaddress = ipAddress.String()
	} else {
		report.Fqdn = job.TargetConfig.TargetBaseConfig.Hostname
	}
	report.Tags = job.Tags
	logrus.Debugf("hand-over report to ingest service")

	removeResults(reportID, report.Profiles, ControlResultsLimit)

	allReportProfileIds := make([]string, len(report.Profiles))
	for i, profile := range report.Profiles {
		allReportProfileIds[i] = profile.Sha256
	}
	esProfilesMissingMeta, err := ESClient.ProfilesMissing(allReportProfileIds)
	if err != nil {
		return errors.Wrap(err, "Report profiles processing error")
	}
	profilesMissingMeta := make(map[string]struct{}, 0)
	for _, profileId := range esProfilesMissingMeta {
		profilesMissingMeta[profileId] = struct{}{}
	}
	stripProfilesMetadata(&report, profilesMissingMeta, RunTimeLimit)

	_, err = t.ingestClient.ProcessComplianceReport(ctx, &report)
	if err != nil {
		return errors.Wrap(err, "Report processing error")
	}
	return nil
}

func (t *InspecJobTask) validateJob(job *types.InspecJob) bool {
	if job == nil {
		logrus.Error("jobs.work: job cannot be nil, skipping")
		return false
	}
	deleted, err := t.scannerServer.IsJobDeleted(job.JobID)
	if err != nil {
		// keep on going if we err here.  no reason to block on validating job existence
		logrus.Errorf("inspec agent worker unable to validate job existence: %+v", err)
	}
	if deleted {
		logrus.Infof("aborting job. job id %s has been marked for deletion", job.JobID)
		return false
	}
	return true
}

func (t *InspecJobSummaryTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var jobsPayload ScanJobWorkflowPayload

	if err := task.GetParameters(&jobsPayload); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal summary job parameters")
	}

	for _, job := range jobsPayload.LostJobs {
		endTime := time.Now()
		startTime := jobsPayload.StartTime
		if startTime.IsZero() {
			startTime = endTime
		}
		job := &types.InspecJob{
			InspecBaseJob: types.InspecBaseJob{
				JobID:  job.JobID,
				NodeID: job.NodeID,
			},
			StartTime:  &startTime,
			EndTime:    &endTime,
			NodeStatus: types.StatusFailed,
		}
		err := t.scannerServer.UpdateResult(context.TODO(), job, nil, &inspec.Error{Message: "job lost likely due to process restart"}, "")
		if err != nil {
			logrus.Errorf("error trying to update node %s (%s) during scan job %s: %s", job.NodeName, job.NodeID, job.JobName, err.Error())
		}
	}

	// If this is a /recurring/ job, then we will have a
	// ChildJobID and that is the one we want to update.
	if jobsPayload.ChildJobID != "" {
		logrus.Debugf("Updating job %s with overall status of %s", jobsPayload.ChildJobID, jobsPayload.OverallJobStatus)
		err := t.scannerServer.UpdateJobStatus(jobsPayload.ChildJobID, jobsPayload.OverallJobStatus, nil, timeNowRef())
		if err != nil {
			logrus.Errorf("error updating status for job %s : %s", jobsPayload.ChildJobID, err.Error())
		}
	} else {
		logrus.Debugf("Updating job %s with overall status of %s", jobsPayload.ParentJobID, jobsPayload.OverallJobStatus)
		err := t.scannerServer.UpdateJobStatus(jobsPayload.ParentJobID, jobsPayload.OverallJobStatus, nil, timeNowRef())
		if err != nil {
			logrus.Errorf("error updating status for job %s : %s", jobsPayload.ChildJobID, err.Error())
		}
	}

	return nil, nil
}

func (t *CreateChildTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job jobs.Job
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal parent job")
	}

	job.JobCount++
	err := t.scanner.UpdateParentJobSchedule(job.Id, job.JobCount, job.Recurrence, job.ScheduledTime)
	if err != nil {
		logrus.Errorf("error updating parent job schedule for job %s (%s) : %s", job.Name, job.Id, err.Error())
	}

	childJob, err := t.scanner.CreateChildJob(&job)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create child job")
	}

	return childJob, nil
}

func timeNowRef() *time.Time {
	tn := time.Now().UTC().Round(time.Second)
	return &tn
}

func nodeHasSecrets(tc *inspec.TargetConfig) bool {
	switch tc.Backend {
	case "ssh", "winrm":
		return secretInfoExists(tc)
	}
	return true
}

func secretInfoExists(tc *inspec.TargetConfig) bool {
	if len(tc.SecretsArr) > 0 {
		return true
	}
	if len(tc.User) == 0 {
		return false
	}
	if len(tc.Password) == 0 {
		if len(tc.KeyFiles) == 0 {
			return false
		}
	}
	return true
}

func cloudEnvVars(tc *inspec.TargetConfig) (map[string]string, map[string]string, error) {
	envsMap := map[string]string{
		"CHEF_LICENSE": "accept-no-persist",
	}
	inputs := make(map[string]string)
	switch tc.Backend {
	case "aws":
		if tc.AwsUser == "" || tc.AwsPassword == "" {
			logrus.Debugf("no aws creds found in env vars, no aws creds found for node; attempting to use aws credential chain via inspec/train")
			return envsMap, inputs, nil
		}
		envsMap["AWS_ACCESS_KEY_ID"] = tc.AwsUser
		envsMap["AWS_SECRET_ACCESS_KEY"] = tc.AwsPassword

		// Only set a TOKEN ENV variable when one is needed.
		// Otherwise it prevents the TOKEN-less account credentials from working
		if tc.AwsSessionToken != "" {
			envsMap["AWS_SESSION_TOKEN"] = tc.AwsSessionToken
		}
		return envsMap, inputs, nil
	case "azure":
		if tc.AzureClientID == "" || tc.AzureClientSecret == "" || tc.AzureTenantID == "" {
			logrus.Debugf("no azure creds found in environment, no azure creds found for node; attempting to use azure credential chain via inspec/train")
			return envsMap, inputs, nil
		}
		envsMap["AZURE_CLIENT_ID"] = tc.AzureClientID
		envsMap["AZURE_CLIENT_SECRET"] = tc.AzureClientSecret
		envsMap["AZURE_TENANT_ID"] = tc.AzureTenantID
		envsMap["AZURE_SUBSCRIPTION_ID"] = tc.AzureSubscriptionID
		return envsMap, inputs, nil
	case "gcp":
		if tc.GcpCredsJson != "" {
			// GCP scans require an input of the gcp_project_id
			gcpCredStruct := secrets.GcpCredential{}
			err := json.Unmarshal([]byte(tc.GcpCredsJson), &gcpCredStruct)
			if err != nil {
				return envsMap, inputs, errors.Wrapf(err, "unable to find gcp_project_id")
			}
			inputs["gcp_project_id"] = gcpCredStruct.ProjectID

			// Specify "" for the temp dir as ioutil will pick TMPDIR or OS default
			tmpFile, err := ioutil.TempFile("", ".gcp-project-cred.json")
			if err != nil {
				return envsMap, inputs, err
			}

			err = ioutil.WriteFile(tmpFile.Name(), []byte(tc.GcpCredsJson), 0400)
			if err != nil {
				return envsMap, inputs, err
			}

			// Not consumed by InSpec via the json config stdin but via file on disk
			tc.GcpCredsJson = ""
			envsMap["GOOGLE_APPLICATION_CREDENTIALS"] = tmpFile.Name()

			return envsMap, inputs, nil
		} else {
			return envsMap, inputs, fmt.Errorf("cloudEnvVars: GcpCredsJson can't be empty, job will fail execution")
		}
	}

	return envsMap, inputs, nil
}

// doDetect executes a detect job and returns error type for retrying purposes
func doDetect(job *types.InspecJob) (osInfo *inspec.OSInfo, err *inspec.Error) {
	timeout := time.Duration(job.Timeout) * time.Second
	// inputs are not required here, as detect has no need for them
	env, _, genericErr := cloudEnvVars(&job.TargetConfig)
	defer func() {
		cleanupCreds(env)
	}()
	if genericErr != nil {
		return nil, inspec.NewInspecError(inspec.UNKNOWN_ERROR, genericErr.Error())
	}

	for i, tc := range potentialTargetConfigs(job) {
		osInfo, err = inspec.Detect(&tc, timeout, env)
		if err == nil {
			break
		}
		logrus.Errorf("%s(%s) connection attempt # %d failed for node %s(%s) with error: %s", job.JobType, job.JobID, i+1, job.NodeName, job.NodeID, err.Message)
	}
	if err != nil {
		return nil, err
	}

	job.NodeStatus = types.StatusCompleted
	return osInfo, nil
}

func doExec(job *types.InspecJob) (jsonBytes []byte, err *inspec.Error) {
	timeout := time.Duration(job.Timeout) * time.Second
	env, inputs, genericErr := cloudEnvVars(&job.TargetConfig)
	defer func() {
		cleanupCreds(env)
	}()
	if genericErr != nil {
		return nil, inspec.NewInspecError(inspec.UNKNOWN_ERROR, genericErr.Error())
	}

	for i, tc := range potentialTargetConfigs(job) {
		jsonBytes, _, err = inspec.Scan(job.InternalProfiles, &tc, timeout, env, inputs)
		if err == nil {
			break
		}
		logrus.Errorf("%s(%s) connection attempt # %d failed for node(%s) with error: %s", job.JobType, job.JobID, i+1, job.NodeID, err.Message)
	}
	if err != nil {
		return nil, err
	}

	job.NodeStatus = types.StatusCompleted
	return jsonBytes, nil
}

// The purpose of this function is to remove the results from a control once they
// exceed `limit`. This avoids large reports that cannot be ingested.
func removeResults(reportId string, profiles []*ingest_inspec.Profile, limit int) {
	for _, profile := range profiles {
		if profile.Controls != nil {
			for _, control := range profile.Controls {
				if control.Results != nil {
					if len(control.Results) > limit {
						logrus.Debugf("Control '%s' for report '%s' has %d results and will be trimmed based on the value (%d) of 'control-results-limit'", control.Id, reportId, len(control.Results), limit)
						// Sort results in this non-alphabetical order: failed, skipped, passed. This order is needed
						// to ensure the overall status of the control is still calculated correctly in ingestion
						sort.Slice(control.Results, func(i, j int) bool {
							return control.Results[i].Status == "failed" || (control.Results[i].Status == "skipped" && control.Results[j].Status == "passed")
						})
						chopped := ingest_inspec.RemovedResultsCounts{}
						for i := limit; i < len(control.Results); i++ {
							switch control.Results[i].Status {
							case "failed":
								chopped.Failed += 1
							case "skipped":
								chopped.Skipped += 1
							case "passed":
								chopped.Passed += 1
							}
						}
						control.RemovedResultsCounts = &chopped
						control.Results = control.Results[0:limit]
					}
				}
			}
		}
	}
}

// The purpose of this function is to remove the profiles metadata (control title, desc, code, etc)
// for profiles that already exist in the Automate profiles index.
func stripProfilesMetadata(report *ingest_events_compliance_api.Report, missingProfiles map[string]struct{}, runTimeLimit float32) {
	if report.Profiles == nil {
		return
	}
	for _, profile := range report.Profiles {
		// If the profile is not in the ones missing in the backend, we can proceed with the metadata removal
		if _, ok := missingProfiles[profile.Sha256]; ok {
			continue
		}
		// Profile 'Name' is a required property. By not sending it in the report, we make it clear to the ingestion backend that the profile metadata has been stripped from this profile in the report.
		// Profile 'title' and 'version' are still kept for troubleshooting purposes in the backend.
		profile.Name = ""
		profile.Groups = nil
		profile.CopyrightEmail = ""
		profile.Copyright = ""
		profile.Summary = ""
		profile.Supports = nil
		profile.License = ""
		profile.Maintainer = ""
		profile.Depends = nil
		if profile.Controls == nil {
			continue
		}
		for _, control := range profile.Controls {
			control.Code = ""
			control.Desc = ""
			// TODO: Add here control.Descriptions when the field makes it into the structs
			control.Impact = 0
			control.Refs = nil
			control.Tags = nil
			control.Title = ""
			control.SourceLocation = nil
			control.WaiverData = nil
			if control.Results == nil {
				continue
			}
			for _, result := range control.Results {
				if result.RunTime < runTimeLimit {
					result.RunTime = 0
					result.StartTime = ""
				}
			}
		}
		report.RunTimeLimit = runTimeLimit
	}
}

func potentialTargetConfigs(job *types.InspecJob) []inspec.TargetConfig {
	// GCP profile requires the project_id to be passed in as an attribute. Used the SubscriptionId to get this value from the database
	if job.TargetConfig.Backend == "gcp" {
		raw := (json.RawMessage)([]byte(fmt.Sprintf(`{"gcp_project_id":"%s"}`, job.TargetConfig.SubscriptionId)))
		job.TargetConfig.AttributesJson = &raw
		job.TargetConfig.SubscriptionId = ""
	}

	if len(job.TargetConfig.SecretsArr) > 0 {
		tcs := make([]inspec.TargetConfig, len(job.TargetConfig.SecretsArr))

		for i, secret := range job.TargetConfig.SecretsArr {
			tc := job.TargetConfig
			tc.User = secret.User
			tc.Password = secret.Password
			tc.KeyFiles = secret.KeyFiles
			tc.SecretsArr = nil
			tcs[i] = tc
		}

		return tcs
	} else {
		return []inspec.TargetConfig{job.TargetConfig}
	}
}

// Cleanup the ssh private keys stored on disk after they are not longer used
func cleanupKeys(keys []string) {
	logrus.Debugf("cleanupKeys deleting temporary private key files")
	for _, f := range keys {
		os.Remove(f) // nolint: errcheck
	}
}

// Cleanup the ssh private keys stored on disk after they are not longer used
func cleanupCreds(envs map[string]string) {
	logrus.Debugf("cleanupCreds deleting temporary cloud cred files")
	if envs["GOOGLE_APPLICATION_CREDENTIALS"] != "" {
		os.Remove(envs["GOOGLE_APPLICATION_CREDENTIALS"]) // nolint: errcheck
	}
}

// This allows inspec to use automate profiles without the `automate login` headache. Only works when the profile store is local
// A2: Replaces 'compliance://admin/apache-baseline#2.0.1' => 'http://127.0.0.1:2133/profiles/tar?owner=CiQwOGE4Njg0Yi1kYjg4LTRiNzMtOTBhOS0zY2QxNjYxZjU0NjYSBWxvY2Fs&name=apache-baseline&version=2.0.2'
// NOTE: THIS IS NOT COMPATIBLE WITH 1.x; we changed the url schema here
func updateComplianceURLs(urls []string) ([]string, string) {
	var newProfiles []string
	var owner string
	for _, url := range urls {
		if strings.HasPrefix(url, "compliance://") {
			profile := url[13:]
			ownerID := strings.SplitN(profile, "/", 2)
			if len(ownerID) < 2 {
				logrus.Errorf("no profile owner supplied")
				continue
			}
			idVersion := strings.SplitN(ownerID[1], "#", 2)
			owner = neturl.QueryEscape(ownerID[0])
			if len(idVersion) < 2 {
				logrus.Errorf("no profile version supplied")
				continue
			}
			name := neturl.QueryEscape(idVersion[0])
			version := neturl.QueryEscape(idVersion[1])
			url = fmt.Sprintf("http://127.0.0.1:%d/profiles/tar?owner=%s&name=%s&version=%s", ListenPort, owner, name, version)
			newProfiles = append(newProfiles, url)
		} else {
			newProfiles = append(newProfiles, url)
		}
	}
	return newProfiles, owner
}
