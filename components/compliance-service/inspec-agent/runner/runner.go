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

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/remote"
	"github.com/chef/automate/components/compliance-service/inspec-agent/resolver"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/cereal"
)

var ListenPort int = 2133

func InitCerealManager(m *cereal.Manager, workerCount int, ingestClient ingest.ComplianceIngesterClient,
	scanner *scanner.Scanner, resolver *resolver.Resolver, remoteInspecVersion string) error {
	err := m.RegisterWorkflowExecutor("scan-job-workflow", &ScanJobWorkflow{})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor("create-child", &CreateChildTask{
		scanner,
	}, cereal.TaskExecutorOpts{Workers: 1})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor("resolve-job", &ResolveTask{
		remoteInspecVersion,
		scanner,
		resolver,
	}, cereal.TaskExecutorOpts{Workers: 1})
	if err != nil {
		return err
	}

	err = m.RegisterTaskExecutor("scan-job", &InspecJobTask{
		ingestClient,
		scanner,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
	if err != nil {
		return err
	}

	return m.RegisterTaskExecutor("scan-job-summary", &InspecJobSummaryTask{
		scanner,
	}, cereal.TaskExecutorOpts{Workers: 1})
}

type ScanJobWorkflow struct{}
type ScanJobWorkflowPayload struct {
	OutstandingJobs  int
	ParentJobID      string
	ChildJobID       string
	OverallJobStatus string
	LostJobIDs       []string
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

	taskName := "create-child"
	if job.Recurrence == "" {
		// If we don't have a recurrence, it means this job isn't a
		// scheduled job and thus doesn't need a child job created in
		// the database.
		taskName = "resolve-job"
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
		LostJobIDs:       nil,
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
	case "create-child":
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

		err = w.EnqueueTask("resolve-job", childJob)
		if err != nil {
			err = errors.Wrap(err, "failed to enqueue resolve-job task")
			logrus.WithError(err)
			return w.Fail(err)
		}

		return w.Continue(&payload)
	case "resolve-job":
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
			err = w.EnqueueTask("scan-job", job)
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue scan-job task")
				logrus.WithError(err)
				return w.Fail(err)
			}
		}

		payload.OutstandingJobs = len(jobs)
		return w.Continue(&payload)
	case "scan-job":
		payload.OutstandingJobs--

		var childJobStatus string
		var job *types.InspecJob
		jobName := "unknown"
		jobID := "unknown"

		if err := ev.Result.GetParameters(&job); err != nil {
			logrus.WithError(err).Warn("could not read scan job parameters")
		}

		if job != nil {
			jobID = job.JobID
			jobName = job.JobName
		}

		logctx := logrus.WithFields(logrus.Fields{
			"jobID":   jobID,
			"jobName": jobName,
		})
		if err := ev.Result.Err(); err != nil {
			logctx.WithError(ev.Result.Err()).Error("scan-job failed with abnormal error")
			childJobStatus = types.StatusFailed
			if err == cereal.ErrTaskLost && jobID != "" {
				payload.LostJobIDs = append(payload.LostJobIDs, jobID)
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
			err := w.EnqueueTask("scan-job-summary", payload)
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue scan-job-summary task")
				logctx.WithError(err).Error("failed to enqueue scan-job-summary")
				return w.Fail(err)
			}
		}
		return w.Continue(&payload)
	case "scan-job-summary":
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
	ingestClient  ingest.ComplianceIngesterClient
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
		now := time.Now()
		nodeJob := &types.InspecJob{
			InspecBaseJob: types.InspecBaseJob{
				JobID: job.Id,
			},
			StartTime:  &now,
			EndTime:    &now,
			NodeStatus: types.StatusAborted,
		}
		t.scanner.UpdateJobStatus(nodeJob.JobID, "failed", nodeJob.StartTime, nodeJob.EndTime)
		t.scanner.UpdateResult(context.TODO(), nodeJob, nil, &inspec.Error{Message: "no nodes found"}, "")
		return nil, errors.New("No nodes found for job")
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

		t.scanner.UpdateJobStatus(job.JobID, job.Status, nil, nil)
		job.InternalProfiles, job.ProfilesOwner = updateComplianceURLs(job.Profiles)
	}

	return nodeJobs, nil
}

func (t *InspecJobTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job types.InspecJob
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal inspec job")
	}

	logrus.Debugf("working on job %s for node %s", job.JobID, job.NodeID)

	if !t.validateJob(&job) {
		return types.StatusAborted, nil
	}

	if job.Retries > 0 && job.RetriesLeft == 0 {
		job.RetriesLeft = job.Retries
	}

	job.StartTime = timeNowRef()
	job.NodeStatus = types.StatusRunning

	t.scannerServer.UpdateJobStatus(job.JobID, job.NodeStatus, job.StartTime, nil)

	currentJobSummary := job.JobType + " " + job.TargetConfig.Backend + " " + job.TargetConfig.Hostname

	if job.JobType != types.JobTypeDetect && job.JobType != types.JobTypeExec {
		return types.StatusFailed, errors.Errorf("Invalid job type %q", job.JobType)
	}

	var inspecErr *inspec.Error
	var execInfo []byte
	var detectInfo *inspec.OSInfo
	var reportID string
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
				inspecErr = remote.RunSSMJob(ctx, &job)
			}
		} else if nodeHasSecrets(&job.TargetConfig) {
			switch job.JobType {
			case types.JobTypeDetect:
				detectInfo, inspecErr = doDetect(&job)
			case types.JobTypeExec:
				execInfo, inspecErr = doExec(&job)
			}
		} else {
			job.NodeStatus = types.StatusFailed
			inspecErr = inspec.NewInspecError(inspec.NO_CREDS_PROVIDED, "insufficient information for ssh or winrm scan")
		}
		job.RetriesLeft--
		if job.NodeStatus == types.StatusRunning &&
			(inspecErr.Type == inspec.CONN_TIMEOUT || inspecErr.Type == inspec.UNREACHABLE_HOST) &&
			job.RetriesLeft > 0 {
			logrus.Debugf("retrying(%d) job %s(%s) for node %s", job.RetriesLeft, job.JobID, currentJobSummary, job.NodeID)
		}
	}

	cleanupKeys(job.TargetConfig.KeyFiles)
	logrus.Debugf("job %s finished", job.JobID)

	if job.NodeStatus == types.StatusRunning {
		job.NodeStatus = types.StatusFailed
	}

	job.EndTime = timeNowRef()

	if job.SSM {
		switch job.JobType {
		case types.JobTypeDetect:
			// ssm ping is online, so we set node to reachable up above by setting node status to completed
			// but we don't actually run an inspec detect, b/c ssm jobs need to report back to automate, and
			// detect doesn't do this. so here we do nothing. we'll update the node further down.
			detectInfo = &inspec.OSInfo{}
		case types.JobTypeExec:
			// ssm jobs report directly to automate, so we attached a report id to the
			// reporter config when we assembled the script
			t.scannerServer.UpdateResult(ctx, &job, nil, inspecErr, job.Reporter.ReportUUID)
		default:
			return types.StatusFailed, errors.Errorf("unknown job type: %s", job.JobType)
		}
	} else {
		switch job.JobType {
		case types.JobTypeDetect:
			detectInfoByte, err := json.Marshal(detectInfo)
			if err != nil {
				logrus.Errorf("error trying to marshal detectInfo for job %s", job.JobID)
				job.NodeStatus = types.StatusFailed
				inspecErr = inspec.NewInspecError(inspec.INVALID_OUTPUT, err.Error())
			}
			t.scannerServer.UpdateResult(ctx, &job, detectInfoByte, inspecErr, "")
		case types.JobTypeExec:
			if job.NodeStatus == types.StatusCompleted {
				reportID = uuid.Must(uuid.NewV4()).String()
				err := t.reportIt(ctx, &job, execInfo, reportID)
				if err != nil {
					logrus.Errorf("worker error: %s", err)
					job.NodeStatus = types.StatusFailed
					inspecErr = inspec.NewInspecError(inspec.INVALID_OUTPUT, err.Error())
				}
			}
			t.scannerServer.UpdateResult(ctx, &job, nil, inspecErr, reportID)
		default:
			return types.StatusFailed, errors.Errorf("unknown job type: %+v", job.JobType)
		}
	}
	t.scannerServer.UpdateNode(ctx, &job, detectInfo)
	logrus.Debugf("finished job %s with status %s", job.JobID, job.NodeStatus)
	return job.NodeStatus, nil
}

func (t *InspecJobTask) reportIt(ctx context.Context, job *types.InspecJob, content []byte, reportID string) error {
	var report compliance.Report
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
	ipAddress := net.ParseIP(job.TargetConfig.TargetBaseConfig.Hostname)
	if ipAddress != nil {
		report.Ipaddress = ipAddress.String()
	} else {
		report.Fqdn = job.TargetConfig.TargetBaseConfig.Hostname
	}
	report.Tags = job.Tags
	logrus.Debugf("hand-over report to ingest service")

	_, err := t.ingestClient.ProcessComplianceReport(ctx, &report)
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

	for _, jobID := range jobsPayload.LostJobIDs {
		endTime := time.Now()
		startTime := jobsPayload.StartTime
		if startTime.IsZero() {
			startTime = endTime
		}
		job := &types.InspecJob{
			InspecBaseJob: types.InspecBaseJob{
				JobID: jobID,
			},
			StartTime:  &startTime,
			EndTime:    &endTime,
			NodeStatus: types.StatusFailed,
		}
		t.scannerServer.UpdateResult(context.TODO(), job, nil, &inspec.Error{Message: "job lost likely due to process restart"}, "")
	}

	// If this is a /recurring/ job, then we will have a
	// ChildJobID and that is the one we want to update.
	if jobsPayload.ChildJobID != "" {
		logrus.Debugf("Updating job %s with overall status of %s", jobsPayload.ChildJobID, jobsPayload.OverallJobStatus)
		t.scannerServer.UpdateJobStatus(jobsPayload.ChildJobID, jobsPayload.OverallJobStatus, nil, timeNowRef())
	} else {
		logrus.Debugf("Updating job %s with overall status of %s", jobsPayload.ParentJobID, jobsPayload.OverallJobStatus)
		t.scannerServer.UpdateJobStatus(jobsPayload.ParentJobID, jobsPayload.OverallJobStatus, nil, timeNowRef())
	}

	return nil, nil
}

func (t *CreateChildTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job jobs.Job
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal parent job")
	}

	job.JobCount++
	t.scanner.UpdateParentJobSchedule(job.Id, job.JobCount, job.Recurrence, job.ScheduledTime)

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

func cloudEnvVars(tc *inspec.TargetConfig) (map[string]string, error) {
	envsMap := map[string]string{
		"CHEF_LICENSE": "accept-no-persist",
	}
	switch tc.Backend {
	case "aws":
		if tc.AwsUser == "" || tc.AwsPassword == "" {
			logrus.Debugf("no aws creds found in env vars, no aws creds found for node; attempting to use aws credential chain via inspec/train")
			return envsMap, nil
		}
		envsMap["AWS_ACCESS_KEY_ID"] = tc.AwsUser
		envsMap["AWS_SECRET_ACCESS_KEY"] = tc.AwsPassword

		// Only set a TOKEN ENV variable when one is needed.
		// Otherwise it prevents the TOKEN-less account credentials from working
		if tc.AwsSessionToken != "" {
			envsMap["AWS_SESSION_TOKEN"] = tc.AwsSessionToken
		}
		return envsMap, nil
	case "azure":
		if tc.AzureClientID == "" || tc.AzureClientSecret == "" || tc.AzureTenantID == "" {
			logrus.Debugf("no azure creds found in environment, no azure creds found for node; attempting to use azure credential chain via inspec/train")
			return envsMap, nil
		}
		envsMap["AZURE_CLIENT_ID"] = tc.AzureClientID
		envsMap["AZURE_CLIENT_SECRET"] = tc.AzureClientSecret
		envsMap["AZURE_TENANT_ID"] = tc.AzureTenantID
		return envsMap, nil
	case "gcp":
		if tc.GcpCredsJson != "" {
			// Specify "" for the temp dir as ioutil will pick TMPDIR or OS default
			tmpFile, err := ioutil.TempFile("", ".gcp-project-cred.json")
			if err != nil {
				return envsMap, err
			}

			err = ioutil.WriteFile(tmpFile.Name(), []byte(tc.GcpCredsJson), 0400)
			if err != nil {
				return envsMap, err
			}

			// Not consumed by InSpec via the json config stdin but via file on disk
			tc.GcpCredsJson = ""
			envsMap["GOOGLE_APPLICATION_CREDENTIALS"] = tmpFile.Name()
			return envsMap, nil
		} else {
			return envsMap, fmt.Errorf("cloudEnvVars: GcpCredsJson can't be empty, job will fail execution")
		}
	}

	return envsMap, nil
}

// doDetect executes a detect job and returns error type for retrying purposes
func doDetect(job *types.InspecJob) (osInfo *inspec.OSInfo, err *inspec.Error) {
	timeout := time.Duration(job.Timeout) * time.Second
	env, genericErr := cloudEnvVars(&job.TargetConfig)
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
		logrus.Errorf("%s(%s) connection attempt # %d failed for node(%s) with error: %s", job.JobType, job.JobID, i+1, job.NodeID, err.Message)
	}
	if err != nil {
		return nil, err
	}

	job.NodeStatus = types.StatusCompleted
	return osInfo, nil
}

func doExec(job *types.InspecJob) (jsonBytes []byte, err *inspec.Error) {
	timeout := time.Duration(job.Timeout) * time.Second
	env, genericErr := cloudEnvVars(&job.TargetConfig)
	defer func() {
		cleanupCreds(env)
	}()
	if genericErr != nil {
		return nil, inspec.NewInspecError(inspec.UNKNOWN_ERROR, genericErr.Error())
	}

	for i, tc := range potentialTargetConfigs(job) {
		jsonBytes, _, err = inspec.Scan(job.InternalProfiles, &tc, timeout, env)
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
