package runner

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/remote"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/workflow"
)

func InitWorkflowManager(w *workflow.WorkflowManager, workerCount int, ingestClient ingest.ComplianceIngesterClient,
	scannerServer *scanner.Scanner) {
	w.RegisterWorkflowExecutor("scan-job-workflow", &ScanJobWorkflow{})
	w.RegisterTaskExecutor("scan-job", &InspecJobTask{
		ingestClient,
		scannerServer,
	}, workflow.TaskExecutorOpts{Workers: workerCount})

	w.RegisterTaskExecutor("scan-job-summary", &InspecJobSummaryTask{
		ingestClient,
		scannerServer,
	}, workflow.TaskExecutorOpts{Workers: 1})
}

type ScanJobWorkflow struct{}
type ScanJobWorkflowPayload struct {
	OutstandingJobs int
	ParentJobID     string
	ParentJobStatus string
}

func (p *ScanJobWorkflow) OnStart(w workflow.WorkflowInstance,
	ev workflow.StartEvent) workflow.Decision {

	jobs := []*types.InspecJob{}
	err := w.GetParameters(&jobs)
	if err != nil {
		logrus.WithError(err).Error("Failed to unmarshal jobs!")
		return w.Complete()
	}

	logrus.Debugf("OnStart got %d job(s)", len(jobs))
	for _, job := range jobs {
		w.EnqueueTask("scan-job", job)
	}

	return w.Continue(&ScanJobWorkflowPayload{
		len(jobs),
		jobs[0].ParentJobID,
		types.StatusRunning,
	})
}

func (p *ScanJobWorkflow) OnTaskComplete(w workflow.WorkflowInstance,
	ev workflow.TaskCompleteEvent) workflow.Decision {

	var payload ScanJobWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Fatal("Could not decode payload")
	}

	logrus.Debugf("Entered ScanJobWorkflow > OnTaskComplete with payload %+v", payload)
	switch ev.TaskName {
	case "scan-job":
		payload.OutstandingJobs--

		var childJobStatus string
		if err := ev.Result.Get(&childJobStatus); err != nil {
			logrus.WithError(err).Error("Could not decode childJobStatus")
		}
		logrus.Debugf("ScanJobWorkflow > OnTaskComplete with %d outstanding jobs and childJobStatus of %s", payload.OutstandingJobs, childJobStatus)

		if childJobStatus == types.StatusFailed {
			payload.ParentJobStatus = types.StatusFailed
		}
		switch childJobStatus {
		case types.StatusFailed:
			payload.ParentJobStatus = types.StatusFailed
		case types.StatusAborted:
			if payload.ParentJobStatus != types.StatusFailed {
				payload.ParentJobStatus = types.StatusAborted
			}
		}

		if payload.OutstandingJobs > 0 {
			return w.Continue(&payload)
		} else {
			// No more jobs left to run, if status hasn't changed from the initial Running, we change it to Completed
			if payload.ParentJobStatus == types.StatusRunning {
				payload.ParentJobStatus = types.StatusCompleted
			}
			w.EnqueueTask("scan-job-summary", payload)
			return w.Continue(&payload)
		}
	case "scan-job-summary":
		// We only want to complete after processing the summary task
		// This task is designed to conclude the overall status of a job that is resolved in child jobs
		return w.Complete()
	}

	return w.Continue(&payload)
}

func (s *ScanJobWorkflow) OnCancel(w workflow.WorkflowInstance, ev workflow.CancelEvent) workflow.Decision {
	logrus.Debugf("ScanJobWorkflow got OnCancel")
	return w.Complete()
}

type InspecJobTask struct {
	ingestClient  ingest.ComplianceIngesterClient
	scannerServer *scanner.Scanner
}

type InspecJobSummaryTask struct {
	ingestClient  ingest.ComplianceIngesterClient
	scannerServer *scanner.Scanner
}

func (t *InspecJobTask) Run(ctx context.Context, task workflow.Task) (interface{}, error) {
	var job types.InspecJob
	if err := task.GetParameters(&job); err != nil {
		logrus.WithError(err).Error("could not unmarshal job parameters")
		return nil, err
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

func (t *InspecJobSummaryTask) Run(ctx context.Context, task workflow.Task) (interface{}, error) {
	var jobsPayload ScanJobWorkflowPayload

	if err := task.GetParameters(&jobsPayload); err != nil {
		logrus.WithError(err).Error("could not unmarshal summary job parameters")
		return nil, err
	}

	logrus.Debugf("Updating parent job %s with overall status of %s", jobsPayload.ParentJobID, jobsPayload.ParentJobStatus)
	t.scannerServer.UpdateJobStatus(jobsPayload.ParentJobID, jobsPayload.ParentJobStatus, nil, timeNowRef())
	return nil, nil
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
