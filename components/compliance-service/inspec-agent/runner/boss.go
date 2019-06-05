package runner

import (
	"context"
	"sync"
	"time"

	"strings"

	"fmt"
	neturl "net/url"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/lib/workflow"
)

var maxWorkers int
var jobsChan chan *types.InspecJob
var jobRunningMap jobRunningStore
var jobStatusMap jobStatusStore

var ListenPort int = 2133

type Runner struct {
	managerClient       manager.NodeManagerServiceClient
	nodesClient         nodes.NodesServiceClient
	db                  *pgdb.DB
	scannerServer       *scanner.Scanner
	ingestClient        ingest.ComplianceIngesterClient
	remoteInspecVersion string
	workflowManager     *workflow.WorkflowManager
}

func New(managerClient manager.NodeManagerServiceClient, nodesClient nodes.NodesServiceClient, db *pgdb.DB,
	ingestClient ingest.ComplianceIngesterClient, remoteInspecVersion string, workflowManager *workflow.WorkflowManager) *Runner {
	scannerServer := scanner.New(managerClient, nodesClient, db)
	go watchJobsNodesStatus(scannerServer)
	return &Runner{managerClient, nodesClient, db, scannerServer, ingestClient, remoteInspecVersion, workflowManager}
}

type jobStatusStore struct {
	*sync.Map
}

type jobRunningStore struct {
	*sync.Map
}

func NewJobStatusStore() jobStatusStore {
	return jobStatusStore{&sync.Map{}}
}

func NewJobRunningMap() jobRunningStore {
	return jobRunningStore{&sync.Map{}}
}

func (s *jobStatusStore) Set(key string, value map[string]*string) {
	s.Store(key, value)
}

func (s *jobStatusStore) Remove(key string) {
	s.Delete(key)
}

func (s *jobRunningStore) Set(key string, value bool) {
	s.Store(key, value)
}

func (s *jobRunningStore) Get(key string) bool {
	val, ok := s.Load(key)
	if !ok {
		return false
	}
	return val.(bool)
}

func (s *jobRunningStore) Remove(key string) {
	s.Delete(key)
}

func init() {
	maxWorkers = 0
	jobRunningMap = NewJobRunningMap()
	jobStatusMap = NewJobStatusStore()
}

// Add is used to populate the jobs array and pass jobs to the workers via the channel
func (r *Runner) Add(job *types.InspecJob) error {
	logrus.Debugf("Add job %s to be handled by one of the %d workers", job.JobID, maxWorkers)
	if maxWorkers == 0 {
		return errors.New("No workers have been instantiated, can't add job")
	}
	if job == nil {
		return errors.New("Job cannot be nil")
	}
	job.Status = types.StatusScheduled

	if job.SSM {
		job.RemoteInspecVersion = r.remoteInspecVersion
	}

	// place most recent job in the channel to be processed by the workers and updated based on the outcome
	jobsChan <- job

	logrus.Debugf("Add: added job %s", job.JobID)
	return nil
}

// AddJobs is used to populate the jobs array and pass jobs to the workers via the channel
func (r *Runner) AddJobs(id string, jobs []*types.InspecJob) error {
	if len(jobs) == 0 {
		return errors.Errorf("no jobs have been provided!")
	}

	jobNodeStatusMap := make(map[string]*string)
	for _, job := range jobs {
		status := types.StatusScheduled
		jobNodeStatusMap[job.NodeID] = &status
		jobStatusMap.Set(job.JobID, jobNodeStatusMap)
		if job == nil {
			return errors.New("Job cannot be nil")
		}
		job.Status = types.StatusScheduled
		job.NodeStatus = jobNodeStatusMap[job.NodeID]

		if job.SSM {
			job.RemoteInspecVersion = r.remoteInspecVersion
		}

		r.scannerServer.UpdateJobStatus(job.JobID, job.Status, nil, nil)
		job.InternalProfiles, job.ProfilesOwner = updateComplianceURLs(job.Profiles)

	}
	return r.workflowManager.EnqueueWorkflow(context.TODO(), "scan-job-workflow", fmt.Sprintf("scan-job-%s", id), jobs)
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
			owner_id := strings.SplitN(profile, "/", 2)
			if len(owner_id) < 2 {
				logrus.Errorf("no profile owner supplied")
				continue
			}
			id_version := strings.SplitN(owner_id[1], "#", 2)
			owner = neturl.QueryEscape(owner_id[0])
			if len(id_version) < 2 {
				logrus.Errorf("no profile version supplied")
				continue
			}
			name := neturl.QueryEscape(id_version[0])
			version := neturl.QueryEscape(id_version[1])
			url = fmt.Sprintf("http://127.0.0.1:%d/profiles/tar?owner=%s&name=%s&version=%s", ListenPort, owner, name, version)
			newProfiles = append(newProfiles, url)
		} else {
			newProfiles = append(newProfiles, url)
		}
	}
	return newProfiles, owner
}

func (s *jobStatusStore) CheckJobStatus(scannerServer *scanner.Scanner) {
	jobStatusMap.Range(func(jobId interface{}, jobNodeStatusMap interface{}) bool {
		logrus.Debugf("checking jobId %s", jobId)
		finished, jobStatus := isJobFinished(jobNodeStatusMap.(map[string]*string))
		if finished {
			cleanupMaps(jobId.(string))
			scannerServer.UpdateJobStatus(jobId.(string), jobStatus, nil, timeNowRef())
		} else if jobStatus == types.StatusRunning {
			// to prevent unnecessary db calls, ensure that the job has not already
			// been marked as running by checking for the jobId in the jobRunningMap
			running := jobRunningMap.Get(jobId.(string))
			if !running {
				jobRunningMap.Set(jobId.(string), true)
				scannerServer.UpdateJobStatus(jobId.(string), jobStatus, timeNowRef(), nil)
			}
		}
		return true
	})
}

func watchJobsNodesStatus(scannerServer *scanner.Scanner) {
	for {
		jobStatusMap.CheckJobStatus(scannerServer)
		time.Sleep(time.Duration(5) * time.Second)
	}
}

func isJobFinished(jobNodeStatusMap map[string]*string) (bool, string) {
	var scheduled, failure, completed, aborted bool

	//gather the state for each node.
	for _, status := range jobNodeStatusMap {
		switch *status {
		case types.StatusRunning:
			return false, types.StatusRunning // If any nodes are still running, then the job is Running. That simple.
		case types.StatusScheduled:
			scheduled = true
		case types.StatusFailed:
			failure = true
		case types.StatusAborted:
			aborted = true
		case types.StatusCompleted:
			completed = true
		}
	}

	if scheduled {
		if failure || aborted || completed {
			return false, types.StatusRunning
		} else {
			return false, types.StatusScheduled
		}
	} else if failure {
		return true, types.StatusFailed
	} else if aborted {
		return true, types.StatusAborted
	}

	//if not running, scheduled, failed or aborted.. we must be complete!
	return true, types.StatusCompleted
}

func cleanupMaps(jobId string) {
	jobStatusMap.Remove(jobId)
	jobRunningMap.Remove(jobId)
}
