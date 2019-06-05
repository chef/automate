package runner

import (
	"context"
	"sync"

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
	//go watchJobsNodesStatus(scannerServer)
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
	logrus.Debugf("Calling EnqueueWorkflow for scan-job-workflow")
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
