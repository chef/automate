package runner

import (
	"context"
	"fmt"
	neturl "net/url"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/workflow"
)

var ListenPort int = 2133

type Runner struct {
	scannerServer       *scanner.Scanner
	remoteInspecVersion string
	workflowManager     *workflow.WorkflowManager
}

func New(scannerServer *scanner.Scanner, remoteInspecVersion string, workflowManager *workflow.WorkflowManager) *Runner {
	return &Runner{scannerServer, remoteInspecVersion, workflowManager}
}

// AddJobs is used to populate the jobs array and pass jobs to the workers via the channel
func (r *Runner) AddJobs(id string, jobs []*types.InspecJob) error {
	if len(jobs) == 0 {
		return errors.Errorf("no jobs have been provided!")
	}

	for _, job := range jobs {
		if job == nil {
			return errors.New("Job cannot be nil")
		}
		s := types.StatusScheduled

		job.Status = s
		job.NodeStatus = &s

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
