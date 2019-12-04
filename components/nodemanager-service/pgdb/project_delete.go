package pgdb

import (
	"context"

	project_update_lib "github.com/chef/automate/lib/authz"
	uuid "github.com/chef/automate/lib/uuid4"
	log "github.com/sirupsen/logrus"
)

const deleteProjectNodeAssociation = `
DELETE FROM nodes_projects np
USING projects p
WHERE p.id = np.project_id AND p.project_id = $1;
`

type ProjectDelete struct {
	jobStatus      project_update_lib.JobStatus
	jobStatusError error
	running        bool
	ID             string
}

func (db *DB) DeleteProjectTag(ctx context.Context,
	projectTagToBeDeleted string) ([]string, error) {
	jobUUID := uuid.Must(uuid.NewV4()).String()
	log.Debugf("Running node manager DeleteProjectTag %v ID: %s", projectTagToBeDeleted, jobUUID)

	db.ProjectDelete.start(jobUUID)

	go db.deleteProject(projectTagToBeDeleted)

	return []string{jobUUID}, nil
}

func (db *DB) deleteProject(projectTagToBeDeleted string) {
	_, err := db.Exec(deleteProjectNodeAssociation, projectTagToBeDeleted)
	if err != nil {
		db.ProjectDelete.updateFail(err)
	}

	db.ProjectDelete.updateComplete()
}

func (projectDelete *ProjectDelete) start(jobUUID string) {
	projectDelete.jobStatusError = nil
	projectDelete.running = true
	projectDelete.jobStatus = project_update_lib.JobStatus{
		Completed:             false,
		PercentageComplete:    0,
		EstimatedEndTimeInSec: 0,
	}
	projectDelete.ID = jobUUID
}

func (projectDelete *ProjectDelete) getJobStatus(jobUUID string) (project_update_lib.JobStatus, error) {
	if projectDelete.ID == jobUUID {
		if projectDelete.jobStatusError != nil {
			return project_update_lib.JobStatus{}, projectDelete.jobStatusError
		}
		return projectDelete.jobStatus, nil
	}

	// If the jobID does not match the currently running job then it must have completed before.
	return project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1,
		EstimatedEndTimeInSec: 0,
	}, nil
}

func (projectDelete *ProjectDelete) updateComplete() {
	projectDelete.jobStatus = project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1.0,
		EstimatedEndTimeInSec: 0,
	}
	projectDelete.running = false
}

func (projectDelete *ProjectDelete) updateFail(err error) {
	projectDelete.jobStatusError = err
	projectDelete.jobStatus = project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1.0,
		EstimatedEndTimeInSec: 0,
	}
	projectDelete.running = false
}
