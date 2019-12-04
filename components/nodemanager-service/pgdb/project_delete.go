package pgdb

import (
	"context"

	project_update_lib "github.com/chef/automate/lib/authz"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

const deleteProjectNodeAssociation = `
DELETE FROM nodes_projects np
WHERE np.project_id = $1;
`

const deleteProject = `
DELETE FROM projects p
WHERE p.project_id = $1;
`

type ProjectDelete struct {
	jobStatus      project_update_lib.JobStatus
	jobStatusError error
	running        bool
	ID             string
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

func (projectDelete *ProjectDelete) cancel(jobUUID string) {
	if projectDelete.ID == jobUUID {
		projectDelete.running = false
	}
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

func (projectDelete *ProjectDelete) updateProjectStatus(percentageComplete float32) {
	projectDelete.jobStatus = project_update_lib.JobStatus{
		Completed:             false,
		PercentageComplete:    percentageComplete,
		EstimatedEndTimeInSec: 0,
	}
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

func (db *DB) DeleteProjectTag(ctx context.Context,
	projectTagToBeDelete string) ([]string, error) {
	jobUUID := uuid.Must(uuid.NewV4()).String()
	log.Debugf("Running node manager DeleteProjectTag %v ID: %s", projectTagToBeDelete, jobUUID)

	db.ProjectDelete.start(jobUUID)

	go db.deleteProject(projectTagToBeDelete)

	return []string{jobUUID}, nil
}

func (db *DB) deleteProject(projectTagToBeDelete string) {
	err := Transact(db, func(tx *DBTrans) error {
		_, err := tx.Exec(deleteProjectNodeAssociation, projectTagToBeDelete)
		if err != nil {
			return errors.Wrap(err, 
				"deleteProjectNodeAssociation unable to delete project association from the nodes_projects table")
		}
		db.ProjectDelete.updateProjectStatus(0.5)

		_, err = tx.Exec(deleteProject, projectTagToBeDelete)
		if err != nil {
			return errors.Wrap(err, "deleteProject unable to delete project from the projects table")
		}
		db.ProjectDelete.updateProjectStatus(1.0)

		return nil
	})

	if err != nil {
		db.ProjectDelete.updateFail(err)
	}

	db.ProjectDelete.updateComplete()
}
