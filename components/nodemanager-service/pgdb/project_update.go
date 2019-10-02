package pgdb

import (
	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	project_update_lib "github.com/chef/automate/lib/authz"
	uuid "github.com/chef/automate/lib/uuid4"
	log "github.com/sirupsen/logrus"
)

// JobCancel - cancel the a currently running project update
func (db *DB) JobCancel(ctx context.Context, projectID string) error {
	log.Infof("Running node manager JobCancel %s", projectID)
	return nil
}

// UpdateProjectTags - start a project update
func (db *DB) UpdateProjectTags(ctx context.Context, projectRules map[string]*iam_v2.ProjectRules) ([]string, error) {
	jobUUID, _ := uuid.NewV4()
	log.Infof("Running node manager UpdateProjectTags %v ID: %s", projectRules, jobUUID.String())

	go db.updateNodes(ctx, projectRules)
	return []string{jobUUID.String()}, nil
}

// JobStatus - get the job status of the project update
func (db *DB) JobStatus(ctx context.Context, projectID string) (project_update_lib.JobStatus, error) {
	log.Infof("Running node manager JobStatus %s", projectID)
	return db.projectUpdateJobStatus, nil
}

func (db *DB) updateNodes(ctx context.Context, projectRules map[string]*iam_v2.ProjectRules) {
	// numberOfNodes := len(nodes)
	//  db.projectUpdateJobStatus = project_update_lib.JobStatus{
	// 		Completed:             false,
	// 		PercentageComplete:    0.0,
	// 		EstimatedEndTimeInSec: 0,
	// 	}
	// for index, node := range nodes {
	//  matchingProjectIDs := getMatchingProjectIDs(node, projectRules)
	//  updateNodeProjectIDs(node, matchingProjectIDs)
	//  db.projectUpdateJobStatus = project_update_lib.JobStatus{
	// 		Completed:             false,
	// 		PercentageComplete:    index+1/numberOfNodes,
	// 		EstimatedEndTimeInSec: 0,
	// 	}
	// }
	// Need to update the Job status to say complete.
	//	db.projectUpdateJobStatus = project_update_lib.JobStatus{
	// 		Completed:             true,
	// 		PercentageComplete:    1.0,
	// 		EstimatedEndTimeInSec: 0,
	// 	}
}
