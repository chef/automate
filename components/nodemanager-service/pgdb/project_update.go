package pgdb

import (
	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	project_update_lib "github.com/chef/automate/lib/authz"
	uuid "github.com/chef/automate/lib/uuid4"
	log "github.com/sirupsen/logrus"
)

type NodeProjectData struct {
	ID   string
	Data string
}

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
	nodes, err := db.getAllNodes()
	if err != nil {
		// Fail the project update
	}

	numberOfNodes := float32(len(nodes))
	db.projectUpdateJobStatus = project_update_lib.JobStatus{
		Completed:             false,
		PercentageComplete:    0,
		EstimatedEndTimeInSec: 0,
	}
	for index, node := range nodes {
		matchingProjectIDs := getMatchingProjectIDs(node, projectRules)

		db.updateNodeProjectIDs(node, matchingProjectIDs)

		db.projectUpdateJobStatus = project_update_lib.JobStatus{
			Completed:             false,
			PercentageComplete:    float32((index + 1)) / numberOfNodes,
			EstimatedEndTimeInSec: 0,
		}
	}

	// Mark the Job status complete.
	db.projectUpdateJobStatus = project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1.0,
		EstimatedEndTimeInSec: 0,
	}
}

func (db *DB) updateNodeProjectIDs(node *NodeProjectData, matchingProjectIDs []string) {
	// update the nodes project IDs
}

func (db *DB) getAllNodes() ([]*NodeProjectData, error) {
	return []*NodeProjectData{}, nil
}

func getMatchingProjectIDs(node *NodeProjectData, projectRules map[string]*iam_v2.ProjectRules) []string {
	return []string{}
}
