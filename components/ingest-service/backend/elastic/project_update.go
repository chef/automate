package elastic

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/elasticutil"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func (backend *Backend) ListProjectUpdateTasks(ctx context.Context) ([]project_update_lib.SerializedProjectUpdateTask, error) {
	return []project_update_lib.SerializedProjectUpdateTask{
		project_update_lib.SerializedProjectUpdateTask{
			Params: map[string]string{
				"n": mappings.NodeState.Index,
			},
			Priority: time.Now().Unix(),
		},
	}, nil
}

func (backend *Backend) RunProjectUpdateTask(ctx context.Context, projectUpdateID string,
	params map[string]string, projectTaggingRules map[string]*authz.ProjectRules) (
	project_update_lib.SerializedProjectUpdateTaskID,
	project_update_lib.SerializedProjectUpdateTaskStatus,
	error) {

	if params == nil {
		return "", project_update_lib.NoStatus, errors.New("project update tasks params missing")
	}

	indexName := params["n"]
	if indexName == "" {
		return "", project_update_lib.NoStatus, errors.New("project update task index missing")
	}

	var id string
	var err error
	if indexName == mappings.NodeState.Index {
		id, err = backend.UpdateNodeProjectTags(ctx, projectTaggingRules)
	} else {
		return "", project_update_lib.NoStatus, errors.Errorf(
			"project update index %s does not match any current mappings", indexName)
	}

	if err != nil {
		return "", project_update_lib.NoStatus, err
	}

	return project_update_lib.SerializedProjectUpdateTaskID(id),
		project_update_lib.SerializedProjectUpdateTaskStatus{
			State: project_update_lib.SerializedProjectUpdateTaskRunning,
		}, nil
}

func (backend *Backend) MonitorProjectUpdateTask(ctx context.Context,
	projectUpdateID string, id project_update_lib.SerializedProjectUpdateTaskID) (
	project_update_lib.SerializedProjectUpdateTaskStatus, error) {

	res, err := elasticutil.GetUpdateByQueryTask(ctx, backend.client, string(id))

	if err != nil {
		logrus.WithError(err).Error("Could not query task")
		return project_update_lib.NoStatus, err
	}

	projectUpdateStatus := project_update_lib.SerializedProjectUpdateTaskStatus{}
	if res.Completed {
		projectUpdateStatus.PercentageComplete = 1
		if res.Response == nil {
			projectUpdateStatus.State = project_update_lib.SerializedProjectUpdateTaskSuccess
			logrus.Warnf("response missing from completed task for task %s", id)
		} else if len(res.Response.Failures) > 0 {
			projectUpdateStatus.State = project_update_lib.SerializedProjectUpdateTaskFailed
			projectUpdateStatus.Error = res.Response.Failures[0]
		} else {
			projectUpdateStatus.State = project_update_lib.SerializedProjectUpdateTaskSuccess
		}
	} else {
		projectUpdateStatus.State = project_update_lib.SerializedProjectUpdateTaskRunning
		if res.Task.Status != nil && res.Task.Status.Total > 0 {
			projectUpdateStatus.PercentageComplete = float32(
				res.Task.Status.Updated+res.Task.Status.Created+res.Task.Status.Deleted+
					res.Task.Status.VersionConflicts+res.Task.Status.Noops) / float32(res.Task.Status.Total)
		}
	}

	return projectUpdateStatus, nil
}

func (backend *Backend) CancelProjectUpdateTask(ctx context.Context, projectUpdateID string,
	id project_update_lib.SerializedProjectUpdateTaskID) error {
	return backend.JobCancel(ctx, string(id))
}
