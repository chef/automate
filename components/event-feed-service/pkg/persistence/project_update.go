package persistence

import (
	"context"

	"github.com/chef/automate/api/interservice/authz"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/elasticutil"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	olivere "gopkg.in/olivere/elastic.v6"
)

func (efs ElasticFeedStore) UpdateProjectTags(
	ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) ([]string, error) {
	logrus.Debug("starting UpdateProjectTags")

	jobID, err := efs.updateEventProjectTags(ctx, projectTaggingRules)

	return []string{jobID}, err
}

func (efs ElasticFeedStore) JobCancel(ctx context.Context, jobID string) error {
	logrus.Debug("starting JobCancel")
	_, err := olivere.NewTasksCancelService(efs.client).
		TaskId(jobID).
		Do(ctx)
	return err
}

func (efs ElasticFeedStore) ListProjectUpdateTasks(
	ctx context.Context) ([]project_update_lib.SerializedProjectUpdateTask, error) {
	logrus.Debug("start ListProjectUpdateTasks")

	return []project_update_lib.SerializedProjectUpdateTask{
		{
			Params: map[string]string{
				"n": IndexNameFeeds,
			},
			Priority: 1,
		},
	}, nil
}

func (efs ElasticFeedStore) RunProjectUpdateTask(ctx context.Context, projectUpdateID string, params map[string]string,
	projectTaggingRules map[string]*authz.ProjectRules) (project_update_lib.SerializedProjectUpdateTaskID, project_update_lib.SerializedProjectUpdateTaskStatus, error) {
	logrus.Debug("running RunProjectUpdateTask")

	if params == nil {
		return "", project_update_lib.NoStatus, errors.New("project update tasks params missing")
	}

	indexName := params["n"]
	if indexName == "" {
		return "", project_update_lib.NoStatus, errors.New("project update task index missing")
	}

	if indexName == IndexNameFeeds {
		id, err := efs.updateEventProjectTags(ctx, projectTaggingRules)
		if err != nil {
			return "", project_update_lib.NoStatus, err
		}

		return project_update_lib.SerializedProjectUpdateTaskID(id),
			project_update_lib.SerializedProjectUpdateTaskStatus{
				State: project_update_lib.SerializedProjectUpdateTaskRunning,
			}, nil
	}

	return "", project_update_lib.NoStatus, errors.Errorf(
		"project update index %s does not match any current mappings", indexName)
}

func (efs ElasticFeedStore) MonitorProjectUpdateTask(ctx context.Context, projectUpdateID string,
	id project_update_lib.SerializedProjectUpdateTaskID) (project_update_lib.SerializedProjectUpdateTaskStatus, error) {
	logrus.Debug("running MonitorProjectUpdateTask")

	res, err := elasticutil.GetUpdateByQueryTask(ctx, efs.client, string(id))

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

func (efs ElasticFeedStore) CancelProjectUpdateTask(ctx context.Context,
	projectUpdateID string, id project_update_lib.SerializedProjectUpdateTaskID) error {
	logrus.Debug("running CancelProjectUpdateTask")
	return efs.JobCancel(ctx, string(id))
}

func (efs ElasticFeedStore) updateEventProjectTags(
	ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) (string, error) {
	logrus.Debug("running updateEventProjectTags")
	script := `
	  if (ctx._source.producer_object_type != "chef_server") {
			ctx._source.projects = new ArrayList();
			return;
		}
		ArrayList matchingProjects = new ArrayList();
		for (def project : params.projects) {
			for (def rule : project.rules) {
				def match = rule.conditions.length != 0;
				for (def condition : rule.conditions) {
					if (condition.chefServers.length > 0) {
						def found = false;
						for (def chefServer : condition.chefServers){
							if (ctx._source.chef_infra_server == chefServer ) {
								found = true;
								break;
							}
						}
						if ( !found ) {
							match = false;
							break;
						}
					}
					if (condition.organizations.length > 0) {
						def found = false;
						for (def organization : condition.organizations){
							if (ctx._source.chef_organization == organization ) {
								found = true;
								break;
							}
						}
						if ( !found ) {
							match = false;
							break;
						}
					}
				}
				if ( match ) {
					matchingProjects.add(project.name);
					break;
				}
			}
		}

		ctx._source.projects = matchingProjects.toArray();
	`

	startTaskResult, err := olivere.NewUpdateByQueryService(efs.client).
		Index(IndexNameFeeds).
		Type(Feeds.Type).
		Script(olivere.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)
	if err != nil {
		return "", err
	}

	return startTaskResult.TaskId, nil
}

func convertProjectTaggingRulesToEsParams(projectTaggingRules map[string]*authz.ProjectRules) map[string]interface{} {
	esProjectCollection := make([]map[string]interface{}, len(projectTaggingRules))
	projectIndex := 0
	for projectName, projectRules := range projectTaggingRules {
		esRuleCollection := make([]map[string]interface{}, len(projectRules.Rules))
		for ruleIndex, rule := range projectRules.Rules {
			esConditionCollection := make([]map[string]interface{}, len(rule.Conditions))

			for conditionIndex, condition := range rule.Conditions {
				chefServers := []string{}
				organizations := []string{}
				switch condition.Attribute {
				case authz.ProjectRuleConditionAttributes_CHEF_SERVER:
					chefServers = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
					organizations = condition.Values
				}
				esConditionCollection[conditionIndex] = map[string]interface{}{
					"chefServers":   chefServers,
					"organizations": organizations,
				}

			}
			esRuleCollection[ruleIndex] = map[string]interface{}{
				"conditions": esConditionCollection,
			}
		}

		esProjectCollection[projectIndex] = map[string]interface{}{
			"name":  projectName,
			"rules": esRuleCollection,
		}
		projectIndex++
	}

	return map[string]interface{}{"projects": esProjectCollection}
}
