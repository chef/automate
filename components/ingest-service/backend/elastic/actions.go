//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"fmt"

	"github.com/olivere/elastic"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

// InsertAction inserts a Chef Client Action into action-YYYY.MM.DD index
func (es *Backend) InsertAction(ctx context.Context, action backend.InternalChefAction) error {
	mapping := mappings.Actions
	// Using time field to ingest to correctly dated index
	time := action.RecordedAt
	err := es.addDataToTimeseriesIndex(ctx, mapping, time, action)
	return err
}

func (es *Backend) CreateBulkActionRequest(action backend.InternalChefAction) elastic.BulkableRequest {
	mapping := mappings.Actions
	time := action.RecordedAt
	return es.createBulkUpdateRequestToTimeseriesIndex(mapping, time, action)
}

func (es *Backend) DeleteActionProjectTag(ctx context.Context,
	projectTagToBeDelete string) (string, error) {
	script := `
		if (ctx._source['projects'] != null) {
			def foundIndex = ctx._source.projects.indexOf(params.project);

			if (foundIndex != -1) {
				ctx._source.projects.remove(foundIndex);
			}
		}
	`

	params := map[string]interface{}{"project": projectTagToBeDelete}

	index := fmt.Sprintf("%s-%s", mappings.Actions.Index, "*")

	startTaskResult, err := elastic.NewUpdateByQueryService(es.client).
		Index(index).
		Type(mappings.Actions.Type).
		Script(elastic.NewScript(script).Params(params)).
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)

	if err != nil {
		return "", err
	}

	return startTaskResult.TaskId, nil
}

func (es *Backend) UpdateActionProjectTags(ctx context.Context,
	projectTaggingRules map[string]*iam_v2.ProjectRules) (string, error) {
	script := `
		ArrayList matchingProjects = new ArrayList();
		for (def project : params.projects) {
			for (def rule : project.rules) {
				def match = rule.conditions.length != 0;
				for (def condition : rule.conditions) {
					if (condition.chefServers.length > 0) {
						def found = false;
						for (def chefServer : condition.chefServers){
							if (ctx._source.remote_hostname == chefServer ) {
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
							if (ctx._source.organization_name == organization ) {
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

	index := fmt.Sprintf("%s-%s", mappings.Actions.Index, "*")

	startTaskResult, err := elastic.NewUpdateByQueryService(es.client).
		Index(index).
		Type(mappings.Actions.Type).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)
	if err != nil {
		return "", err
	}

	return startTaskResult.TaskId, nil
}
