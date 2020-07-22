//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"fmt"

	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/interservice/authz"
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

func (es *Backend) UpdateActionProjectTags(ctx context.Context,
	projectTaggingRules map[string]*authz.ProjectRules) (string, error) {
	return es.UpdateActionProjectTagsForIndex(ctx, fmt.Sprintf("%s-%s", mappings.Actions.Index, "*"),
		projectTaggingRules)
}

func (es *Backend) UpdateActionProjectTagsForIndex(ctx context.Context, index string,
	projectTaggingRules map[string]*authz.ProjectRules) (string, error) {
	script := `
		ArrayList matchingProjects = new ArrayList();
		for (def project : params.projects) {
			for (def rule : project.rules) {
				def match = rule.conditions.length != 0;
				for (def condition : rule.conditions) {
					if (condition.chefServers.length > 0) {
						def found = false;
						for (def chefServer : condition.chefServers){
							if (ctx._source.service_hostname == chefServer ) {
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
