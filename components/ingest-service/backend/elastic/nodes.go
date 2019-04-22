//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"errors"
	"time"

	"github.com/olivere/elastic"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

// InsertNode inserts the state of the node as a result of a Chef Client Run into node-state index
func (es *Backend) InsertNode(ctx context.Context, node backend.Node) error {
	mapping := mappings.NodeState
	err := es.upsertDataWithID(ctx, mapping, node.EntityUuid, node)
	return err
}

func (es *Backend) CreateBulkNodeUpdateRequest(node backend.Node) elastic.BulkableRequest {
	mapping := mappings.NodeState
	return es.createBulkRequestUpsertDataWithID(mapping, node.EntityUuid, node)
}

// InsertNodeAttribute inserts the ohai attributes from a CCR into the node-attribute index
func (es *Backend) InsertNodeAttribute(ctx context.Context, nodeAttr backend.NodeAttribute) error {
	mapping := mappings.NodeAttribute
	err := es.addDataToIndexWithID(ctx, mapping, nodeAttr.EntityUUID, nodeAttr)
	return err
}

func (es *Backend) CreateBulkNodeAttributeUpdateRequest(nodeAttr backend.NodeAttribute) elastic.BulkableRequest {
	mapping := mappings.NodeAttribute
	return es.createBulkUpdateRequestToIndexWithID(mapping, nodeAttr.EntityUUID, nodeAttr)
}

// MarkForDeleteMultipleNodesByID marks for delete multiple nodes by their IDs
func (es *Backend) MarkForDeleteMultipleNodesByID(ctx context.Context, nodeIDs []string) (int, error) {
	if len(nodeIDs) == 0 {
		return 0, nil
	}

	query := elastic.NewBoolQuery()

	for _, nodeID := range nodeIDs {
		query.Should(elastic.NewTermsQuery("entity_uuid", nodeID))
	}

	script := elastic.NewScript("ctx._source.exists = false")

	bulkResponse, err := elastic.NewUpdateByQueryService(es.client).
		Index(mappings.NodeState.Alias).
		Type(mappings.NodeState.Type).
		Query(query).
		Script(script).
		Refresh("true").
		ProceedOnVersionConflict().
		Do(ctx)

	if err != nil {
		if bulkResponse != nil {
			return int(bulkResponse.Updated), err
		}

		return 0, err
	}

	return int(bulkResponse.Updated), nil
}

// DeleteNodeByID deletes a node from node-state by it's ID
func (es *Backend) DeleteNodeByID(ctx context.Context, nodeID string) (int, error) {
	filters := map[string]string{
		"entity_uuid": nodeID,
	}
	docIDs, err := es.findNodeIDByFields(ctx, filters)

	if err != nil {
		return 0, err
	}
	return es.deleteNodeByDocID(ctx, docIDs)
}

// UpdateNodeProjectTags - update project tagging with all the rules
//
// All the conditions must be true for a rule to be true (ANDed together).
// Only one rule has to be true for the project to match (ORed together).
// If there are no rules the project does not match
// If the rule does not have any conditions it does not match any resources.
func (es *Backend) UpdateNodeProjectTags(ctx context.Context, projectTaggingRules map[string]*iam_v2.ProjectRules) (string, error) {
	script := `
		ArrayList matchingProjects = new ArrayList();
		for (def project : params.projects) {
			for (def rule : project.rules) {
				def match = rule.conditions.length != 0;
				for (def condition : rule.conditions) {
					if (condition.chefServers.length > 0) {
						def found = false;
						for (def chefServer : condition.chefServers){
							if (ctx._source.source_fqdn == chefServer ) {
								found = true;
								break;
							}
						}
						if ( !found ) {
							match = false;
							break;
						}
					}
					if (condition.environments.length > 0) {
						def found = false;
						for (def environment : condition.environments){
							if (ctx._source.environment == environment ) {
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

					if (condition.policyNames.length > 0) {
						def found = false;
						for (def policyName : condition.policyNames){
							if (ctx._source.policy_name == policyName ) {
								found = true;
								break;
							}
						}
						if ( !found ) {
							match = false;
							break;
						}
					}

					if (condition.policyGroups.length > 0) {
						def found = false;
						for (def policyGroup : condition.policyGroups){
							if (ctx._source.policy_group == policyGroup ) {
								found = true;
								break;
							}
						}
						if ( !found ) {
							match = false;
							break;
						}
					}
					if (condition.roles.length > 0) {
						def found = false;
						for (def ruleRole : condition.roles){
							for (def ccrRole : ctx._source.roles){
								if (ccrRole == ruleRole ) {
									found = true;
									break;
								}
							}
							if (found) { break; }
						}
						if ( !found ) {
							match = false;
							break;
						}
					}
					if (condition.chefTags.length > 0) {
						def found = false;
						for (def ruleChefTag : condition.chefTags) {
							for (def ccrChefTag : ctx._source.chef_tags) {
								if (ccrChefTag == ruleChefTag ) {
									found = true;
									break;
								}
							}
							if (found) { break; }
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
		Index(mappings.NodeState.Alias).
		Type(mappings.NodeState.Type).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)

	return startTaskResult.TaskId, err
}

// DeleteNodeByFields deletes a node from node state but it's org name, node name and source_fqdn
func (es *Backend) DeleteNodeByFields(ctx context.Context, orgName string, remoteHostname string, nodeName string) (int, error) {
	filters := map[string]string{
		"organization_name": orgName,
		"node_name":         nodeName,
		"source_fqdn":       remoteHostname,
	}
	docIDs, err := es.findNodeIDByFields(ctx, filters)

	if err != nil {
		return 0, err
	}
	return es.deleteNodeByDocID(ctx, docIDs)
}

func (es *Backend) RecordLivenessPing(ctx context.Context, liveness backend.Liveness) error {
	mapping := mappings.NodeState
	return es.upsertDataWithID(ctx, mapping, liveness.NodeID, liveness)
}

func (es *Backend) deleteNodeByDocID(ctx context.Context, docIds []string) (int, error) {
	if len(docIds) == 0 {
		return 0, nil
	}

	bulkRequest := es.client.Bulk()
	for _, docID := range docIds {
		updateReq := elastic.NewBulkUpdateRequest().
			Index(mappings.NodeState.Alias).
			Type(mappings.NodeState.Type).
			Id(docID).
			Doc(struct {
				Exists    bool      `json:"exists"`
				Timestamp time.Time `json:"timestamp"`
			}{
				Exists:    false,
				Timestamp: time.Now().UTC(),
			})
		bulkRequest = bulkRequest.Add(updateReq)
	}
	bulkResponse, err := bulkRequest.Do(ctx)

	if err != nil {
		return 0, err
	}

	updateCount := len(bulkResponse.Updated())

	return updateCount, err
}

func (es *Backend) FindNodeIDByInstanceId(ctx context.Context, instanceId string) ([]string, error) {
	var docIDs []string

	boolQuery := elastic.NewBoolQuery()
	termQuery := elastic.NewTermQuery("cloud_id", instanceId)
	existsQuery := elastic.NewTermQuery("exists", true)
	boolQuery = boolQuery.Must(termQuery)
	boolQuery = boolQuery.Must(existsQuery)

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		FetchSource(false)

	searchResult, err := es.client.Search().
		SearchSource(searchSource).
		Index(mappings.NodeState.Alias).
		Do(ctx)

	if err != nil {
		return nil, err
	}

	if searchResult.Hits.TotalHits > 0 {
		docIDs = make([]string, searchResult.Hits.TotalHits)
		for i, hit := range searchResult.Hits.Hits {
			docIDs[i] = hit.Id
		}
	}

	return docIDs, nil

}

func (es *Backend) findNodeIDByFields(ctx context.Context, filters map[string]string) ([]string, error) {
	var docIDs []string

	// Main boolQuery
	mainQuery := newBoolQueryFromFilters(filters)

	// Add a filter that will prevent to return Nodes that are
	// already marked as "DELETED", that is that has the field
	// `exists` equals to `false`
	termQueryNotExists := elastic.NewTermsQuery(nodeExists, false)
	mainQuery.MustNot(termQueryNotExists)

	searchResult, err := es.client.Search().
		Query(mainQuery).
		Index(mappings.NodeState.Alias).
		Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	if searchResult.Hits.TotalHits > 0 {
		docIDs = make([]string, searchResult.Hits.TotalHits)
		for i, hit := range searchResult.Hits.Hits {
			docIDs[i] = hit.Id
		}
	} else {
		return nil, errors.New("Zero nodes found with the provided filters")
	}

	return docIDs, nil

}

func convertProjectTaggingRulesToEsParams(projectTaggingRules map[string]*iam_v2.ProjectRules) map[string]interface{} {
	esProjectCollection := make([]map[string]interface{}, len(projectTaggingRules))
	projectIndex := 0
	for projectName, projectRules := range projectTaggingRules {
		esRuleCollection := make([]map[string]interface{}, len(projectRules.Rules))
		for ruleIndex, rule := range projectRules.Rules {
			esConditionCollection := make([]map[string]interface{}, len(rule.Conditions))

			for conditionIndex, condition := range rule.Conditions {
				chefServers := []string{}
				organizations := []string{}
				environments := []string{}
				roles := []string{}
				chefTags := []string{}
				policyGroups := []string{}
				policyNames := []string{}
				switch condition.Type {
				case iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS:
					chefServers = condition.Values
				case iam_v2.ProjectRuleConditionTypes_CHEF_ORGS:
					organizations = condition.Values
				case iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS:
					environments = condition.Values
				case iam_v2.ProjectRuleConditionTypes_ROLES:
					roles = condition.Values
				case iam_v2.ProjectRuleConditionTypes_CHEF_TAGS:
					chefTags = condition.Values
				case iam_v2.ProjectRuleConditionTypes_POLICY_GROUP:
					policyGroups = condition.Values
				case iam_v2.ProjectRuleConditionTypes_POLICY_NAME:
					policyNames = condition.Values
				}
				esConditionCollection[conditionIndex] = map[string]interface{}{
					"chefServers":   chefServers,
					"organizations": organizations,
					"environments":  environments,
					"roles":         roles,
					"chefTags":      chefTags,
					"policyGroups":  policyGroups,
					"policyNames":   policyNames,
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
