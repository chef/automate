package ingestic

import (
	"time"

	"fmt"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	project_update_lib "github.com/chef/automate/lib/authz"
	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

type ESClient struct {
	client      *elastic.Client
	initialized bool
}

func NewESClient(client *elastic.Client) *ESClient {
	return &ESClient{client: client, initialized: false}
}

// This method will support adding a document with a specified ID
// giving the ID is important if you want to be able to update the document later
func (backend *ESClient) addDataToIndexWithID(ctx context.Context,
	mapping mappings.Mapping,
	ID string,
	data interface{}) error {

	// Add a document on a particular index and a particular id
	// This is not creating an index with a timestring at the end
	_, err := backend.client.Index().
		Index(mapping.Index).
		Id(ID).
		Type(mapping.Type).
		BodyJson(data).
		Do(ctx)
	return err
}

// InitializeStore runs the necessary initialization processes to make elasticsearch usable
// in particular it creates the indexes and aliases for documents to be added
func (backend *ESClient) InitializeStore(ctx context.Context) {
	logrus.Info("Initialize elastic with mappings")
	if !backend.initialized {
		for _, esMap := range mappings.AllMappings {
			backend.CreateTemplate(ctx, esMap.Index, esMap.Mapping)
			if !esMap.Timeseries {
				backend.createStoreIfNotExists(ctx, esMap.Index, esMap.Mapping)
				backend.createStoreAliasIfNotExists(ctx, esMap.Alias, esMap.Index)
			}
		}
	}
	backend.initialized = true
}

func (backend *ESClient) createStoreAliasIfNotExists(ctx context.Context, alias string, index string) {
	if len(alias) > 0 {
		if exists, _ := relaxting.StoreExists(backend.client, alias); !exists {
			indexError := backend.createAlias(ctx, alias, index)
			if indexError != nil {
				logrus.Errorf("Error creating alias %s with error: %s", alias, indexError.Error())
			}
		}
	}
}

func (backend *ESClient) CreateTemplate(ctx context.Context, templateName string, mapping string) {
	// We don't care if it already exists because it will update the template
	_, err := backend.client.IndexPutTemplate(templateName).BodyString(mapping).Do(ctx)
	if err != nil {
		logrus.Errorf("Error creating index %s with error: %s", templateName, err.Error())
	}
}

func (backend *ESClient) createStoreIfNotExists(ctx context.Context, indexName string, mapping string) {
	exists, _ := relaxting.StoreExists(backend.client, indexName)
	if !exists {
		indexError := backend.createStore(ctx, indexName, mapping)
		if indexError != nil {
			logrus.Errorf("Error creating index %s with error: %s", indexName, indexError.Error())
		}
	}
}

func (backend *ESClient) createAlias(ctx context.Context, aliasName string, indexName string) error {
	_, error := backend.client.Alias().Add(indexName, aliasName).Do(ctx)

	return error
}

func (backend *ESClient) createStore(ctx context.Context, indexName string, mapping string) error {
	// Create the actual index. The mapping is applied using the CreateTemplate method
	_, error := backend.client.CreateIndex(indexName).Do(ctx)

	return error
}

//ProfileExists returns true if profile exists already in ES.. false if not
func (backend *ESClient) ProfileExists(hash string) (bool, error) {
	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(hash)

	searchResult, err := backend.client.Search().
		Index(relaxting.CompProfilesIndex).
		Query(idsQuery).
		Size(0).
		Do(context.Background())

	if err != nil {
		return false, errors.Wrap(err, fmt.Sprintf("ProfileExists unable to complete search for %s", hash))
	}

	logrus.Debugf("ProfileExists got %d profiles in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	return searchResult.TotalHits() > 0, nil
}

func (backend *ESClient) InsertInspecSummary(ctx context.Context, id string, endTime time.Time, data *relaxting.ESInSpecSummary) error {
	mapping := mappings.ComplianceSumDate
	index := mapping.IndexTimeseriesFmt(endTime)
	data.DailyLatest = true
	data.ReportID = id

	// Add the summary document to the compliance timeseries index using the specified report id as document id
	_, err := backend.client.Index().
		Index(index).
		Id(id).
		Type(mappings.DocType).
		BodyJson(*data).
		Refresh("false").
		Do(ctx)
	if err != nil {
		return err
	}

	err = backend.setDailyLatestToFalse(ctx, data.NodeID, id, index)
	return err
}

func (backend *ESClient) InsertInspecReport(ctx context.Context, id string, endTime time.Time, data *relaxting.ESInSpecReport) error {
	docType := mappings.DocType
	mapping := mappings.ComplianceRepDate
	index := mapping.IndexTimeseriesFmt(endTime)
	data.DailyLatest = true
	data.ReportID = id

	// Add the report document to the compliance timeseries index using the specified report id as document id
	_, err := backend.client.Index().
		Index(index).
		Id(id).
		Type(docType).
		BodyJson(*data).
		Refresh("false").
		Do(ctx)
	if err != nil {
		return err
	}

	err = backend.setDailyLatestToFalse(ctx, data.NodeID, id, index)
	return err
}

func (backend *ESClient) InsertInspecProfile(ctx context.Context, data *relaxting.ESInspecProfile) error {
	mapping := mappings.ComplianceProfiles
	err := backend.addDataToIndexWithID(ctx, mapping, data.Sha256, data)
	return err
}

func (backend *ESClient) setDailyLatestToFalse(ctx context.Context, nodeId string, reportId string, index string) error {
	termQueryDailyLatestTrue := elastic.NewTermQuery("daily_latest", true)
	termQueryThisNode := elastic.NewTermsQuery("node_uuid", nodeId)
	termQueryNotThisReport := elastic.NewTermsQuery("_id", reportId)

	boolQueryDailyLatestThisNodeNotThisReport := elastic.NewBoolQuery().
		Must(termQueryDailyLatestTrue).
		Must(termQueryThisNode).
		MustNot(termQueryNotThisReport)

	script := elastic.NewScript("ctx._source.daily_latest = false")

	//retries := 3
	//err := errors.New("init")
	//sec := rand.Intn(60)
	//time.Sleep(time.Duration(sec) * time.Second)
	logrus.Infof("Running NewUpdateByQueryService for node %s w/ report %s", nodeId, reportId)
	//for retries > 0 && err != nil {
	_, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Query(boolQueryDailyLatestThisNodeNotThisReport).
		Script(script).
		Refresh("true").
		Do(ctx)
		//	retries -= 1
		//}
	if err != nil {
		errors.Wrap(err, "daily_latest update failed")
	}

	return err
}

func (backend *ESClient) UpdateProjectTags(ctx context.Context, projectTaggingRules map[string]*iam_v2.ProjectRules) ([]string, error) {
	logrus.Debug("starting project updater")

	esReportJobID, err := backend.UpdateReportProjectsTags(ctx, projectTaggingRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch report project tags update")
	}

	esSummaryJobID, err := backend.UpdateSummaryProjectsTags(ctx, projectTaggingRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch summary project tags update")
	}

	logrus.Debugf("Started Project rule updates with report job ID: %q and summary job ID %q",
		esReportJobID, esSummaryJobID)

	return []string{esReportJobID, esSummaryJobID}, nil
}

func (backend *ESClient) UpdateReportProjectsTags(ctx context.Context, projectTaggingRules map[string]*iam_v2.ProjectRules) (string, error) {

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

	docType := mappings.DocType
	mapping := mappings.ComplianceRepDate
	index := fmt.Sprintf("%s-%s", mapping.Index, "*")

	startTaskResult, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Type(docType).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		Refresh("true").
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)

	return startTaskResult.TaskId, err
}

func (backend *ESClient) UpdateSummaryProjectsTags(ctx context.Context, projectTaggingRules map[string]*iam_v2.ProjectRules) (string, error) {

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

	docType := mappings.DocType
	mapping := mappings.ComplianceSumDate
	index := fmt.Sprintf("%s-%s", mapping.Index, "*")

	startTaskResult, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Type(docType).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		Refresh("true").
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)

	return startTaskResult.TaskId, err
}

func (backend *ESClient) JobCancel(ctx context.Context, jobID string) error {
	_, err := elastic.NewTasksCancelService(backend.client).
		TaskId(jobID).
		Do(ctx)
	return err
}

func (backend *ESClient) JobStatus(ctx context.Context, jobID string) (project_update_lib.JobStatus, error) {
	tasksGetTaskResponse, err := elastic.NewTasksGetTaskService(backend.client).
		TaskId(jobID).
		WaitForCompletion(false).
		Do(ctx)
	if err != nil {
		return project_update_lib.JobStatus{}, err
	}

	if tasksGetTaskResponse.Task == nil {
		return project_update_lib.JobStatus{
			Completed: tasksGetTaskResponse.Completed,
		}, nil
	}

	var estimatedEndTimeInSec int64

	percentageComplete := getPercentageComplete(tasksGetTaskResponse.Task.Status)

	if percentageComplete != 0 {
		runningTimeNanos := float64(tasksGetTaskResponse.Task.RunningTimeInNanos)
		timeLeftSec := int64(runningTimeNanos / percentageComplete / 1000000000.0)
		estimatedEndTimeInSec = tasksGetTaskResponse.Task.StartTimeInMillis/1000 + timeLeftSec
	}

	return project_update_lib.JobStatus{
		Completed:             tasksGetTaskResponse.Completed,
		PercentageComplete:    float32(percentageComplete),
		EstimatedEndTimeInSec: estimatedEndTimeInSec,
	}, nil
}

func getPercentageComplete(status interface{}) float64 {
	statusMap, ok := status.(map[string]interface{})
	if !ok {
		return 0
	}

	updated, ok := statusMap["updated"].(float64)
	if !ok {
		return 0
	}
	total, ok := statusMap["total"].(float64)
	if !ok {
		return 0
	}

	if total == 0 {
		return 0
	}

	return updated / total
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
				switch condition.Attribute {
				case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS:
					chefServers = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS:
					organizations = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS:
					environments = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_ROLES:
					roles = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_TAGS:
					chefTags = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_POLICY_GROUP:
					policyGroups = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_POLICY_NAME:
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
