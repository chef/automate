package ingestic

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"time"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	project_update_lib "github.com/chef/automate/lib/authz"
	elastic "github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
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
	// This is not creating  an index with a timestring at the end
	_, err := backend.client.Index().
		Index(mapping.Index).
		Id(ID).
		BodyJson(data).
		Do(ctx)
	return err
}

// This method will support adding a document with a specified id
func (backend *ESClient) upsertComplianceRunInfo(ctx context.Context, mapping mappings.Mapping, id string, runDateTime time.Time) error {
	runDateTimeAsString := runDateTime.Format(time.RFC3339)

	script := elastic.NewScript("ctx._source.last_run = params.rundate").Param("rundate", runDateTimeAsString)

	_, err := backend.client.Update().
		Index(mapping.Index).
		Id(id).
		Script(script).
		Upsert(map[string]interface{}{
			"node_uuid": id,
			"first_run": runDateTime,
			"last_run":  runDateTime,
		}).
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

// ProfileExists returns true if profile exists already in ES.. false if not
func (backend *ESClient) ProfileExists(hash string) (bool, error) {
	idsQuery := elastic.NewIdsQuery()
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

// ProfilesMissing takes an array of profile sha256 IDs and returns back
// the ones that are missing from the profiles metadata index
//
func (backend *ESClient) ProfilesMissing(allHashes []string) (missingHashes []string, err error) {
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(allHashes...)
	docVersionQuery := elastic.NewMatchQuery("doc_version", "1")

	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(idsQuery)
	boolQuery = boolQuery.Must(docVersionQuery)

	esIndex := relaxting.CompProfilesIndex

	fsc := elastic.NewFetchSourceContext(false)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(1000)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "ProfilesMissing unable to get Source")
	}
	relaxting.LogQueryPartMin(esIndex, source, "ProfilesMissing query searchSource")

	searchResult, err := backend.client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(context.Background())

	if err != nil {
		return missingHashes, errors.Wrap(err, "ProfilesMissing unable to complete search")
	}

	logrus.Debugf("ProfilesMissing got %d meta profiles in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)
	existingHashes := make(map[string]struct{}, searchResult.TotalHits())

	if searchResult.TotalHits() > 0 {
		for _, hit := range searchResult.Hits.Hits {
			existingHashes[hit.Id] = struct{}{}
		}
	}

	for _, oneHash := range allHashes {
		if _, ok := existingHashes[oneHash]; !ok {
			missingHashes = append(missingHashes, oneHash)
		}
	}

	logrus.Debugf("ProfilesMissing returning missingHashes: %v\n", missingHashes)
	return missingHashes, nil
}

// Internal helper method to get profile meta information to complement
// reports being ingested without profile metadata information
func (backend *ESClient) GetProfilesMissingMetadata(profileIDs []string) (map[string]*relaxting.ESInspecProfile, error) {
	esProfilesMeta := make(map[string]*relaxting.ESInspecProfile, 0)
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(profileIDs...)
	esIndex := relaxting.CompProfilesIndex

	fsc := elastic.NewFetchSourceContext(true).Include(
		"took",
		"name",
		"controls.id",
		"controls.impact",
		"controls.title",
		"controls.tags",
	)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(idsQuery).
		Size(10)

	source, err := searchSource.Source()
	if err != nil {
		return esProfilesMeta, errors.Wrap(err, "GetProfilesMissingMetadata unable to get Source")
	}
	relaxting.LogQueryPartMin(esIndex, source, "GetProfilesMissingMetadata query searchSource")

	scroll := backend.client.Scroll().
		Index(esIndex).
		SearchSource(searchSource)

	for {
		results, err := scroll.Do(context.Background())
		//LogQueryPartMin(results, "GetProfilesMissingMetadata query results")
		if err == io.EOF {
			return esProfilesMeta, nil // all results retrieved
		}
		if err != nil {
			return esProfilesMeta, errors.Wrap(err, "GetProfilesMissingMetadata unable to get results")
		}
		if results.TotalHits() > 0 && len(results.Hits.Hits) > 0 {
			logrus.Debugf("GetProfilesMissingMetadata got %d profiles in %d milliseconds\n", results.TotalHits(), results.TookInMillis)

			for _, hit := range results.Hits.Hits {
				esProfile := &relaxting.ESInspecProfile{}
				if err := json.Unmarshal(hit.Source, &esProfile); err != nil {
					logrus.Errorf("GetProfilesMissingMetadata unmarshal error: %s", err.Error())
				}
				esProfilesMeta[hit.Id] = esProfile
			}
		}
	}
}

func (backend *ESClient) InsertInspecSummary(ctx context.Context, id string, endTime time.Time, data *relaxting.ESInSpecSummary) error {
	mapping := mappings.ComplianceSumDate
	index := mapping.IndexTimeseriesFmt(endTime)
	data.DayLatest = false
	if endTime.After(time.Now().Add(-24 * time.Hour)) {
		// Only setting day_latest to true if it's end_time is within the last day (24 hours)
		data.DayLatest = true
	}
	data.DailyLatest = true
	data.ReportID = id
	// Add the summary document to the compliance timeseries index using the specified report id as document id
	_, err := backend.client.Index().
		Index(index).
		Id(id).
		BodyJson(*data).
		Refresh("false").
		Do(ctx)
	if err != nil {
		return errors.Wrap(err, "InsertInspecSummary")
	}

	if data.DayLatest {
		err = backend.UpdateDayLatestToFalse(ctx, data.NodeID, id, index, mapping)
		if err != nil {
			return err
		}
	}
	return backend.setLatestsToFalse(ctx, data.NodeID, id, index)
}

func (backend *ESClient) InsertInspecReport(ctx context.Context, id string, endTime time.Time, data *relaxting.ESInSpecReport) error {
	mapping := mappings.ComplianceRepDate
	index := mapping.IndexTimeseriesFmt(endTime)
	data.DayLatest = false
	if endTime.After(time.Now().Add(-24 * time.Hour)) {
		// Only setting day_latest to true if it's end_time is within the last day (24 hours)
		data.DayLatest = true
	}
	data.DailyLatest = true
	data.ReportID = id

	// Add the report document to the compliance timeseries index using the specified report id as document id
	_, err := backend.client.Index().
		Index(index).
		Id(id).
		BodyJson(*data).
		Refresh("false").
		Do(ctx)
	if err != nil {
		return errors.Wrap(err, "InsertInspecReport")
	}

	if data.DayLatest {
		err = backend.UpdateDayLatestToFalse(ctx, data.NodeID, id, index, mapping)
		if err != nil {
			return err
		}
	}
	return backend.setLatestsToFalse(ctx, data.NodeID, id, index)
}

func (backend *ESClient) InsertInspecProfile(ctx context.Context, data *relaxting.ESInspecProfile) error {
	mapping := mappings.ComplianceProfiles
	if len(data.Controls) == 0 {
		// Use this to flag a profile that seems incomplete. Probably skipped or failed.
		// We just store it for the profile only metadata, looking to replace it when the same profile
		// is ingested that contains controls metadata as well
		data.DocVersion = "0"
	}
	err := backend.addDataToIndexWithID(ctx, mapping, data.Sha256, data)
	return err
}

func (backend *ESClient) InsertComplianceRunInfo(ctx context.Context, nodeId string, runDateTime time.Time) error {
	mapping := mappings.ComplianceRunInfo
	err := backend.upsertComplianceRunInfo(ctx, mapping, nodeId, runDateTime)
	return err
}

// Sets the 'day_latest' field to 'false' for all reports (except <reportId>) of node <nodeId> from yesterday upto 90 days UTC index.
func (backend *ESClient) UpdateDayLatestToFalse(ctx context.Context, nodeId string, reportId string, index string, mapping mappings.Mapping) error {
	termQueryDayLatestTrue := elastic.NewTermQuery("day_latest", true)
	termQueryThisNode := elastic.NewTermsQuery("node_uuid", nodeId)
	termQueryNotThisReport := elastic.NewTermsQuery("_id", reportId)

	boolQueryDayLatestThisNodeNotThisReport := elastic.NewBoolQuery().
		Must(termQueryDayLatestTrue).
		Must(termQueryThisNode).
		MustNot(termQueryNotThisReport)

	script := elastic.NewScript("ctx._source.day_latest = false")

	oneDayAgo := time.Now().Add(-24 * time.Hour)

	time90daysAgo := time.Now().Add(-24 * time.Hour * 90)

	// Making a filter query to get all the indices from today to 90 days back
	filters := map[string][]string{"start_time": {time90daysAgo.Format(time.RFC3339)}, "end_time": {oneDayAgo.Format(time.RFC3339)}}

	// Getting all the indices
	esIndexs, err := relaxting.GetEsIndex(filters, false)
	if err != nil {
		logrus.Errorf("Cannot get indexes: %+v", err)
		return err
	}

	// Updating in all the Indices
	_, err = elastic.NewUpdateByQueryService(backend.client).
		Index(esIndexs).
		Query(boolQueryDayLatestThisNodeNotThisReport).
		Script(script).
		Refresh("false").
		Do(ctx)

	return errors.Wrap(err, "setYesterdayLatestToFalse")
}

// Sets the 'daily_latest' and 'day_latest' fields to 'false' for all reports (except <reportId>) of node <nodeId> in ES index <index>
// setLatestsToFalse targets only one ES UTC index
func (backend *ESClient) setLatestsToFalse(ctx context.Context, nodeId string, reportId string, index string) error {
	termQueryDailyLatestTrue := elastic.NewTermQuery("daily_latest", true)
	termQueryThisNode := elastic.NewTermsQuery("node_uuid", nodeId)
	termQueryNotThisReport := elastic.NewTermsQuery("_id", reportId)

	boolQueryDailyLatestThisNodeNotThisReport := elastic.NewBoolQuery().
		Must(termQueryDailyLatestTrue).
		Must(termQueryThisNode).
		MustNot(termQueryNotThisReport)

	script := elastic.NewScript(`
		ctx._source.daily_latest = false;
		ctx._source.day_latest = false
	`)

	//source, _ := boolQueryDailyLatestThisNodeNotThisReport.Source()
	//relaxting.LogQueryPartMin(index, source, "UPDATE")

	_, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Query(boolQueryDailyLatestThisNodeNotThisReport).
		Script(script).
		Refresh("false").
		Do(ctx)

	return errors.Wrap(err, "setLatestsToFalse")
}

func (backend *ESClient) UpdateProjectTags(ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) ([]string, error) {
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

func (backend *ESClient) UpdateReportProjectsTags(ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) (string, error) {
	index := fmt.Sprintf("%s-%s", mappings.ComplianceRepDate.Index, "*")
	return backend.UpdateReportProjectsTagsForIndex(ctx, index, projectTaggingRules)
}

func (backend *ESClient) UpdateReportProjectsTagsForIndex(ctx context.Context, index string, projectTaggingRules map[string]*authz.ProjectRules) (string, error) {

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
						if (ctx._source['roles'] == null) {
							match = false;
							break;
						}
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
						if (ctx._source['chef_tags'] == null) {
							match = false;
							break;
						}
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

	startTaskResult, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		Refresh("true").
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)
	if err != nil {
		return "", err
	}
	return startTaskResult.TaskId, nil
}

func (backend *ESClient) UpdateSummaryProjectsTags(ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) (string, error) {
	index := fmt.Sprintf("%s-%s", mappings.ComplianceSumDate.Index, "*")
	return backend.UpdateSummaryProjectsTagsForIndex(ctx, index, projectTaggingRules)
}

func (backend *ESClient) UpdateSummaryProjectsTagsForIndex(ctx context.Context, index string, projectTaggingRules map[string]*authz.ProjectRules) (string, error) {

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
						if (ctx._source['roles'] == null) {
							match = false;
							break;
						}
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
						if (ctx._source['chef_tags'] == null) {
							match = false;
							break;
						}
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

	startTaskResult, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Script(elastic.NewScript(script).Params(convertProjectTaggingRulesToEsParams(projectTaggingRules))).
		Refresh("true").
		WaitForCompletion(false).
		ProceedOnVersionConflict().
		DoAsync(ctx)
	if err != nil {
		return "", err
	}
	return startTaskResult.TaskId, nil
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

	percentageComplete, ok := getPercentageComplete(tasksGetTaskResponse.Task.Status)

	if !ok {
		return project_update_lib.JobStatus{
			Completed:             tasksGetTaskResponse.Completed,
			PercentageComplete:    0,
			EstimatedEndTimeInSec: estimatedEndTimeInSec,
		}, nil
	}

	// If the task is marked complete but the percentage complete is not 1 then the task stopped unexpectedly
	if tasksGetTaskResponse.Completed && percentageComplete != 1 {
		logrus.Errorf("Task %s stopped unexpectedly. "+
			"For more information go to http://localhost:10141/.tasks/task/%s", jobID, jobID)

		return project_update_lib.JobStatus{}, fmt.Errorf("Task %s stopped unexpectedly. "+
			"For more information go to http://localhost:10141/.tasks/task/%s", jobID, jobID)
	}

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

func getPercentageComplete(status interface{}) (float64, bool) {
	statusMap, ok := status.(map[string]interface{})
	if !ok {
		return 0, false
	}

	created, ok := statusMap["created"].(float64)
	if !ok {
		return 0, false
	}

	deleted, ok := statusMap["deleted"].(float64)
	if !ok {
		return 0, false
	}

	updated, ok := statusMap["updated"].(float64)
	if !ok {
		return 0, false
	}

	conflicts, ok := statusMap["version_conflicts"].(float64)
	if !ok {
		return 0, false
	}

	noops, ok := statusMap["noops"].(float64)
	if !ok {
		return 0, false
	}

	total, ok := statusMap["total"].(float64)
	if !ok {
		return 0, false
	}

	if total == 0 {
		return 1, true
	}

	return (created + deleted + updated + conflicts + noops) / total, true
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
				environments := []string{}
				roles := []string{}
				chefTags := []string{}
				policyGroups := []string{}
				policyNames := []string{}
				switch condition.Attribute {
				case authz.ProjectRuleConditionAttributes_CHEF_SERVER:
					chefServers = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
					organizations = condition.Values
				case authz.ProjectRuleConditionAttributes_ENVIRONMENT:
					environments = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_ROLE:
					roles = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_TAG:
					chefTags = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP:
					policyGroups = condition.Values
				case authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME:
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

func (backend *ESClient) GetDocByReportUUId(ctx context.Context, reportUuid string, index string) (*relaxting.ESInSpecReport, error) {
	logrus.Debug("Fetching project by UUID")

	var item relaxting.ESInSpecReport
	boolQuery := elastic.NewBoolQuery()

	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportUuid)
	boolQuery = boolQuery.Must(idsQuery)
	fsc := elastic.NewFetchSourceContext(true)
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(1)

	searchResult, err := backend.client.Search().
		SearchSource(searchSource).
		Index(index).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(ctx)

	if err != nil {
		return nil, err
	}

	if searchResult.TotalHits() > 0 {
		logrus.Printf("Found a total of %d ESInSpecReport\n", searchResult.TotalHits())
		// Iterate through results
		for _, hit := range searchResult.Hits.Hits {
			// hit.Index contains the name of the index
			if hit.Source != nil {
				// Deserialize hit.Source into a ESInSpecReport (could also be just a map[string]interface{}).
				err := json.Unmarshal(hit.Source, &item)
				if err != nil {
					logrus.Errorf("Received error while unmarshling %+v", err)
				}

			}

		}
	}

	return &item, nil
}

func (backend *ESClient) CheckIfControlIdExistsForToday(docId string, indexToday string) (bool, error) {
	logrus.Debugf("Checking the control document exists for today with doc Id :%s", docId)
	fsc := elastic.NewFetchSourceContext(false)
	boolQuery := elastic.NewBoolQuery()
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(docId)
	boolQuery = boolQuery.Must(idsQuery)
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(1)
	searchResult, err := backend.client.Search().
		SearchSource(searchSource).
		Index(indexToday).
		Do(context.Background())
	if err != nil {
		switch {
		case elastic.IsTimeout(err):
			logrus.Errorf("Timeout retrieving document: %v", err)
			return false, err
		default:
			logrus.Errorf("Received error: %v", err)
			return false, err
		}
	}

	if searchResult.TotalHits() > 0 {
		// Iterate through results
		for _, hit := range searchResult.Hits.Hits {
			// hit.Index contains the id of the index
			if len(hit.Id) > 0 {
				logrus.Debugf("Found the document with for control with doc Id %s", docId)
				return true, nil
			}

		}
	}
	return false, nil

}

func (backend *ESClient) UploadDataToControlIndex(ctx context.Context, reportuuid string, controls []relaxting.Control, endTime time.Time) error {
	mapping := mappings.ComplianceControlRepData
	index := mapping.IndexTimeseriesFmt(endTime)

	bulkRequest := backend.client.Bulk()
	for _, control := range controls {
		docId := GetDocIdByControlIdAndProfileID(control.ControlID, control.Profile.ProfileID)
		err := backend.SetDailyLatestToFalseForControlIndex(ctx, control.ControlID, control.Profile.ProfileID, mapping, index, control.Nodes[0].NodeUUID)
		if err != nil {
			logrus.Errorf("Unable to SetDailyLatestToFalseForControlIndex %v", err)
		}
		err = backend.SetDayLatestToFalseForControlIndex(ctx, control.ControlID, control.Profile.ProfileID, mapping, index, control.Nodes[0].NodeUUID)
		if err != nil {
			logrus.Errorf("Unable to set Day Latest To false for control index %v", err)
		}
		found, err := backend.CheckIfControlIdExistsForToday(docId, index)
		if err != nil {
			logrus.Errorf("Unable to fetch document for control id %s|%s", control.ControlID, control.Profile.ProfileID)
		}
		if found {
			bulkRequest = bulkRequest.Add(elastic.NewBulkUpdateRequest().Index(index).Id(docId).Script(createScriptForAddingNode(control.Nodes[0])).Type("_doc"))
			continue
		}
		bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Id(docId).Doc(control).Type("_doc"))
	}
	approxBytes := bulkRequest.EstimatedSizeInBytes()
	bulkResponse, err := bulkRequest.Refresh("false").Do(ctx)
	if err != nil {
		logrus.Errorf("Unable to send the request in bulk for reportuuid :%s with error :%v", reportuuid, err)
		return err
	}
	if bulkResponse == nil {
		logrus.Errorf("Unable to fetch the response of bulk request reportuuid id:%s with error :%v", reportuuid, err)
		return err
	}

	logrus.Debugf("Bulk insert %d summaries, ~size %dB, took %dms", len(controls), approxBytes, bulkResponse.Took)
	return nil

}

func GetDocIdByControlIdAndProfileID(controlID string, profileID string) string {
	return fmt.Sprintf("%s|%s", controlID, profileID)
}

func createScriptForAddingNode(node relaxting.Node) *elastic.Script {
	params := make(map[string]interface{})
	params["node"] = node

	return elastic.NewScript("if (!(ctx._source.nodes instanceof Collection)) {ctx._source.nodes = [ctx._source.nodes];} ctx._source.nodes.add(params.node)").Params(params)

}

// Sets the 'day_latest' field to 'false' for all control index
// This way, the last 90 days is covered
func (backend *ESClient) SetDayLatestToFalseForControlIndex(ctx context.Context, controlId string, profileId string, mapping mappings.Mapping, index string, nodeId string) error {
	termQueryThisControl := elastic.NewTermsQuery("_id", GetDocIdByControlIdAndProfileID(controlId, profileId))

	boolQueryDayLatest := elastic.NewBoolQuery().
		Must(termQueryThisControl)

	script := elastic.NewScript(`
	def targets = ctx._source.nodes.findAll(node -> node.node_uuid == params.node_uuid);
	for(node in targets) { 
		if(node.day_latest==true) {
			node.day_latest = false
		}
	}
	if (ctx._source.day_latest==true) {
		ctx._source.day_latest = false;
	}`).Param("node_uuid", nodeId)

	// Getting the date before 90 days
	time90daysAgo := time.Now().Add(-24 * time.Hour * 90)
	// Getting all the indices for past 90 days
	esIndexes, err := relaxting.IndexDates(relaxting.CompDailyControlIndexPrefix, time90daysAgo.Format(time.RFC3339), time.Now().Add(-24*time.Hour).Format(time.RFC3339))
	if err != nil {
		logrus.Errorf("Cannot get indexes: %+v", err)
		return err
	}
	// Updating in all the Indices
	_, err = elastic.NewUpdateByQueryService(backend.client).
		Index(esIndexes).
		Query(boolQueryDayLatest).
		Script(script).
		Refresh("false").
		Do(ctx)
	return errors.Wrap(err, "SetDayLatestsToFalseorControlIndex")
}

// Sets the 'daily_latest'  fields to 'false' for new control index
// This targets only one ES UTC index
func (backend *ESClient) SetDailyLatestToFalseForControlIndex(ctx context.Context, controlId string, profileId string, mapping mappings.Mapping, index string, nodeId string) error {
	termQueryThisControl := elastic.NewTermsQuery("_id", GetDocIdByControlIdAndProfileID(controlId, profileId))

	boolQueryDailyLatest := elastic.NewBoolQuery().
		Must(termQueryThisControl)

	// Script to find the nodes and making daily_latest as false
	script := elastic.NewScript(`
		 def targets = ctx._source.nodes.findAll(node -> node.node_uuid == params.node_uuid);
		 for(node in targets) { 
			 if(node.daily_latest==true) {
				 node.daily_latest = false
			 }
		 }`).Param("node_uuid", nodeId)

	// Updating in all the Indices
	_, err := elastic.NewUpdateByQueryService(backend.client).
		Index(index).
		Query(boolQueryDailyLatest).
		Script(script).
		Refresh("false").
		Do(ctx)
	return errors.Wrap(err, "SetDailyLatestToFalseForControlIndex")
}
