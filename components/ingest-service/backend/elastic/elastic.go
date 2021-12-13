package elastic

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	project_update_lib "github.com/chef/automate/lib/authz"
)

const (
	// Elasticsearch fields we use within this package
	fieldCheckin = "checkin"
	timestamp    = "timestamp"
	nodeExists   = "exists"
)

type Backend struct {
	initialized bool
	client      *elastic.Client
}

func (es *Backend) createBulkRequestUpsertNodeInfo(
	mapping mappings.Mapping,
	ID string,
	runDateTime time.Time,
) elastic.BulkableRequest {
	runDateTimeAsString := runDateTime.Format(time.RFC3339)

	script := elastic.NewScript("ctx._source.last_run = params.rundate").Param("rundate", runDateTimeAsString)

	return elastic.NewBulkUpdateRequest().
		Index(mapping.Index).
		Type(mapping.Type).
		Id(ID).
		Script(script).
		Upsert(map[string]interface{}{
			"node_uuid": ID,
			"first_run": runDateTime,
			"last_run":  runDateTime,
		}).
		RetryOnConflict(3)
}

// This method will support adding a document with a specified id
func (es *Backend) upsertNodeRunInfo(ctx context.Context, mapping mappings.Mapping, id string, runDateTime time.Time) error {
	runDateTimeAsString := runDateTime.Format(time.RFC3339)

	script := elastic.NewScript("ctx._source.last_run = params.rundate").Param("rundate", runDateTimeAsString)

	_, err := es.client.Update().
		Index(mapping.Index).
		Type(mapping.Type).
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

func (es *Backend) InsertNodeRunDateInfo(ctx context.Context, nodeInfo backend.Run) error {
	mapping := mappings.NodeRunInfo
	err := es.upsertNodeRunInfo(ctx, mapping, nodeInfo.EntityUuid, nodeInfo.EndTime)
	return err
}

func New(esURL string) (*Backend, error) {
	client, err := elastic.NewClient(
		// In the future we will need to create a custom http.Client to pass headers.
		// => (ent? user? token?)
		//elastic.SetHttpClient(httpClient),
		elastic.SetURL(esURL),
		elastic.SetSniff(false),
	)
	return &Backend{false, client}, err
}

func (es *Backend) Initializing() bool {
	return !es.initialized
}

func (es *Backend) UpdateProjectTags(ctx context.Context, projectTaggingRules map[string]*authz.ProjectRules) ([]string, error) {
	log.Debug("starting project updater")

	esNodeJobID, err := es.UpdateNodeProjectTags(ctx, projectTaggingRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	log.Debugf("Started Project rule updates with node job ID: %q", esNodeJobID)

	return []string{esNodeJobID}, nil
}

func (es *Backend) JobStatus(ctx context.Context, jobID string) (project_update_lib.JobStatus, error) {
	tasksGetTaskResponse, err := elastic.NewTasksGetTaskService(es.client).
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
		log.Errorf("Task %s stopped unexpectedly. "+
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

func (es *Backend) JobCancel(ctx context.Context, jobID string) error {
	_, err := elastic.NewTasksCancelService(es.client).
		TaskId(jobID).
		Do(ctx)
	return err
}

// Adds data to specified index, which will have timeseries string added to the end of it
// Allows elasticsearch generate a flake ID for the document.
func (es *Backend) addDataToTimeseriesIndex(ctx context.Context,
	mapping mappings.Mapping,
	date time.Time,
	data interface{}) error {

	index := mapping.IndexTimeseriesFmt(date)
	// Wrapping the ES query to measure
	f := func() error {
		// Add a document on a particular index and let elasticsearch generate flake id
		_, err := es.client.Index().
			Index(index).
			Type(mapping.Type).
			BodyJson(data).
			Do(ctx)
		return err
	}

	// Fields to identify the metric
	fields := log.Fields{
		"metric": "elasticsearch",
		"type":   "doc_insert",
		"index":  index,
	}

	// Clock func and return the error
	return backend.ClockFnWithFields(f, fields)
}

func (es *Backend) createBulkUpdateRequestToTimeseriesIndex(
	mapping mappings.Mapping,
	date time.Time,
	data interface{}) elastic.BulkableRequest {
	index := mapping.IndexTimeseriesFmt(date)
	return elastic.NewBulkIndexRequest().Index(index).Type(mapping.Type).Doc(data)
}

func (es *Backend) SendBulkRequest(ctx context.Context, bulkUpdateRequests []elastic.BulkableRequest) error {
	f := func() error {
		_, err := es.client.Bulk().Add(bulkUpdateRequests...).Do(ctx)
		return err
	}

	// Fields to identify the metric
	fields := log.Fields{
		"metric":   "elasticsearch",
		"type":     "bulk_insert",
		"requests": len(bulkUpdateRequests),
	}

	// Clock func and return the error
	return backend.ClockFnWithFields(f, fields)
}

func (es *Backend) upsertDataWithID(ctx context.Context,
	mapping mappings.Mapping,
	ID string,
	data interface{}) error {
	// Wrapping the ES query to measure
	f := func() error {
		// Upsert a document on a particular index and a particular id
		// This is for non-timeseries indices
		// Uses doc as upsert
		_, err := es.client.Update().
			Index(mapping.Index).
			Id(ID).
			Type(mapping.Type).
			Doc(data).
			DocAsUpsert(true).
			RetryOnConflict(3).
			Do(ctx)
		return err
	}

	// Fields to identify the metric
	fields := log.Fields{
		"metric": "elasticsearch",
		"type":   "doc_insert",
		"index":  mapping.Index,
	}

	// Clock func and return the error
	return backend.ClockFnWithFields(f, fields)
}

func (es *Backend) createBulkRequestUpsertDataWithID(
	mapping mappings.Mapping,
	ID string,
	data interface{}, upsertData interface{}) elastic.BulkableRequest {
	return elastic.NewBulkUpdateRequest().
		Index(mapping.Alias).
		Type(mapping.Type).
		Id(ID).
		Upsert(upsertData). // Data used when the node is first created
		Doc(data).          // Data used to update the node
		RetryOnConflict(3)
}

// This method will support adding a document with a specified ID
// giving the ID is important if you want to be able to update the document later
func (es *Backend) addDataToIndexWithID(ctx context.Context,
	mapping mappings.Mapping,
	ID string,
	data interface{}) error {

	// Wrapping the ES query to measure
	f := func() error {
		// Add a document on a particular index and a particular id
		// This is not creating an index with a timestring at the end
		_, err := es.client.Index().
			Index(mapping.Index).
			Id(ID).
			Type(mapping.Type).
			BodyJson(data).
			Do(ctx)
		return err
	}

	// Fields to identify the metric
	fields := log.Fields{
		"metric": "elasticsearch",
		"type":   "doc_insert",
		"index":  mapping.Index,
	}

	// Clock func and return the error
	return backend.ClockFnWithFields(f, fields)
}

func (es *Backend) createBulkUpdateRequestToIndexWithID(
	mapping mappings.Mapping,
	ID string,
	data interface{}) elastic.BulkableRequest {
	return elastic.NewBulkUpdateRequest().
		Index(mapping.Index).
		Type(mapping.Type).
		Id(ID).
		Doc(data).DocAsUpsert(true)
}

// InitializeStore runs the necessary initialization processes to make elasticsearch usable
// in particular it creates the indexes and aliases for documents to be added
func (es *Backend) InitializeStore(ctx context.Context) error {
	if !es.initialized {
		for _, esMap := range mappings.AllMappings {
			if esMap.Timeseries {
				err := es.createTemplate(ctx, esMap.Index, esMap.Mapping)
				if err != nil {
					return err
				}
			} else {
				err := es.createOrUpdateStore(ctx, esMap)
				if err != nil {
					return err
				}

				err = es.createStoreAliasIfNotExists(ctx, esMap.Alias, esMap.Index)
				if err != nil {
					return err
				}
			}
		}
	}
	es.initialized = true

	return nil
}

func (es *Backend) createStoreAliasIfNotExists(ctx context.Context, alias string, index string) error {
	if len(alias) > 0 {
		if !es.storeExists(ctx, alias) {
			indexError := es.CreateAlias(ctx, alias, index)
			if indexError != nil {
				return errors.Errorf("Error creating alias %s with error: %s", alias, indexError.Error())
			}
		}
	}

	return nil
}

func (es *Backend) createTemplate(ctx context.Context, templateName string, mapping string) error {
	// We don't care if it already exists because it will update the template
	_, err := es.client.IndexPutTemplate(templateName).BodyString(mapping).Do(ctx)
	if err != nil {
		return errors.Errorf("Error creating index %s with error: %s", templateName, err.Error())
	}
	return nil
}

func (es *Backend) createOrUpdateStore(ctx context.Context, esMap mappings.Mapping) error {
	indexName := esMap.Index
	if !es.storeExists(ctx, indexName) {
		indexError := es.createStore(ctx, indexName, esMap.Mapping)
		if indexError != nil {
			return errors.Errorf("Error creating index %s with error: %s", indexName, indexError.Error())
		}
	} else {
		// Ensure we have the latest mapping registered.
		indexError := es.updateMapping(ctx, esMap)
		if indexError != nil {
			return errors.Errorf("Error creating index %s with error: %s", indexName, indexError.Error())
		}
	}

	return nil
}

func (es *Backend) CreateAlias(ctx context.Context, aliasName string, indexName string) error {
	_, err := es.client.Alias().Add(indexName, aliasName).Do(ctx)

	return err
}

func (es *Backend) createStore(ctx context.Context, indexName string, mapping string) error {
	_, err := es.client.CreateIndex(indexName).Body(mapping).Do(ctx)

	return err
}

func (es *Backend) updateMapping(ctx context.Context, esMap mappings.Mapping) error {
	_, err := es.client.PutMapping().Index(esMap.Index).Type(esMap.Type).BodyString(esMap.Properties).Do(ctx)
	return err
}

func (es *Backend) ReindexNodeStateToLatest(ctx context.Context, previousIndex string) (string, error) {
	src := elastic.NewReindexSource().Index(previousIndex)
	dst := elastic.NewReindexDestination().Index(mappings.NodeState.Index)
	startTaskResult, err := es.client.Reindex().Source(src).Destination(dst).DoAsync(ctx)
	if err != nil {
		return "", err
	}
	return startTaskResult.TaskId, nil
}

func (es *Backend) storeExists(ctx context.Context, indexName string) bool {
	exists, err := es.client.IndexExists().Index([]string{indexName}).Do(ctx)

	if err != nil {
		log.Errorf("Error checking if index %s exists with error %s", indexName, err.Error())
	}

	return exists
}

func newBoolQueryFromFilters(filters map[string]string) *elastic.BoolQuery {
	boolQuery := elastic.NewBoolQuery()
	for field, value := range filters {
		if len(value) > 0 {
			termQuery := elastic.NewTermsQuery(field, value)
			boolQuery = boolQuery.Must(termQuery)
		}
	}
	return boolQuery
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

	total, ok := statusMap["total"].(float64)
	if !ok {
		return 0, false
	}

	if total == 0 {
		return 1, true
	}

	return (created + deleted + updated) / total, true
}
