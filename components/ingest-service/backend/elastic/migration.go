package elastic

import (
	"encoding/json"

	"context"

	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

// DoesAliasExists - does the alias 'aliasName' exists in elasticsearch
func (es *Backend) DoesAliasExists(ctx context.Context, aliasName string) bool {
	// TODO @afiune We could use CatAliasesResponse if we upgrade.
	// => https://github.com/olivere/elastic/blob/release-branch.v6/cat_aliases.go#L158-L169
	aliasesResult, err := elastic.NewAliasesService(es.client).Index(aliasName).Do(ctx)
	if err != nil {
		return false
	}

	for _, result := range aliasesResult.Indices {
		for _, alias := range result.Aliases {
			if alias.AliasName == aliasName {
				return true
			}
		}
	}
	return false
}

// DoesIndexExists - does the index 'indexName' exists in elasticsearch
func (es *Backend) DoesIndexExists(ctx context.Context, indexName string) (bool, error) {
	return es.client.IndexExists(indexName).Do(ctx)
}

// DeleteTemplate - delete the template with name 'templateName'
// DELETE /_template/node-state
func (es *Backend) DeleteTemplate(ctx context.Context, templateName string) error {
	exists, err := es.client.IndexTemplateExists(templateName).Do(ctx)
	if err != nil {
		return err
	}

	if exists {
		indicesDeleteTemplateResponse, err := es.client.IndexDeleteTemplate(templateName).Do(ctx)

		log.WithFields(log.Fields{
			"Acknowledged": indicesDeleteTemplateResponse.Acknowledged,
			"Index":        indicesDeleteTemplateResponse.Index}).
			Info("DeleteTemplate")

		return err
	}

	return nil
}

// DeleteIndex - delete index with name 'index'
func (es *Backend) DeleteIndex(ctx context.Context, index string) error {
	_, err := es.client.DeleteIndex(index).Do(ctx)
	return err
}

// ReindexInsightstoActions -reindex the A1 insights indexes to the actions indexes
// Below is a link to how the actions were created in A1
// https://github.com/chef/automate/blob/master/logstash/config/10-action-filter.conf
func (es *Backend) ReindexInsightstoActions(ctx context.Context,
	insightsIndexName string, actionsIndexName string) error {
	script := `
		ctx._source.remove('source_fqdn');
		ctx._source.message_version = ctx._source['@version'];
		ctx._source.remove('@version');
		if (ctx._source['@uuid'] == '' || ctx._source['@uuid'] == null) {
			ctx._source.id = ctx._id;
		} else {
			ctx._source.id = ctx._source['@uuid'];
			ctx._source.remove('@uuid');
		}
		ctx._source.remove('@uuid');
		ctx._source.message_type = 'action';
		ctx._source.revision_id = '';
		ctx._source.entity_name = ctx._source[ctx._source.type + '_name'];
		ctx._source.remove(ctx._source.type + '_name');

		// Painless supports all of Java’s control flow statements except the switch statement.
		// Guess what. Here we really needed it
		if (ctx._source.event_action == 'created') {
			ctx._source.task = 'create';
		}
		if (ctx._source.event_action == 'updated') {
			ctx._source.task = 'update';
		}
		if (ctx._source.event_action == 'deleted') {
			ctx._source.task = 'delete';
		}
		if (ctx._source.event_action == 'associated') {
			ctx._source.task = 'associate';
		}
		ctx._source.remove('event_action');

		if (ctx._source.entity_type == '' || ctx._source.entity_type == null) {
			ctx._source.entity_type = ctx._source.event_type;
		} else {
			ctx._source.parent_type = ctx._source.event_type;
		}
		ctx._source.remove('type');
		ctx._source.remove('@timestamp');
		ctx._source.remove('tags');
		ctx._source.remove('event_type');
		if (ctx._source.entity_type == 'policy' && ctx._source.data != null) {
			ctx._source.revision_id = ctx._source.data.revision_id;
		}
		ctx._source.data = '{}';
	`
	esScript := elastic.NewScript(script).Lang("painless")
	reindexSource := elastic.NewReindexSource().
		Index(insightsIndexName).
		Type("bag", "environment", "policy_group", "cookbook_artifact", "client",
			"cookbook", "role", "node", "group", "organization", "user")

	bulkIndexByScrollResponse, err := es.client.Reindex().
		WaitForCompletion(true).
		Source(reindexSource).
		DestinationIndexAndType(actionsIndexName, "actions").
		Script(esScript).
		Do(ctx)

	if bulkIndexByScrollResponse != nil {
		log.WithFields(log.Fields{
			"Updated":          bulkIndexByScrollResponse.Updated,
			"Created":          bulkIndexByScrollResponse.Created,
			"Canceled":         bulkIndexByScrollResponse.Canceled,
			"Failures":         bulkIndexByScrollResponse.Failures,
			"VersionConflicts": bulkIndexByScrollResponse.VersionConflicts,
			"error":            err,
			"Total":            bulkIndexByScrollResponse.Total}).
			Info("ReindexInsightstoActions")
	}

	return err
}

// ReindexInsightstoConvergeHistory - reindex the A1 insights indexes to the converge-history indexes
//
// TODO: Still need to add the VersionedCookbooks to each run.
func (es *Backend) ReindexInsightstoConvergeHistory(ctx context.Context, insightsIndexName string, convergeHistoryIndexName string) error {
	script := `
		// We don't want an empty string
		if (ctx._source.ipaddress == '') {
			ctx._source.ipaddress = null;
		}

		// renameing 'name' to 'source_name'
		ctx._source.node_name = ctx._source.name;
		ctx._source.remove('name');
		def sf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");

		// Creating a timestamp on when added to the system
		sf.setTimeZone(TimeZone.getTimeZone("GMT"));
		ctx._source.timestamp = sf.format(new Date());
		ctx._source.remove('node');
	`
	esScript := elastic.NewScript(script).Lang("painless")
	boolQuery := elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("event_action", "finished"))

	reindexSource := elastic.NewReindexSource().
		Index(insightsIndexName).
		Type("converge").
		Query(boolQuery)

	bulkIndexByScrollResponse, err := es.client.Reindex().
		WaitForCompletion(true).
		Source(reindexSource).
		DestinationIndexAndType(convergeHistoryIndexName, "converge").
		Script(esScript).
		Size(50). // Reindex batches of 50 documents (AIA-562)
		Do(ctx)

	if bulkIndexByScrollResponse != nil {
		log.WithFields(log.Fields{
			"Updated":          bulkIndexByScrollResponse.Updated,
			"Created":          bulkIndexByScrollResponse.Created,
			"Canceled":         bulkIndexByScrollResponse.Canceled,
			"Failures":         bulkIndexByScrollResponse.Failures,
			"VersionConflicts": bulkIndexByScrollResponse.VersionConflicts,
			"error":            err,
			"Total":            bulkIndexByScrollResponse.Total}).
			Info("ReindexInsightstoConvergeHistory")
	}

	return err
}

// ReindexNodeStateA1 - reindex the A1 node-state index to the current node-state index
func (es *Backend) ReindexNodeStateA1(ctx context.Context, a1NodeStateIndexName string) error {
	script := `
		// We don't want an empty string
		if (ctx._source.ipaddress == '') {
			ctx._source.ipaddress = null;
		}

		// Renameing 'name' to 'source_name'
		ctx._source.node_name = ctx._source.name;
		ctx._source.remove('name');
		ctx._source.remove('compliance_summary');
		ctx._source.latest_run_id = ctx._source.run_id;

		// Renameing 'run_id' to 'latest_run_id'
		ctx._source.remove('run_id');
		ctx._source.event_action = 'Finished';
		def sf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
		sf.setTimeZone(TimeZone.getTimeZone("GMT"));

		// Creating a timestamp on when added to the system
		ctx._source.timestamp = sf.format(new Date());

		// Needed because 'exists' was a string
		if (ctx._source.exists == 'true') {
			ctx._source.exists = true;
		} else {
			ctx._source.exists = false;
		}
	`
	esScript := elastic.NewScript(script).Lang("painless")
	bulkIndexByScrollResponse, err := es.client.Reindex().
		WaitForCompletion(true).
		SourceIndex(a1NodeStateIndexName).
		DestinationIndex(mappings.NodeState.Index).
		Script(esScript).
		Do(ctx)

	if bulkIndexByScrollResponse != nil {
		log.WithFields(log.Fields{
			"Updated":          bulkIndexByScrollResponse.Updated,
			"Created":          bulkIndexByScrollResponse.Created,
			"Canceled":         bulkIndexByScrollResponse.Canceled,
			"Failures":         bulkIndexByScrollResponse.Failures,
			"VersionConflicts": bulkIndexByScrollResponse.VersionConflicts,
			"error":            err,
			"Total":            bulkIndexByScrollResponse.Total}).
			Info("ReindexNodeStateA1toA2")
	}

	es.RefreshIndex(ctx, mappings.NodeState.Index)

	return err
}

// GetAllTimeseriesIndiceNames - get all the indice names for the givin indexName tag.
// Example of indexName = converge-history, insights
func (es *Backend) GetAllTimeseriesIndiceNames(ctx context.Context, indexName string) ([]string, error) {
	res, err := es.client.IndexGetSettings().Index(indexName + "-*").Do(ctx)
	if err != nil {
		return []string{}, err
	}

	var names []string
	for name := range res {
		names = append(names, name)
	}

	return names, nil
}

// GetLatestA1NodeRun - get the newest A1 Node run from the insights index for the givin 'nodeEntityUUID'.
// Return
// InsightsRun - run data if found in elasticsearch
// bool if runs were found.
// Error trying to find runs
func (es *Backend) GetLatestA1NodeRun(ctx context.Context,
	nodeEntityUUID string, a1IndexInsights string) (backend.InsightsRun, bool, error) {
	var run backend.InsightsRun

	boolQuery := elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("entity_uuid", nodeEntityUUID)).
		Must(elastic.NewTermQuery("event_action", "finished"))

	fetchSource := elastic.NewFetchSourceContext(true).Include(
		"run_id",
		"start_time", "end_time",
		"total_resource_count",
		"updated_resource_count",
		"node",
		"resources")

	searchResult, err := es.client.Search().
		Query(boolQuery).
		Size(1).
		Sort("end_time", false).
		FetchSourceContext(fetchSource).
		Index(a1IndexInsights).
		Type("converge").
		Do(ctx)

	if err != nil {
		return run, false, err
	}

	if searchResult.Hits.TotalHits == 0 {
		return run, false, nil
	}

	source := searchResult.Hits.Hits[0].Source
	err = json.Unmarshal(*source, &run)
	if err != nil {
		return run, false, err
	}

	return run, true, nil
}

func (es *Backend) RefreshIndex(ctx context.Context, indexName string) error {
	_, err := es.client.Refresh(indexName).Do(ctx)

	return err
}

// EmptyNodeLatestRunID - Set the nodes latest Run ID to a empty string
func (es *Backend) EmptyNodeLatestRunID(ctx context.Context, nodeEntityUUID string) error {
	_, err := es.client.Update().
		Index(mappings.NodeState.Index).
		Type("node-state").
		Id(nodeEntityUUID).
		Script(elastic.NewScript("ctx._source.latest_run_id = ''")).
		Do(ctx)

	return err
}

// UpdateNode - Update a node-state doc with the givin data.
func (es *Backend) UpdateNode(ctx context.Context, nodeEntityUUID string, insightsRun backend.InsightsRun,
	versionedCookbooks []backend.VersionedCookbook) error {
	script := `
		ctx._source.start_time = params.start_time;
		ctx._source.latest_run_id = params.run_id;
		ctx._source.end_time = params.end_time;
		ctx._source.total_resource_count = params.total_resource_count;
		ctx._source.updated_resource_count = params.updated_resource_count;
		ctx._source.resources = params.resources;
		ctx._source.last_ccr_received = params.end_time;
		ctx._source.versioned_cookbooks = params.versioned_cookbooks;
	`
	esScript := elastic.NewScript(script).
		Param("start_time", insightsRun.StartTime).
		Param("end_time", insightsRun.EndTime).
		Param("total_resource_count", insightsRun.TotalResourceCount).
		Param("updated_resource_count", insightsRun.UpdatedResourceCount).
		Param("resources", insightsRun.Resources).
		Param("run_id", insightsRun.RunID).
		Param("versioned_cookbooks", versionedCookbooks)

	_, err := es.client.Update().
		Index(mappings.NodeState.Index).
		Type("node-state").
		Id(nodeEntityUUID).
		Script(esScript).
		Do(ctx)

	return err
}

// GetNodeIds - Get the 100 node IDs & Checkin times after the cursorID. 'SearchAfter' is used because we can not pull all the nodes at once.
// If there where 10,000 nodes elasticsearch would not allow us to pull them all at once.
func (es *Backend) GetNodeBasics(ctx context.Context, cursorID string) ([]backend.NodeBasics, error) {
	fetchSource := elastic.NewFetchSourceContext(true).Include("entity_uuid", "checkin")

	searchService := es.client.Search().
		Index(mappings.NodeState.Index).Size(100).
		FetchSourceContext(fetchSource).
		Sort("entity_uuid", true)

	if cursorID != "" {
		searchService = searchService.SearchAfter(cursorID)
	}

	searchResult, err := searchService.Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	nodeBasicsCollection := make([]backend.NodeBasics, 0, searchResult.Hits.TotalHits)
	var n backend.NodeBasics
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			err := json.Unmarshal(*hit.Source, &n)
			if err != nil {
				log.WithFields(log.Fields{"error": err}).
					Error("Error unmarshalling the node object")
			}

			nodeBasicsCollection = append(nodeBasicsCollection, backend.NodeBasics{EntityUuid: n.EntityUuid, Checkin: n.Checkin})
		}
	}

	return nodeBasicsCollection, nil
}

// GetInsightsRunData -
func (es *Backend) GetInsightsRunData(ctx context.Context, insightsIndex string, cursorID string) ([]backend.InsightsRunNodePayLoadData, error) {
	fetchSource := elastic.NewFetchSourceContext(true).Include("run_id", "node")

	searchService := es.client.Search().
		Index(insightsIndex).Size(100).
		FetchSourceContext(fetchSource).
		Sort("run_id", true)

	if cursorID != "" {
		searchService = searchService.SearchAfter(cursorID)
	}

	searchResult, err := searchService.Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var insightsRunNodePayLoadDataCollection []backend.InsightsRunNodePayLoadData
	var run backend.InsightsRunNodePayLoadData
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			err := json.Unmarshal(*hit.Source, &run)
			if err != nil {
				log.WithFields(log.Fields{"error": err}).
					Error("Error unmarshalling the node object")
			}

			insightsRunNodePayLoadDataCollection = append(insightsRunNodePayLoadDataCollection, run)
		}
	}

	return insightsRunNodePayLoadDataCollection, nil
}

// RemoveAlias - remove an alias from the index named 'indexName'
func (es *Backend) RemoveAlias(ctx context.Context, aliasName string, indexName string) error {
	_, err := es.client.Alias().Remove(indexName, aliasName).Do(ctx)

	return err
}

//GetNodeCount - count how many node-state documents are in an index
func (es *Backend) GetNodeCount(ctx context.Context, indexName string) (int64, error) {
	count, err := es.client.Count(indexName).Type("node-state").Do(ctx)
	if err != nil {
		log.WithError(err).Error("Error retrieving nodes counts")
		return 0, err
	}
	return count, err
}
