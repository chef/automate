package migration

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

var (
	insightsIndexNameTag = "insights"
	indexDatePattern     = regexp.MustCompile(`\d{4}\.\d{2}\.\d{2}`)
)

func (ms *Status) migrateA1ToCurrent() error {
	ms.calculateA1Tasks()

	ms.update("Starting Stage 1")
	err := ms.stage1()
	if err != nil {
		return err
	}

	ms.update("Starting Stage 2")
	go ms.stage2()

	return nil
}

func (ms *Status) calculateA1Tasks() {
	// Total tasks: For now we are calculating this manually so we have to make sure
	// we update this total task number if we modify the migration
	//
	// The State 1 is one single task
	ms.total = 1

	// 1) reindexNodeState
	ms.total++

	// 2) updateNodesWithMissingRunData
	nodeCount, err := ms.client.GetNodeCount(ms.ctx, "node-state")
	if err != nil {
		logWarning(err.Error(), "Unable to determine node count")
	} else {
		ms.total = ms.total + nodeCount
		ms.update("Found " + strconv.Itoa(int(nodeCount)) + " nodes to migrate")
	}

	// 3) reindexAllInsightsIndexes
	insightsIndices, _ := ms.client.GetAllTimeseriesIndiceNames(ms.ctx, insightsIndexNameTag)
	ms.total = ms.total + int64(len(insightsIndices))

	// 4) finalCleanup
	ms.total++
}

// stage1 - this is all done before the service says it is healthy.
func (ms *Status) stage1() error {
	err := ms.client.DeleteTemplate(ms.ctx, mappings.NodeState.Alias)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch failed to delete Automate 1.x node-state template")
	}

	err = ms.client.RemoveAlias(ms.ctx, mappings.NodeState.Alias, a1NodeStateIndexName)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch removing '"+a1NodeStateIndexName+"' alias")
	}

	ms.client.InitializeStore(ms.ctx)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch indices failed to initialize")
		return err
	}

	err = ms.client.CreateAlias(ms.ctx, mappings.NodeState.Alias, mappings.NodeState.Index)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch creating '"+mappings.NodeState.Index+"' alias")
	}

	ms.taskCompleted()
	return nil
}

// Stage 2. Start the background task of reindexing the insights indexes
func (ms *Status) stage2() {
	ms.update("Reindexing node-state")
	ms.reindexNodeState()
	ms.taskCompleted()

	ms.update("Updating missing run data")
	ms.updateNodesWithMissingRunData()

	ms.update("Reindexing insights indexes")
	ms.reindexAllInsightsIndexes()

	ms.update("Finishing migration by deleting old templates and indexes")
	ms.finalCleanup()
	ms.taskCompleted()

	ms.finish("Migration from Automate 1.x finished successfully")
}

func (ms *Status) finalCleanup() {
	err := ms.client.DeleteTemplate(ms.ctx, insightsIndexNameTag)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch indices failed to initialize")
	}

	// delete node-state-2
	ms.client.DeleteIndex(ms.ctx, a1NodeStateIndexName)
}

// Walk each of the nodes and find the latest run from the insights indexes.
// It is pulling 100 nodes at a time from elasticsearch
// With the run, update the below fields on the node-state
// * versioned_cookbooks - in node data
// * resources
// * event_action
// * start_time
// * end_time
// * total_resource_count
// * updated_resource_count
func (ms *Status) updateNodesWithMissingRunData() {
	nodeBasicsCollection, err := ms.client.GetNodeBasics(ms.ctx, "")
	if err != nil {
		logFatal(err.Error(), "Elasticsearch could not collect nodes")
		return
	}

	for len(nodeBasicsCollection) > 0 {
		for _, nodeBasic := range nodeBasicsCollection {
			var (
				nodePayload        backend.NodePayload
				versionedCookbooks []backend.VersionedCookbook
			)
			nodeUUID := nodeBasic.EntityUuid
			checkin := nodeBasic.Checkin.Format("2006.01.02")
			a1IndexInsights := insightsIndexNameTag + "-" + checkin
			run, runsFound, err := ms.client.GetLatestA1NodeRun(ms.ctx, nodeUUID, a1IndexInsights)
			if err != nil {
				logWarning(err.Error(), "Problem trying to get node runs")
				continue
			}

			if !runsFound {
				// If no run was found with a specific index based on last checkin try the wildcard index
				// this would catch any CCRs that might be liveness managed and have a checkin time updated in a1
				// by a node ping message but the CCR could be a few days older.
				a1WildcardIndex := insightsIndexNameTag + "-*"
				run, runsFound, err = ms.client.GetLatestA1NodeRun(ms.ctx, nodeUUID, a1WildcardIndex)

				if err != nil {
					logWarning(err.Error(), "Unable to get latest a1 node run to update node-state with a2 data for node "+nodeUUID)
					continue
				}

				if !runsFound {
					err = ms.client.EmptyNodeLatestRunID(ms.ctx, nodeUUID)
					if err != nil {
						logWarning(err.Error(), "Could not update node: "+nodeUUID)
					}
					continue
				}
			}

			if run.Node != "" {
				err = json.Unmarshal([]byte(run.Node), &nodePayload)
				if err != nil {
					logWarning(err.Error(), "NodeState: Could not create NodePayload from node data")
				} else {
					err = ms.createNodeAttributeDoc(nodePayload, nodeUUID)
					if err != nil {
						logWarning(err.Error(), "Elasticsearch could not create NodeAttribute")
					}
				}
			}

			versionedCookbooks = backend.VersionedCookbooks(nodePayload)

			err = ms.client.UpdateNode(ms.ctx, nodeUUID, run, versionedCookbooks)
			if err != nil {
				logWarning(err.Error(), "Error on update")
			}
			ms.taskCompleted()
		}

		nodeBasicsCollection, err = ms.client.GetNodeBasics(ms.ctx,
			nodeBasicsCollection[len(nodeBasicsCollection)-1].EntityUuid)
		if err != nil {
			logFatal(err.Error(), "Elasticsearch could not collect nodes")
			return
		}
	}
}

func (ms *Status) createNodeAttributeDoc(nodePayload backend.NodePayload, nodeUUID string) error {
	attribute, err := backend.CreateNodeAttribute(nodePayload, nodeUUID)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch could not create NodeAttribute")
		return err
	}

	err = ms.client.InsertNodeAttribute(ms.ctx, attribute)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch could not insert NodeAttribute")
		return err
	}

	return nil
}

func (ms *Status) reindexNodeState() {
	err := ms.client.ReindexNodeStateA1(ms.ctx, a1NodeStateIndexName)
	if err != nil {
		logWarning(err.Error(), "Elasticsearch reindex of A1 '"+
			a1NodeStateIndexName+"' to A2 '"+mappings.NodeState.Index+"'")
	}
}

func (ms *Status) reindexAllInsightsIndexes() {
	insightsIndiceNames, err := ms.client.GetAllTimeseriesIndiceNames(ms.ctx, insightsIndexNameTag)
	if err != nil {
		logWarning(err.Error(), "ReindexInsightsToConvergeHistory")
		return
	}
	for _, insightsIndexName := range insightsIndiceNames {
		convergeHistoryIndexName := RenameFromInsightsIndexName(insightsIndexName, mappings.ConvergeHistory.Index)
		err := ms.client.ReindexInsightstoConvergeHistory(ms.ctx, insightsIndexName, convergeHistoryIndexName)
		if err != nil {
			logWarning(err.Error(), "ReindexInsightsToConvergeHistory")
		}

		actionsIndexName := RenameFromInsightsIndexName(insightsIndexName, mappings.Actions.Index)
		err = ms.client.ReindexInsightstoActions(ms.ctx, insightsIndexName, actionsIndexName)
		if err != nil {
			logWarning(err.Error(), "ReindexInsightsToActions")
		}

		err = ms.client.DeleteIndex(ms.ctx, insightsIndexName)
		if err != nil {
			logWarning(err.Error(), "DeleteIndex")
		} else {
			logInfo(fmt.Sprintf("Deleted index %s", insightsIndexName))
		}
		ms.taskCompleted()
	}
}

// RenameFromInsightsIndexName - rename from the original insights index name to the new index name
// This must remove extra tags from re-indexes.
func RenameFromInsightsIndexName(insightsIndexName, newIndexTagName string) string {
	dateString := indexDatePattern.FindString(insightsIndexName)
	return newIndexTagName + "-" + dateString
}
