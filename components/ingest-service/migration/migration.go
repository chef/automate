package migration

import (
	"context"
	"errors"
	"fmt"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

var (
	// This variable will allow us to add a test so that if the mappings.NodeState.Index
	// changes we might need to add some migration bits over here
	a2CurrentNodeStateIndex = "node-state-6"

	// NodeState 5 index: This change was made since we are now indexing the 'projects' field
	a2NodeState5Index = "node-state-5"

	// NodeState 4 index: This change was made since we are now indexing the
	// chef_tags and chef_version fields to be engram's (searchable fields)
	a2NodeState4Index = "node-state-4"

	a1NodeStateIndexName     = "node-state-2"
	berlinNodeStateIndexName = "node-state-1"
	nodeStateAliasName       = "node-state"
)

type Status struct {
	total     int64
	completed int64
	status    string
	finished  bool
	ctx       context.Context
	client    backend.Client
}

func New(client backend.Client) *Status {
	return &Status{
		total:     0,
		completed: 0,
		status:    "There is no migration running",
		finished:  false,
		ctx:       context.Background(),
		client:    client,
	}
}

// Start - start the migration from a prior automate to the current automate for only the config-mgmt-service.
//
// We would like to use the Chain-of-responsibility pattern here but cannot because of the
// requirement of two stages of migration. We can not have the second stage start its migration from
// version 1 -> 2 after the first stage has migrated from 1 -> 2 -> ... -> n. This causes a problem because
// the second stage is migration from 1 -> 2 does not know what state the first stage has left the data in.
// For the migration framework we are always going to migrate from the current state to the current version.
// This does cause a maintenance problem because for each new version the old migration stages need to be updated.
func (ms *Status) Start() error {
	exists, err := ms.hasA1ElasticsearchData()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to detect migration status")
		return err
	}
	if exists {
		ms.update("Starting Automate 1.x migration")
		err = ms.migrateA1ToCurrent()
		if err != nil {
			ms.updateErr(err.Error(), "Unable run 1.x to current migration")
			return err
		}
		return nil
	}

	exists, err = ms.hasBerlinElasticsearchData()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to detect migration status")
		return err
	}
	if exists {
		ms.update("Starting Berlin migration")
		err = ms.migrateBerlinToCurrent()
		if err != nil {
			ms.updateErr(err.Error(), "Unable run berlin to current migration")
			return err
		}
		return nil
	}

	exists, err = ms.hasNodeState4Data()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to detect migration status")
		return err
	}
	if exists {
		ms.update("Starting migration to latest node state index")
		err = ms.migrateNodeStateToCurrent(a2NodeState4Index)
		if err != nil {
			ms.updateErr(err.Error(), "Unable run node-state 4 to current migration")
			return err
		}
		return nil
	}

	exists, err = ms.hasNodeState5Data()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to detect migration status")
		return err
	}
	if exists {
		ms.update("Starting migration to latest node state index")
		err = ms.migrateNodeStateToCurrent(a2NodeState5Index)
		if err != nil {
			ms.updateErr(err.Error(), "Unable run node-state 4 to current migration")
			return err
		}
		return nil
	}

	return nil
}

// MigrationNeeded Verify if a migration is needed
func (ms *Status) MigrationNeeded() (bool, error) {
	var (
		nodeStateAliasExists       = ms.client.DoesAliasExists(ms.ctx, nodeStateAliasName)
		a1Exists, err1             = ms.hasA1ElasticsearchData()
		BerlinExists, err2         = ms.hasBerlinElasticsearchData()
		nodeStateIndexExists, err3 = ms.client.DoesIndexExists(ms.ctx, nodeStateAliasName)
		nodeState4Exists, err4     = ms.hasNodeState4Data()
		nodeState5Exists, err5     = ms.hasNodeState5Data()
	)

	if err1 != nil {
		logFatal(err1.Error(), "Error detecting migration status")
	}
	if err2 != nil {
		logFatal(err2.Error(), "Error detecting migration status")
	}
	if err3 != nil {
		logFatal(err3.Error(), "Error detecting migration status")
	}
	if err4 != nil {
		logFatal(err4.Error(), "Error detecting migration status")
	}
	if err5 != nil {
		logFatal(err4.Error(), "Error detecting migration status")
	}

	// If the node-state alias doesn't exist and it is an index
	// instead, we might have corrupted data
	if !nodeStateAliasExists && nodeStateIndexExists {
		err := errors.New(fmt.Sprintf("Alias %q not found", nodeStateAliasName))
		logFatal(err.Error(), "Data might be corrupted")
		return false, err
	}

	if a1Exists || BerlinExists || nodeState4Exists || nodeState5Exists {
		return true, nil
	}

	return false, nil
}

// MarkUnneeded marks the migrations status to done
func (ms *Status) MarkUnneeded() {
	ms.status = "No migration is needed."
	ms.finished = true
}

// taskCompleted increments the number of completed tasks (private)
func (ms *Status) taskCompleted() {
	ms.completed++
}

// CompletedTasks returns the number of tasks completed
func (ms *Status) CompletedTasks() int64 {
	return ms.completed
}

// TotalTasks returns the total number of tasks to process a migration
func (ms *Status) TotalTasks() int64 {
	return ms.total
}

// String formats the migration status message
func (ms *Status) String() string {
	return ms.status
}

// Done reports when the migration is done or it is still in progress
func (ms *Status) Done() bool {
	return ms.finished
}

// update the migration status message (private)
func (ms *Status) update(s string) {
	logInfo(s)
	ms.status = s
}

// update the migration status message with an error message (private)
func (ms *Status) updateErr(err, s string) {
	logFatal(err, s)
	ms.status = "Error: " + s
	ms.finished = true
}

// finish updates the status and marks the migration as finished
func (ms *Status) finish(s string) {
	logInfo(s)
	ms.status = s
	ms.finished = true
}

func (ms *Status) hasBerlinElasticsearchData() (bool, error) {
	return ms.client.DoesIndexExists(ms.ctx, berlinNodeStateIndexName)
}

func (ms *Status) hasA1ElasticsearchData() (bool, error) {
	return ms.client.DoesIndexExists(ms.ctx, a1NodeStateIndexName)
}

func (ms *Status) hasNodeState4Data() (bool, error) {
	return ms.client.DoesIndexExists(ms.ctx, a2NodeState4Index)
}

func (ms *Status) hasNodeState5Data() (bool, error) {
	return ms.client.DoesIndexExists(ms.ctx, a2NodeState5Index)
}

func (ms *Status) migrateBerlinToCurrent() error {
	err := ms.migrateNodeStateToCurrent(berlinNodeStateIndexName)
	if err != nil {
		return err
	}

	// Rewrite the status to mention that this was a Berlin migration
	ms.finish("Berlin migration finished successfully")
	return nil
}

// migrateNodeStateToCurrent assumes that there is a previous index and that the
// index has the alias 'nodeStateAliasName' pointing to itself. This function
// creates a new index (the current one that the mappings are pointing to), reindex
// the previous index to the new index, removes the alias from previous index,
// creates a new alias pointing to the new index and deletes the previous index.
//
// NOTE: If any of these steps fails, we won't be in a healthy state, so we throw
// an error to the end user to verify what happended with the migration.
func (ms *Status) migrateNodeStateToCurrent(previousIndex string) error {
	ms.total = 5

	ms.update("Initializing new node state index")
	ms.client.InitializeStore(ms.ctx)
	ms.taskCompleted()

	ms.update(fmt.Sprintf("Reindexing %s index to current", previousIndex))
	err := ms.client.ReindexNodeStateToLatest(ms.ctx, previousIndex)
	if err != nil {
		ms.updateErr(err.Error(), "Unable to reindex node-state to latest")
		return err
	}
	ms.taskCompleted()

	ms.update(fmt.Sprintf("Removing alias %s from previous index", nodeStateAliasName))
	err = ms.client.RemoveAlias(ms.ctx, nodeStateAliasName, previousIndex)
	if err != nil {
		ms.updateErr(err.Error(), "Unable to remove alias from previous index")
		return err
	}
	ms.taskCompleted()

	ms.update(fmt.Sprintf("Creating alias %s from current index", nodeStateAliasName))
	err = ms.client.CreateAlias(ms.ctx, nodeStateAliasName, mappings.NodeState.Index)
	if err != nil {
		ms.updateErr(err.Error(), "Unable to create alias from current index")
		return err
	}
	ms.taskCompleted()

	ms.update(fmt.Sprintf("Deleting %s index", previousIndex))
	err = ms.client.DeleteIndex(ms.ctx, previousIndex)
	if err != nil {
		ms.updateErr(err.Error(), "Unable to delete previous index")
		return err
	}
	ms.taskCompleted()

	ms.finish("Migration to current finished successfully")
	return nil
}

// All logging will have the tag type=migration for an easy way to search in the logs
func logFatal(err, s string) {
	log.WithFields(log.Fields{
		"error": err,
		"type":  "migration",
	}).Fatal(s)
}

func logWarning(err, s string) {
	log.WithFields(log.Fields{
		"warning": err,
		"type":    "migration",
	}).Warn(s)
}

func logInfo(s string) {
	log.WithFields(log.Fields{
		"type": "migration",
	}).Info(s)
}
