package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"golang.org/x/net/context"
)

const (
	a2V1IndexPrefix    = "comp-1-"
	a2V1SumIndexPrefix = a2V1IndexPrefix + "s-"
)

type A2V1ElasticSearchIndices struct {
	backend *ES2Backend
}

func (migratable A2V1ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V1SumIndexPrefix
}

func (migratable A2V1ElasticSearchIndices) migrateProfiles() error {
	myName := "A2V1ElasticSearchIndices::migrateProfiles"
	defer util.TimeTrack(time.Now(), myName)

	src := a2V1IndexPrefix + "profiles"
	_, _, err := migratable.backend.reindex(src, CompProfilesIndex, noScript, "doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil
}

// Migrates the summary and report indices for a specific date
func (migratable A2V1ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	ctx := context.Background()
	esClient, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.New("migrateTimeSeries cannot connect to ElasticSearch")
	}

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	srcSumIndex := fmt.Sprintf("%ss-%s", a2V1IndexPrefix, dateToMigrateAsString)
	srcSumType := "doc"
	srcRepIndex := fmt.Sprintf("%sr-%s", a2V1IndexPrefix, dateToMigrateAsString)
	srcRepType := "doc"

	err = migrateTimeSeriesDate(ctx, esClient, dateToMigrateAsString, srcSumIndex, srcSumType, srcRepIndex, srcRepType)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("migrateTimeSeries error"))
	}

	return nil
}

func (migratable A2V1ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "A2V1ElasticSearchIndices::postTimeSeriesMigration"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s date: %s", myName, dateToMigrate))

	err = migratable.backend.markTimeseriesDailyLatest(dateToMigrate)
	if err != nil {
		return err
	}

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	//Cannot do wild card deletion of indices in prod.. so must do the following deletions one by one.
	indexToDelete := fmt.Sprintf("%ss-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%sr-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%ssla-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%srla-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%ssld-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%srld-%s", a2V1IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V1ElasticSearchIndices) postProfilesMigration() error {
	myName := "A2V1ElasticSearchIndices::postProfilesMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	indexToDelete := a2V1IndexPrefix + "profiles-mappings"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = a2V1IndexPrefix + "profiles"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V1ElasticSearchIndices) postFeedsMigration() error {
	myName := "A2V1ElasticSearchIndices::postFeedsMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	indexToDelete := a2V1IndexPrefix + "feeds"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A2V1ElasticSearchIndices) postMigration() error {
	myName := "A2V1ElasticSearchIndices::postMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	indexToDelete := a2V1IndexPrefix + "sla"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	indexToDelete = a2V1IndexPrefix + "rla"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	indexToDelete = a2V1IndexPrefix + "migration-info"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	return nil
}
