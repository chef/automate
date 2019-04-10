package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"golang.org/x/net/context"
)

const (
	a1IndexPrefix             = "compliance-"
	a1SumIndexPrefix          = a1IndexPrefix
	a1ProfilesMigrationScript = "ctx._source.remove(\"type\")"
)

type A1ElasticSearchIndices struct {
	backend *ES2Backend
}

func (migratable A1ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a1SumIndexPrefix
}

func (migratable A1ElasticSearchIndices) migrateProfiles() error {
	myName := "A1ElasticSearchIndices::migrateProfiles"
	defer util.TimeTrack(time.Now(), myName)

	src := a1IndexPrefix + "profiles"
	reindexResp, _, err := migratable.backend.reindex(src, CompProfilesIndex, a1ProfilesMigrationScript, "inspec_profile")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	if reindexResp == nil {
		//there must not have been an alias to named compliance-profiles
		//so let's see if there is an index named compliance-profiles-2 and if so reindex that
		src := a1IndexPrefix + "profiles-2"
		_, _, err = migratable.backend.reindex(src, CompProfilesIndex, a1ProfilesMigrationScript, "inspec_profile")
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
		}
	}
	return nil
}

// Migrates the summary and report indices for a specific date
func (migratable A1ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	ctx := context.Background()
	esClient, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.New("migrateTimeSeries cannot connect to ElasticSearch")
	}

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	// In A1, both sum and rep docs were in the same index
	srcIndex := fmt.Sprintf("%s%s", a1IndexPrefix, dateToMigrateAsString)
	srcSumType := "inspec_summary"
	srcRepType := "inspec_report"

	err = migrateTimeSeriesDate(ctx, esClient, dateToMigrateAsString, srcIndex, srcSumType, srcIndex, srcRepType)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("migrateTimeSeries error"))
	}

	return nil
}

func (migratable A1ElasticSearchIndices) migrateFeeds() error {
	return nil
}

func (migratable A1ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "A1ElasticSearchIndices::postTimeSeriesMigration"
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
	indexToDelete := fmt.Sprintf("compliance-%s", dateToMigrateAsString)

	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting indices", myName))
	}

	return nil
}

func (migratable A1ElasticSearchIndices) postProfilesMigration() error {
	myName := "A1ElasticSearchIndices::postProfilesMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	indexToDelete := "compliance-profiles"
	_, existed, err := deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index pointed to by alias: %s", myName, indexToDelete))
	}

	if !existed {
		//there must not have been an alias to named compliance-profiles
		//so let's see if there is an index named compliance-profiles-2 and if so reindex that
		indexToDelete = a1IndexPrefix + "profiles-2"
		_, _, err = deleteIndex(client, indexToDelete)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
		}
	}

	return nil
}

func (migratable A1ElasticSearchIndices) postFeedsMigration() error {
	return nil
}

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A1ElasticSearchIndices) postMigration() error {
	myName := "A1ElasticSearchIndices::postMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	indexToDelete := a1IndexPrefix + "latest-1"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}
	return nil
}
