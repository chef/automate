package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	a2V3IndexPrefix    = "comp-3-"
	a2V3SumIndexPrefix = a2V3IndexPrefix + "s-"
)

type A2V3ElasticSearchIndices struct {
	backend *ES2Backend
}

func (migratable A2V3ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V3SumIndexPrefix
}

func (migratable A2V3ElasticSearchIndices) migrateProfiles() error { return nil }

func (migratable A2V3ElasticSearchIndices) migrateFeeds() error { return nil }

func (migratable A2V3ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	myName := "migrateTimeSeries"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s for date: %s", myName, dateToMigrate))

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	src := fmt.Sprintf("%ss-%s", a2V3IndexPrefix, dateToMigrateAsString)
	dest := fmt.Sprintf("%s%s", CompDailySumIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with waiversFullScript", src)
	_, err := migratable.backend.reindex(src, dest, waiversFullScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	src = fmt.Sprintf("%sr-%s", a2V3IndexPrefix, dateToMigrateAsString)
	dest = fmt.Sprintf("%s%s", CompDailyRepIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with waiversFullScript", src)
	_, err = migratable.backend.reindex(src, dest, waiversFullScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil
}

func (migratable A2V3ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "A2V3ElasticSearchIndices::postTimeSeriesMigration"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s date: %s", myName, dateToMigrate))

	return migratable.removeOldIndices(dateToMigrate)
}

func (migratable A2V3ElasticSearchIndices) removeOldIndices(dateToMigrate time.Time) error {
	myName := "A2V3ElasticSearchIndices::removeOldIndices"

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	//Cannot do wild card deletion of indices in prod.. so must do the following deletions one by one.
	indexToDelete := fmt.Sprintf("%ss-%s", a2V3IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%sr-%s", a2V3IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V3ElasticSearchIndices) postProfilesMigration() error { return nil }

func (migratable A2V3ElasticSearchIndices) postFeedsMigration() error { return nil }

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A2V3ElasticSearchIndices) postMigration() error { return nil }
