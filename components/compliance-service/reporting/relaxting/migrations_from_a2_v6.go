package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	a2V6IndexPrefix    = "comp-6-"
	a2V6SumIndexPrefix = a2V6IndexPrefix + "s-"
)

// Migration adds
// * Changes the mapping of timeseries to support the new waiver fields

type A2V6ElasticSearchIndices struct {
	backend *ES2Backend
}

func (migratable A2V6ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V6SumIndexPrefix
}

func (migratable A2V6ElasticSearchIndices) migrateProfiles() error { return nil }

func (migratable A2V6ElasticSearchIndices) migrateFeeds() error { return nil }

// reindexes from comp-6-r -> comp-7-r and comp-6-s -> comp-7-s
func (migratable A2V6ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	myName := "migrateTimeSeries"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s for date: %s", myName, dateToMigrate))

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	src := fmt.Sprintf("%ss-%s", a2V6IndexPrefix, dateToMigrateAsString)
	dest := fmt.Sprintf("%s%s", CompDailySumIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with noScript", src)
	_, err := migratable.backend.reindex(src, dest, noScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	src = fmt.Sprintf("%sr-%s", a2V6IndexPrefix, dateToMigrateAsString)
	dest = fmt.Sprintf("%s%s", CompDailyRepIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with noScript", src)
	_, err = migratable.backend.reindex(src, dest, noScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil
}

func (migratable A2V6ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "postMigration"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s date: %s", myName, dateToMigrate))

	return migratable.removeOldIndices(dateToMigrate)
}

func (migratable A2V6ElasticSearchIndices) removeOldIndices(dateToMigrate time.Time) error {
	myName := "A2V5ElasticSearchIndices::removeOldIndices"

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	//Cannot do wild card deletion of indices in prod.. so must do the following deletions one by one.
	indexToDelete := fmt.Sprintf("%ss-%s", a2V6IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%sr-%s", a2V6IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V6ElasticSearchIndices) postProfilesMigration() error { return nil }

func (migratable A2V6ElasticSearchIndices) postFeedsMigration() error { return nil }

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A2V6ElasticSearchIndices) postMigration() error { return nil }
