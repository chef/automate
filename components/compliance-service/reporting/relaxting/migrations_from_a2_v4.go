package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	a2V4IndexPrefix    = "comp-4-"
	a2V4SumIndexPrefix = a2V4IndexPrefix + "s-"
	waiversFullScript  = `
		def waived_total_zero = ['total': 0];
		if (ctx._source['profiles'] != null) {
			for (int i = 0; i < ctx._source.profiles.length; ++i) {
				if (ctx._source.profiles[i] != null) {
					if (ctx._source.profiles[i]['title'] != null && ctx._source.profiles[i]['title'] != '' && ctx._source.profiles[i]['version'] != null && ctx._source.profiles[i]['version'] != '') {
						ctx._source.profiles[i].full = ctx._source.profiles[i].title + ', v' + ctx._source.profiles[i].version;
					} else if (ctx._source.profiles[i]['title'] != null && ctx._source.profiles[i]['title'] != '') {
						ctx._source.profiles[i].full = ctx._source.profiles[i].title;
					}
					if (ctx._source.profiles[i].controls != null ) {
						for (int j = 0; j < ctx._source.profiles[i].controls.length; ++j) {
							if (ctx._source.profiles[i].controls[j] != null ) {
								ctx._source.profiles[i].controls[j].waived_str = "no"
							}
						}
					}
					if (ctx._source.profiles[i].controls_sums != null) {
							ctx._source.profiles[i].controls_sums.waived = waived_total_zero;
					}
				}
			}
		}
		if (ctx._source.controls_sums != null) {
				ctx._source.controls_sums.waived = waived_total_zero;
		}
	`
)

// Migration adds
// * updates the platform.full field to have wildcard matching and suggestions
// * creates a new profiles.full field with wildcard matching and suggestions

type A2V4ElasticSearchIndices struct {
	backend *ES2Backend
}

func (migratable A2V4ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V4SumIndexPrefix
}

func (migratable A2V4ElasticSearchIndices) migrateProfiles() error { return nil }

func (migratable A2V4ElasticSearchIndices) migrateFeeds() error { return nil }

// reindexes from comp-4-r -> comp-5-r and comp-4-s -> comp-5-s
func (migratable A2V4ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	myName := "migrateTimeSeries"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s for date: %s", myName, dateToMigrate))

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	src := fmt.Sprintf("%ss-%s", a2V4IndexPrefix, dateToMigrateAsString)
	dest := fmt.Sprintf("%s%s", CompDailySumIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with waiversFullScript", src)
	_, err := migratable.backend.reindex(src, dest, waiversFullScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	src = fmt.Sprintf("%sr-%s", a2V4IndexPrefix, dateToMigrateAsString)
	dest = fmt.Sprintf("%s%s", CompDailyRepIndexPrefix, dateToMigrateAsString)
	logrus.Debugf("Reindexing %s with waiversFullScript", src)
	_, err = migratable.backend.reindex(src, dest, waiversFullScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil
}

func (migratable A2V4ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "A2V4ElasticSearchIndices::postTimeSeriesMigration"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s date: %s", myName, dateToMigrate))

	return migratable.removeOldIndices(dateToMigrate)
}

func (migratable A2V4ElasticSearchIndices) removeOldIndices(dateToMigrate time.Time) error {
	myName := "A2V4ElasticSearchIndices::removeOldIndices"

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	//Cannot do wild card deletion of indices in prod.. so must do the following deletions one by one.
	indexToDelete := fmt.Sprintf("%ss-%s", a2V4IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%sr-%s", a2V4IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V4ElasticSearchIndices) postProfilesMigration() error { return nil }

func (migratable A2V4ElasticSearchIndices) postFeedsMigration() error { return nil }

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A2V4ElasticSearchIndices) postMigration() error { return nil }
