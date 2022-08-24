package relaxting

import (
	"fmt"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"time"
)

const (
	a2V7CompRunInfoIndexPrefix = "comp-2-"
	olderIndexCompRunInfo      = a2V7CompRunInfoIndexPrefix + "run-info"
)

type A2V7ElasticSearchIndices struct {
	backend *ES2Backend
}

//migrateCompRunInfo migrates the comp run info table
func (migratable A2V7ElasticSearchIndices) migrateCompRunInfo() error {
	myName := "A2V1ElasticSearchIndices::migrateCompRunInfo"
	defer util.TimeTrack(time.Now(), myName)

	src := olderIndexCompRunInfo

	logrus.Infof("Source of the reindexing is: %s", src)
	logrus.Infof("Destination of the reindexing is: %s", CompProfilesIndex)

	_, err := migratable.backend.reindex(src, CompRunInfoIndex, noScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	logrus.Info("Reindexing successful, Please check.")

	return nil

}

func (a A2V7ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V7CompRunInfoIndexPrefix
}

func (a A2V7ElasticSearchIndices) migrateProfiles() error {
	return nil
}

func (a A2V7ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	return nil
}

func (a A2V7ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	return nil
}

func (a A2V7ElasticSearchIndices) postProfilesMigration() error {
	return nil
}

func (a A2V7ElasticSearchIndices) postFeedsMigration() error {
	return nil
}

//removes the older index thats not needed
func (migratable A2V7ElasticSearchIndices) postMigration() error {
	myName := "A2V7ElasticSearchIndices::postMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	indexToDelete := olderIndexCompRunInfo
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	return nil
}

func (a A2V7ElasticSearchIndices) removeOldIndices(dateToMigrate time.Time) error {
	return nil
}
