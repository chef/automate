package relaxting

import (
	"fmt"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/pkg/errors"
	"time"
)

const (
	a2V2CompRunInfoIndexPrefix = "comp-2-"
	olderIndexCompRunInfo      = a2V2CompRunInfoIndexPrefix + "run-info"
)

type A2V2CompRunIndices struct {
	backend *ES2Backend
}

//migrateCompRunInfo migrates the comp run info table
func (migratable A2V2CompRunIndices) migrateCompRunInfo() error {
	myName := "A2V2CompRunIndices::migrateCompRunInfo"
	defer util.TimeTrack(time.Now(), myName)

	src := olderIndexCompRunInfo

	_, err := migratable.backend.reindex(src, CompRunInfoIndex, noScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil

}

func (a A2V2CompRunIndices) getSourceSummaryIndexPrefix() string {
	return a2V2CompRunInfoIndexPrefix
}

func (a A2V2CompRunIndices) migrateProfiles() error {
	return nil
}

func (a A2V2CompRunIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	return nil
}

func (a A2V2CompRunIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	return nil
}

func (a A2V2CompRunIndices) postProfilesMigration() error {
	return nil
}

func (a A2V2CompRunIndices) postFeedsMigration() error {
	return nil
}

//removes the older index thats not needed
func (migratable A2V2CompRunIndices) postMigration() error {
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

func (a A2V2CompRunIndices) removeOldIndices(dateToMigrate time.Time) error {
	return nil
}
