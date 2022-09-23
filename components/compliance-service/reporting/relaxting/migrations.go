package relaxting

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"

	elastic "github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	statusserver "github.com/chef/automate/components/compliance-service/api/status/server"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
)

type esMigratable interface {
	getSourceSummaryIndexPrefix() string
	migrateProfiles() error
	migrateTimeSeries(dateToMigrate time.Time) error
	postTimeSeriesMigration(dateToMigrate time.Time) error
	postProfilesMigration() error
	postFeedsMigration() error
	postMigration() error
	removeOldIndices(dateToMigrate time.Time) error
	migrateCompRunInfo() error
}

const noScript = "NO_SCRIPT"

func StoreExists(client *elastic.Client, indexName string) (bool, error) {
	exists, err := client.IndexExists().Index([]string{indexName}).Do(context.Background())

	if err != nil {
		return false, errors.Wrapf(err, "Error checking if index %s exists", indexName)
	}

	return exists, nil
}

func cleanupEmptyIndicesForPrefix(client *elastic.Client, prefix string, migratable esMigratable) ([]string, error) {
	myName := "cleanupEmptyIndicesForPrefix"
	catIndicesResponse, err := client.CatIndices().Index(prefix).Do(context.Background())
	if err != nil {
		return nil, err
	}

	indices := make([]string, len(catIndicesResponse))

	for _, catIndicesResponseRow := range catIndicesResponse {
		indexName := catIndicesResponseRow.Index
		logrus.Debugf("\nIndex: %s ", indexName)

		dateAsStr := strings.SplitAfterN(indexName, "-", 4)
		if len(dateAsStr) > 0 {
			dateToRemove, err := time.Parse("2006.01.02", dateAsStr[len(dateAsStr)-1])
			if err != nil {
				return indices, err
			}
			err = migratable.removeOldIndices(dateToRemove)
			if err != nil {
				return indices, err
			}
		} else {
			logrus.Warnf("%s - trying to cleanup index: %s, which doesn't have a timeseries date as a suffix", myName, indexName)
		}
	}
	return indices, nil
}

// ReportComplianceStatus returns the overall compliance status of a report based on the passed/failed/skipped control counts
func ReportComplianceStatus(summary *reportingTypes.NodeControlSummary) (status string) {
	if summary.Failed.Total > 0 {
		status = inspec.ResultStatusFailed
	} else if summary.Total == summary.Skipped.Total {
		status = inspec.ResultStatusSkipped
	} else {
		status = inspec.ResultStatusPassed
	}
	return status
}

func BulkInsertComplianceSummaryDocs(client *elastic.Client, ctx context.Context, index string, docsArray []*ESInSpecSummary) error {
	if len(docsArray) == 0 {
		logrus.Info("BulkInsertComplianceSummaryDocs received no documents to insert, skipping...")
		return nil
	}
	// Bulk add the summary documents to the compliance timeseries index using the specified report id as document id
	bulkRequest := client.Bulk()
	for _, doc := range docsArray {
		bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Id(doc.ReportID).Doc(doc))
	}
	approxBytes := bulkRequest.EstimatedSizeInBytes()
	bulkResponse, err := bulkRequest.Refresh("false").Do(ctx)
	if err != nil {
		return err
	}
	if bulkResponse == nil {
		return err
	}
	logrus.Debugf("Bulk insert %d summaries, ~size %dB, took %dms", len(docsArray), approxBytes, bulkResponse.Took)
	return nil
}

func BulkInsertComplianceReportDocs(client *elastic.Client, ctx context.Context, index string, docsArray []*ESInSpecReport) error {
	if len(docsArray) == 0 {
		logrus.Info("BulkInsertComplianceSummaryDocs received no documents to insert, skipping...")
		return nil
	}
	// Bulk add the report documents to the compliance timeseries index using the specified report id as document id
	bulkRequest := client.Bulk()
	for _, doc := range docsArray {
		bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Id(doc.ReportID).Doc(doc))
	}
	approxBytes := bulkRequest.EstimatedSizeInBytes()
	bulkResponse, err := bulkRequest.Refresh("false").Do(ctx)
	if err != nil {
		return err
	}
	if bulkResponse == nil {
		return err
	}
	logrus.Debugf("Bulk insert %d reports, ~size %dB, took %dms", len(docsArray), approxBytes, bulkResponse.Took)
	return nil
}

func deleteIndex(client *elastic.Client, indexName string) (bool, bool, error) {
	myName := "deleteIndex"
	indexExists, err := StoreExists(client, indexName)
	if err != nil {
		return false, indexExists, errors.Wrap(err, fmt.Sprintf("%s Error deleting index %s", myName, indexName))
	}

	//if the index does not exist but there was no error trying to determine its existence, leave it up to caller how to proceed.
	if !indexExists {
		return true, indexExists, nil
	}

	//ok so storeExists tells us if the index or alias of the given name exists.. we still need to know if it's an
	//index or an alias though.. let's do that now.
	indices, err := indexNamesByAlias(client, indexName)
	if err != nil {
		return false, indexExists, errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	//if the list of return indice has something in it, then indexName passed in was actually an alias.
	//use what the alias points to instead
	if len(indices) > 0 {
		indexName = indices[0]
	}

	deletedResponse, err := client.DeleteIndex().Index([]string{indexName}).Do(context.Background())
	if err != nil {
		logrus.Errorf("Error deleting index %s, error %s", indexName, err.Error())
	}

	return deletedResponse.Acknowledged, indexExists, err
}

// indexNamesByAlias returns a list of index names that are
// associated by the given aliasName.
func indexNamesByAlias(client *elastic.Client, aliasName string) ([]string, error) {
	res, err := client.Aliases().Index("_all").Do(context.Background())
	if err != nil {
		return nil, err
	}
	return res.IndicesByAlias(aliasName), nil
}

func RunMigrations(backend ES2Backend, statusSrv *statusserver.Server) error {
	myName := "RunMigrations"

	// Migrates A1 indices to the current version
	a1Indices := A1ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a1Indices, statusSrv, statusserver.MigrationLabelESa1)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa1))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa1, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa1, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 1 indices to the current version
	a2V1Indices := A2V1ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V1Indices, statusSrv, statusserver.MigrationLabelESa2v1)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v1))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v1, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v1, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 2 indices to the current version
	a2V2Indices := A2V2ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V2Indices, statusSrv, statusserver.MigrationLabelESa2v2)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v2))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v2, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v2, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 3 indices to the current version
	a2V3Indices := A2V3ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V3Indices, statusSrv, statusserver.MigrationLabelESa2v3)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v3))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v3, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v3, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 4 indices to the current version
	a2V4Indices := A2V4ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V4Indices, statusSrv, statusserver.MigrationLabelESa2v4)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v4))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v4, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v4, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 5 indices to the current version
	a2V5Indices := A2V5ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V5Indices, statusSrv, statusserver.MigrationLabelESa2v5)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v5))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v5, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v5, statusserver.MigrationFailedMsg)
		return errMsg
	}

	// Migrates A2 version 6 indices to the current version
	a2V6Indices := A2V6ElasticSearchIndices{backend: &backend}
	err = backend.migrate(a2V6Indices, statusSrv, statusserver.MigrationLabelESa2v6)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelESa2v5))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v6, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelESa2v6, statusserver.MigrationFailedMsg)
		return errMsg
	}

	/*// Migrates A2 version 2 for comp-run-info indices to the current version
	a2V2CompRunIndices := A2V2CompRunIndices{backend: &backend}
	err = backend.migrate(a2V2CompRunIndices, statusSrv, statusserver.MigrationLabelCompRun)
	if err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, statusserver.MigrationLabelCompRun))
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelCompRun, errMsg.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelCompRun, statusserver.MigrationFailedMsg)
		return errMsg
	}*/

	return nil
}

func (backend ES2Backend) migrate(migratable esMigratable, statusSrv *statusserver.Server, migrationLabel string) error {
	myName := fmt.Sprintf("migrate (%s)", migrationLabel)
	logrus.Debugf(myName)
	defer util.TimeTrack(time.Now(), fmt.Sprintf(" %s reindex indices", myName))

	statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Post feeds migration cleanup...")
	err = migratable.postFeedsMigration()
	if err != nil {
		logrus.Error(err)
		return err
	}

	//migrate the profiles index
	statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Migrating the profiles index...")
	err = migratable.migrateProfiles()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to migrate profiles in ElasticSearch", myName))
	}

	statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Post profiles migration cleanup...")
	err = migratable.postProfilesMigration()
	if err != nil {
		logrus.Error(err)
		return err
	}

	//migrating the comp run info index
	/*	statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Migrating the comp run info index...")
		err = migratable.migrateCompRunInfo()
		if err != nil {
			logrus.Error(err)
			return err
		}*/

	//migrate the compliance time-series indices
	statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Calculating TimeSeries migration range...")
	earliest, latest, err := backend.getScanDateRange(migratable)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to get scans date range", myName))
	}
	if earliest != nil {
		logrus.Debugf("%s Reports-->Earliest: %s, Latest: %s", migrationLabel, earliest.Format("2006-01-02"), latest.Format("2006-01-02"))

		dayBeforeLatest := latest.AddDate(0, 0, -1)
		dayToStartOnAsync := *latest
		if latest.Truncate(24 * time.Hour).Equal(time.Now().UTC().Truncate(24 * time.Hour)) {
			statusserver.AddMigrationUpdate(statusSrv, migrationLabel, "Migrate latest TimeSeries index...")
			err = migratable.migrateTimeSeries(*latest)
			//if we get back an error when attempting to migrate the latest index, return the error. This means that
			// elasticsearch or the source/target index being migrated needs attention.
			if err != nil {
				return errors.Wrap(err, fmt.Sprintf("%s unable to migrate latest TimeSeries index in Elasticsearch", myName))
			}
			err = migratable.postTimeSeriesMigration(*latest)
			//if we get back an error when attempting post-migration, we move forward.. if in fact the error was
			// due to the fact that we couldn't connect to es for some reason, the post migration will be attempted
			// again upon next compliance-service start.  Here, we could instead, actually return the error if desired
			// and force the app owner to deal with it right away.. again, dealing with it may be as simple as letting
			// compliance-service restart and hoping that it clears the second time.
			if err != nil {
				logrus.Errorf("%s unable to post-migrate a TimeSeries index in Elasticsearch, error: %s", myName, err.Error())
			}
			dayToStartOnAsync = dayBeforeLatest
		}

		statusserver.AddMigrationUpdate(statusSrv, migrationLabel,
			fmt.Sprintf("Migrate TimeSeries indices when earliest=%s, latest=%s ...", earliest, latest))
		go func(dateToStart time.Time) {
			for d := dayToStartOnAsync; d.After(*earliest) || d.Equal(*earliest); d = d.AddDate(0, 0, -1) {
				err = migratable.migrateTimeSeries(d)
				//if we get an error here, move on, the error will be logged and the culprit index should be traceable
				// as there are relevant log messages in the migrateTimeSeries func. Note that we are not running the
				// postTimeSeriesMigration if migrateTimeSeries fails as we don't know if it's safe to delete it yet.
				// this scenario could very well require someone to go into es and look at the indices to see what is
				// causing the issue.
				if err != nil {
					logrus.Errorf("%s unable to migrate a TimeSeries index in Elasticsearch, error: %s", myName, err.Error())
					continue
				}

				//if we get here, it means that migrateTimeSeries has succeeded and now we are able to cleanup by
				// deleting the source index or indices.  Again, if it tries and fails to cleanup. The issue is logged
				// and we move on.. the idea is that we want the owner to know that they are having an issue while not
				// taking down compliance in the process.  This phase of migration is for past indices and is therefore
				// non-blocking.  The indices that are being migrated are also not expected to change as they are from
				// days past.. making their migration constant and therefore very predictable (nothing is being added to them over time).
				err = migratable.postTimeSeriesMigration(d)
				if err != nil {
					logrus.Errorf("%s unable to post-migrate a TimeSeries index in Elasticsearch, error: %s", myName, err.Error())
				}
			}
			statusserver.AddMigrationUpdate(statusSrv, migrationLabel, statusserver.MigrationCompletedMsg)
		}(dayToStartOnAsync) // nolint: errcheck

	} else {
		logrus.Debugf("No %s data to migrate", migrationLabel)
		statusserver.AddMigrationUpdate(statusSrv, migrationLabel, statusserver.MigrationCompletedMsg)
	}

	err = migratable.postMigration()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to clean up with PostMigration index in Elasticsearch", myName))
	}

	return nil
}

func (backend ES2Backend) getScanDateRange(migratable esMigratable) (*time.Time, *time.Time, error) {
	myName := "getScanDateRange"

	indexPrefix := migratable.getSourceSummaryIndexPrefix()

	minScanDateAgg := elastic.NewMinAggregation().Field("end_time").Format("yyyy-MM-dd")
	maxScanDateAgg := elastic.NewMaxAggregation().Field("end_time").Format("yyyy-MM-dd")
	searchSource := elastic.NewSearchSource().
		Aggregation("min_date", minScanDateAgg).
		Aggregation("max_date", maxScanDateAgg).
		Size(0)
	esIndex := fmt.Sprintf("%s20*", indexPrefix)
	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}
	LogQueryPartMin(esIndex, source, fmt.Sprintf("%s query searchSource", myName))

	client, err := backend.ES2Client()
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	searchResult, err := client.Search().SearchSource(searchSource).
		Index(esIndex).
		FilterPath("aggregations.min_date,aggregations.max_date").
		Do(context.Background())

	if err != nil {
		logrus.Errorf("could not get scan date range %s", err)
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to complete search", myName))
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, fmt.Sprintf("%s - search results aggs", myName))

	minDate, found := searchResult.Aggregations.Min("min_date")
	if !found {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to read min_date for range", myName))
	}
	maxDate, found := searchResult.Aggregations.Min("max_date")
	if !found {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to read max_date for range", myName))
	}

	if minDate.Value == nil {
		count, err := backend.getDocCountForIndex(esIndex)
		if err != nil {
			logrus.Warnf("%s - could not get doc count for index: %s.", myName, esIndex)
			return nil, nil, nil
		}

		if count == 0 {
			//if we are here, it means that there are indices with this prefix but not any elasticsearch documents within those indices
			//if there were, then mindDate.value would be non-nil
			//in this case, we need to remove all of the indices with this prefix.. they are all empty anyway
			_, err := cleanupEmptyIndicesForPrefix(client, esIndex, migratable)
			if err != nil {
				return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to delete the empty indices that need to be removed", myName))
			}
			logrus.Warnf("%s - there were not any reports in elasticsearch indices that are prefixed with %s, these indices, therefore, have been successfully deleted", myName, esIndex)
			return nil, nil, nil
		} else {
			//in this case, we have documents in at least one of the time series indices but no document has end_time
			// For now, we do not want to do any cleanup. We'll simply place a warning in the log.
			// This is a very unusual scenario and should therefore be inspected with human eyes to determine remedy
			logrus.Warnf("%s - there were documents in index: %s but for some reason they don't have any end_time attributes. You should inspect them to determine if they should be removed.", myName, indexPrefix)
			return nil, nil, nil
		}
	}

	var minDateAsString string
	err = json.Unmarshal(minDate.Aggregations["value_as_string"], &minDateAsString)
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to Unmarshal min_date", myName))
	}

	logrus.Debugf("earliest scan date for range = %s", minDateAsString)

	formatOfDate := "2006-01-02"
	earliestScanDate, err := time.Parse(formatOfDate, minDateAsString)
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to parse min_date", myName))
	}

	var maxDateAsString string
	err = json.Unmarshal(maxDate.Aggregations["value_as_string"], &maxDateAsString)
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to Unmarshal max_date", myName))
	}

	logrus.Debugf(" most recent scan date for range = %s", maxDateAsString)

	mostRecentScanDate, err := time.Parse(formatOfDate, maxDateAsString)
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to parse min_date", myName))
	}

	return &earliestScanDate, &mostRecentScanDate, nil
}

func (backend ES2Backend) getDocCountForIndex(index string) (int64, error) {
	myName := "getDocCountForIndex"

	client, err := backend.ES2Client()
	if err != nil {
		return 0, errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	// Count documents
	count, err := client.Count(index).Do(context.TODO())
	if err != nil {
		return 0, errors.Wrap(err, fmt.Sprintf("%s cannot get doc count for index prefix %s", myName, index))
	}
	return count, nil
}

func (backend ES2Backend) reindex(src, dest, reindexScript, srcDocType string) (bool, error) {
	myName := "reindex"

	client, err := backend.ES2Client()
	if err != nil {
		return false, errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	indexToMigrateExists, err := StoreExists(client, src)
	if err != nil {
		return false, errors.Wrap(err, fmt.Sprintf("%s Error checking if index exists", myName))
	}

	//if the index does not exist but there was no error trying to determine its existence, leave it up to caller how to proceed.
	if !indexToMigrateExists {
		return indexToMigrateExists, nil
	}

	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s src: %s dest: %s", myName, src, dest))
	logrus.Debugf(" * migrating: %s to %s using src.type: %s", src, dest, srcDocType)

	reindexSource := elastic.NewReindexSource().
		Index(strings.Split(src, ",")...).
		Type(srcDocType)

	reindexDestination := elastic.NewReindexDestination().
		Index(dest)

	script := elastic.NewScript(reindexScript)

	reindexCall := elastic.NewReindexService(client).
		Source(reindexSource).
		Destination(reindexDestination).
		Refresh("true")

	if reindexScript != noScript {
		reindexCall = reindexCall.Script(script)
	}

	startTaskResult, err := reindexCall.DoAsync(context.Background())
	if err != nil {
		return indexToMigrateExists, errors.Wrap(err, fmt.Sprintf("%s call to reindex failed", myName))
	}

	for {
		time.Sleep(time.Second * 1)
		completed, err := backend.ReindexStatus(context.Background(), startTaskResult.TaskId)
		if err != nil {
			return indexToMigrateExists, err
		}
		if completed {
			break
		}
		logrus.Debugf(" * migrating: waiting for reindex task %s to complete", startTaskResult.TaskId)
	}

	return indexToMigrateExists, nil
}

func (backend ES2Backend) getLatestReportIds(sumDailyToday string, reportTime time.Time) (*reportingapi.ReportIds, error) {
	myName := "getLatestReportIds"
	formattedFilters := make(map[string][]string)
	// Setting end_time filter to the end of the index day to avoid any end_time filters from excluding our report
	formattedFilters["end_time"] = []string{reportTime.UTC().Format("2006-01-02") + "T23:59:59Z"}
	logrus.Debugf("getLatestReportIds called with filters: %+v", formattedFilters)
	reportIds, err := backend.getNodeReportIdsFromTimeseries(sumDailyToday, formattedFilters, false)
	if err != nil {
		logrus.Errorf("%s no report ids for this day: %s. Will still reindex yesterday's latest though", myName, sumDailyToday)
	}

	return reportIds, nil
}

func (backend *ES2Backend) markTimeseriesDailyLatest(dateToMigrate time.Time) error {
	myName := "markTimeseriesDailyLatest"
	client, err := backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")
	summaryIndexToUpdate := fmt.Sprintf("%s%s", CompDailySumIndexPrefix, dateToMigrateAsString)
	reportIndexToUpdate := fmt.Sprintf("%s%s", CompDailyRepIndexPrefix, dateToMigrateAsString)

	indexToMigrateExists, err := StoreExists(client, summaryIndexToUpdate)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s Error checking if index exists", myName))
	}

	//if the index does not exist but there was no error trying to determine its existence, leave it up to caller how to proceed.
	if !indexToMigrateExists {
		return nil
	}

	// Refresh the indices to query and then update the daily_latest field
	_, err = client.Refresh(summaryIndexToUpdate, reportIndexToUpdate).Do(context.Background())
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("markTimeseriesDailyLatest unable to refresh indices %s, %s", summaryIndexToUpdate, reportIndexToUpdate))
	}

	reportIds, err := backend.getLatestReportIds(summaryIndexToUpdate, dateToMigrate)
	if err != nil {
		return err
	}

	if reportIds == nil || len(reportIds.Ids) == 0 {
		return nil
	}

	indicesToUpdate := fmt.Sprintf("%s*,%s*", summaryIndexToUpdate, reportIndexToUpdate)
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportIds.Ids...)

	script := elastic.NewScript("ctx._source.daily_latest = true")
	_, err = elastic.NewUpdateByQueryService(client).
		Index(indicesToUpdate).
		Query(idsQuery).
		Script(script).
		Do(context.Background())

	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("error updating index %s", indicesToUpdate))
	}
	return nil
}

// Migrates doc by doc the summary and report indices for a specific date
// Designed to be compatible with multiple sources (A1, A2v1, A2v2)
func migrateTimeSeriesDate(ctx context.Context, esClient *elastic.Client, dateToMigrateAsString string, srcSumIndex string, srcSumType string, srcRepIndex string, srcRepType string) error {
	myName := "migrateTimeSeriesDate"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s for date: %s", myName, dateToMigrateAsString))

	dstSumIndex := fmt.Sprintf("%s%s", CompDailySumIndexPrefix, dateToMigrateAsString)
	dstRepIndex := fmt.Sprintf("%s%s", CompDailyRepIndexPrefix, dateToMigrateAsString)

	// Create the summary destination index if needed
	if exists, _ := StoreExists(esClient, srcSumIndex); !exists {
		return nil
	}
	logrus.Infof("migrateTimeSeries creating sum index %s", dstSumIndex)
	_, err = esClient.IndexPutTemplate(dstSumIndex).BodyString(mappings.ComplianceSumDate.Mapping).Do(ctx)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot create sum index template %s", myName, dstSumIndex))
	}
	// Create the summary destination index if needed. The index shouldn't exist unless a
	// previous migration failed or was canceled. Trying to avoid creation error in this case
	if exists, _ := StoreExists(esClient, dstSumIndex); !exists {
		_, err = esClient.CreateIndex(dstSumIndex).Do(ctx)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("%s cannot create sum index %s", myName, dstSumIndex))
		}
	}

	logrus.Infof("migrateTimeSeries creating rep index %s", dstRepIndex)
	_, err = esClient.IndexPutTemplate(dstRepIndex).BodyString(mappings.ComplianceRepDate.Mapping).Do(ctx)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot create rep index template %s", myName, dstRepIndex))
	}
	// Create the report destination index if needed
	if exists, _ := StoreExists(esClient, dstRepIndex); !exists {
		_, err = esClient.CreateIndex(dstRepIndex).Do(ctx)
		if err != nil {
			return errors.Wrapf(err, "%s cannot create rep index %s", myName, dstRepIndex)
		}
	}

	boolQuery := elastic.NewBoolQuery()
	fsc := elastic.NewFetchSourceContext(true)
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(30)
	// Scrolling with a keepAlive of 10 minutes for each scroll.Do request
	scroll := esClient.Scroll().
		KeepAlive("10m").
		Index(srcSumIndex).
		Type(srcSumType).
		SearchSource(searchSource)

	profilesMetaMap = make(map[string]*missingProfileMeta, 0)
	for {
		results, err := scroll.Do(ctx)
		//LogQueryPartMin(results, "getDocIdHits query results")
		if err == io.EOF {
			return nil // all results retrieved
		}
		if err != nil {
			return errors.Wrap(err, "migrateTimeSeries unable to get documents")
		}
		if results.TotalHits() > 0 && len(results.Hits.Hits) > 0 {
			reportIds := make([]string, len(results.Hits.Hits))
			for i, hit := range results.Hits.Hits {
				reportIds[i] = hit.Id
			}

			esInSpecReports, err := getReportsA2v2(esClient, ctx, srcRepIndex, srcRepType, reportIds)
			if err != nil {
				return errors.Wrapf(err, "migrateTimeSeries unable to get reports from index %s", srcRepIndex)
			}

			dstSums := make([]*ESInSpecSummary, 0)
			dstReps := make([]*ESInSpecReport, 0)
			for _, hit := range results.Hits.Hits {
				reportId := hit.Id
				esInSpecSummary := ESInSpecSummaryA2v2{}
				if hit.Source != nil {
					err := json.Unmarshal(hit.Source, &esInSpecSummary)
					if err != nil {
						return errors.Wrapf(err, "migrateTimeSeries unable to unmarshall report with ID=%s", reportId)
					}
					staticProfileMissing := false
					for _, esInSpecReportProfile := range esInSpecSummary.ProfilesSums {
						_, profileId := rightSplit(esInSpecReportProfile.Profile, "|")
						if profilesMetaMap[profileId] == nil {
							esProfile, err := getProfileA2v2(esClient, ctx, profileId)
							if err != nil {
								logrus.Errorf("migrateTimeSeries unable to get profile %s, not migrating report %s", profileId, reportId)
								staticProfileMissing = true
								// If one of the profiles used by the report can't be found in the static profiles index
								// we break out of this loop as the report won't be migrated anyway
								break
							}
							addProfileToMap(esProfile, profileId)
						}
					}

					if staticProfileMissing {
						continue
					}

					if esInSpecSummary.ReportID == "" {
						// A2v1 data didn't have the report_id field in elasticsearch docs
						esInSpecSummary.ReportID = reportId
					}

					if esInSpecReports[reportId] != nil {
						dstSum, err := convertA2v2SummaryDocToLatest(&esInSpecSummary)
						if err != nil {
							logrus.Errorf(err.Error())
							// Only log errors and avoid migrating incomplete reports
							continue
						}

						dstSum.Statistics.Duration = esInSpecReports[hit.Id].Statistics.Duration
						dstSum.InSpecVersion = esInSpecReports[hit.Id].InSpecVersion

						dstRep, err := convertA2v2ReportDocToLatest(esInSpecReports[hit.Id], dstSum)
						if err != nil {
							logrus.Errorf(err.Error())
							// Only log errors and avoid migrating incomplete reports
							continue
						}

						dstSums = append(dstSums, dstSum)
						dstReps = append(dstReps, dstRep)
					} else {
						logrus.Errorf("Not migrating report %s as it's missing from index %s", reportId, srcRepIndex)
					}
				}
			}

			if len(dstSums) > 0 {
				err = BulkInsertComplianceSummaryDocs(esClient, ctx, dstSumIndex, dstSums)
				if err != nil {
					return errors.Wrapf(err, "migrateTimeSeries unable to bulk insert %d summary docs in index %s", len(dstSums), dstSumIndex)
				}
			}

			if len(dstReps) > 0 {
				err = BulkInsertComplianceReportDocs(esClient, ctx, dstRepIndex, dstReps)
				if err != nil {
					return errors.Wrapf(err, "migrateTimeSeries unable to bulk insert %d report docs in index %s", len(dstReps), dstRepIndex)
				}
			}
		}
	}
}
