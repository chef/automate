package relaxting

import (
	"io"
	"time"

	"fmt"

	"encoding/json"

	"strings"

	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"

	status "github.com/chef/automate/components/compliance-service/api/status"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/lib/workflow"
)

type esMigratable interface {
	getSourceSummaryIndexPrefix() string
	migrateProfiles() error
	migrateTimeSeries(dateToMigrate time.Time) error
	migrateFeeds() error
	postTimeSeriesMigration(dateToMigrate time.Time) error
	postProfilesMigration() error
	postFeedsMigration() error
	postMigration() error
}

const noScript = "NO_SCRIPT"

func StoreExists(client *elastic.Client, indexName string) (bool, error) {
	exists, err := client.IndexExists().Index([]string{indexName}).Do(context.Background())

	if err != nil {
		return false, errors.Wrapf(err, "Error checking if index %s exists", indexName)
	}

	return exists, nil
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
	//logrus.Infof("!!! BulkInsertComplianceSummaryDocs in with len(docsArray) = %d", len(docsArray))
	for _, doc := range docsArray {
		//logrus.Infof("!!! => inserting in index %s, mapping %s, _id %s", index, mappings.DocType, doc.ReportID)
		bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Type(mappings.DocType).Id(doc.ReportID).Doc(doc))
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
		bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Type(mappings.DocType).Id(doc.ReportID).Doc(doc))
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

func (backend ES2Backend) getScanDateRange(indexPrefix string) (*time.Time, *time.Time, error) {
	myName := "getScanDateRange"
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

	var minDateAsString string
	err = json.Unmarshal(*minDate.Aggregations["value_as_string"], &minDateAsString)
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
	err = json.Unmarshal(*maxDate.Aggregations["value_as_string"], &maxDateAsString)
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

func (backend ES2Backend) reindex(src, dest, reindexScript, srcDocType string) (*elastic.BulkIndexByScrollResponse, bool, error) {
	myName := "reindex"

	client, err := backend.ES2Client()
	if err != nil {
		return nil, false, errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	indexToMigrateExists, err := StoreExists(client, src)
	if err != nil {
		return nil, false, errors.Wrap(err, fmt.Sprintf("%s Error checking if index exists", myName))
	}

	//if the index does not exist but there was no error trying to determine its existence, leave it up to caller how to proceed.
	if !indexToMigrateExists {
		return nil, indexToMigrateExists, nil
	}

	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s src: %s dest: %s", myName, src, dest))
	logrus.Debugf(" * migrating: %s to %s using src.type: %s", src, dest, srcDocType)

	reindexSource := elastic.NewReindexSource().
		Index(strings.Split(src, ",")...).
		Type(srcDocType)

	reindexDestination := elastic.NewReindexDestination().
		Index(dest).
		Type(mappings.DocType)

	script := elastic.NewScript(reindexScript)

	reindexCall := elastic.NewReindexService(client).
		Source(reindexSource).
		Destination(reindexDestination).
		Refresh("true")

	if reindexScript != noScript {
		reindexCall = reindexCall.Script(script)
	}

	reindexCallResponse, err := reindexCall.Do(context.Background())

	if err != nil {
		return nil, indexToMigrateExists, errors.Wrap(err, fmt.Sprintf("%s call to reindex failed", myName))
	}
	return reindexCallResponse, indexToMigrateExists, err
}

func (backend ES2Backend) getLatestReportIds(sumDailyToday string) ([]string, error) {
	myName := "getLatestReportIds"
	reportIds, err := backend.getNodeReportIdsFromTimeseries(sumDailyToday, make(map[string][]string, 0), false)
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

	reportIds, err := backend.getLatestReportIds(summaryIndexToUpdate)
	if err != nil {
		return err
	}

	indicesToUpdate := fmt.Sprintf("%s*,%s*", summaryIndexToUpdate, reportIndexToUpdate)
	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(reportIds...)

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
					err := json.Unmarshal(*hit.Source, &esInSpecSummary)
					if err != nil {
						return errors.Wrapf(err, "migrateTimeSeries unable to unmarshall report with ID=%s", reportId)
					}
					for _, esInSpecReportProfile := range esInSpecSummary.ProfilesSums {
						_, profileId := rightSplit(esInSpecReportProfile.Profile, "|")
						if profilesMetaMap[profileId] == nil {
							esProfile, err := getProfileA2v2(esClient, ctx, profileId)
							if err != nil {
								return errors.Wrapf(err, "migrateTimeSeries unable to get profile %s", profileId)
							}
							addProfileToMap(esProfile, profileId)
						}
					}

					if esInSpecSummary.ReportID == "" {
						// A2v1 data didn't have the report_id field in elasticsearch docs
						esInSpecSummary.ReportID = reportId
					}

					if esInSpecReports[reportId] != nil {
						dstSum := convertA2v2SummaryDocToLatest(&esInSpecSummary)
						dstSum.Statistics.Duration = esInSpecReports[hit.Id].Statistics.Duration
						dstSum.InSpecVersion = esInSpecReports[hit.Id].InSpecVersion
						dstSums = append(dstSums, dstSum)
						dstReps = append(dstReps, convertA2v2ReportDocToLatest(esInSpecReports[hit.Id], dstSum))
					} else {
						return errors.Errorf("Report %s missing for index %s", reportId, srcRepIndex)
					}
				}
			}

			err = BulkInsertComplianceSummaryDocs(esClient, ctx, dstSumIndex, dstSums)
			if err != nil {
				return errors.Wrapf(err, "migrateTimeSeries unable to bulk insert %d summary docs in index %s", len(dstSums), dstSumIndex)
			}

			err = BulkInsertComplianceReportDocs(esClient, ctx, dstRepIndex, dstReps)
			if err != nil {
				return errors.Wrapf(err, "migrateTimeSeries unable to bulk insert %d report docs in index %s", len(dstReps), dstRepIndex)
			}
		}
	}
}

// A version of migrate for a workflow version
type LogEntry struct {
	Label     string
	Text      string
	Timestamp time.Time
}

type MigrationLog interface {
	AddMigrationUpdate(label string, text string)
}

func migrateTimeSeries(backend *ES2Backend, migratable esMigratable, logger MigrationLog, migrationLabel string, earliest time.Time, dayToStartOnAsync time.Time) error {
	myName := "migrateTimeSeries"
	for d := dayToStartOnAsync; d.After(earliest) || d.Equal(earliest); d = d.AddDate(0, 0, -1) {
		err = migratable.migrateTimeSeries(d)
		//if we get an error here, move on, the error will be logged and the culprit index should be traceable
		// as there are relevant log messages in the migrateTimeSeries func. Note that we are not running the
		// postTimeSeriesMigration if migrateTimeSeries fails as we don't know if it's safe to delete it yet.
		// this scenario could very well require someone to go into es and look at the indices to see what is
		// causing the issue.
		if err != nil {
			logrus.Errorf("%s unable to migrate a TimeSeries index in Elasticsearch, error: %s", myName, err.Error())
			return err
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
	logger.AddMigrationUpdate(migrationLabel, status.MigrationCompletedMsg)
	return nil
}

type TimeseriesMigrateRange struct {
	Earliest          time.Time
	DayToStartOnAsync time.Time
}

func migrateStartup(backend *ES2Backend, migratable esMigratable, logger MigrationLog, migrationLabel string) (TimeseriesMigrateRange, error) {
	myName := fmt.Sprintf("migrate (%s)", migrationLabel)
	logrus.Debugf(myName)
	defer util.TimeTrack(time.Now(), fmt.Sprintf(" %s reindex indices", myName))

	var result TimeseriesMigrateRange

	//migrate the feeds index
	logger.AddMigrationUpdate(migrationLabel, "Migrating the feeds index...")
	err := migratable.migrateFeeds()
	if err != nil {
		return result, errors.Wrap(err, fmt.Sprintf("%s unable to migrate feeds in ElasticSearch", myName))
	}

	logger.AddMigrationUpdate(migrationLabel, "Post feeds migration cleanup...")
	err = migratable.postFeedsMigration()
	if err != nil {
		logrus.Error(err)
		return result, err
	}

	//migrate the profiles index
	logger.AddMigrationUpdate(migrationLabel, "Migrating the profiles index...")
	err = migratable.migrateProfiles()
	if err != nil {
		return result, errors.Wrap(err, fmt.Sprintf("%s unable to migrate profiles in ElasticSearch", myName))
	}

	logger.AddMigrationUpdate(migrationLabel, "Post profiles migration cleanup...")
	err = migratable.postProfilesMigration()
	if err != nil {
		logrus.Error(err)
		return result, err
	}

	//migrate the compliance time-series indices
	logger.AddMigrationUpdate(migrationLabel, "Calculating TimeSeries migration range...")
	earliest, latest, err := backend.getScanDateRange(migratable.getSourceSummaryIndexPrefix())
	if err != nil {
		return result, errors.Wrap(err, fmt.Sprintf("%s unable to get scans date range", myName))
	}

	if earliest != nil {
		logrus.Debugf("%s Reports-->Earliest: %s, Latest: %s", migrationLabel, earliest.Format("2006-01-02"), latest.Format("2006-01-02"))

		dayBeforeLatest := latest.AddDate(0, 0, -1)
		dayToStartOnAsync := *latest
		if latest.Truncate(24 * time.Hour).Equal(time.Now().UTC().Truncate(24 * time.Hour)) {
			logger.AddMigrationUpdate(migrationLabel, "Migrate latest TimeSeries index...")
			err = migratable.migrateTimeSeries(*latest)
			//if we get back an error when attempting to migrate the latest index, return the error. This means that
			// elasticsearch or the source/target index being migrated needs attention.
			if err != nil {
				return result, errors.Wrap(err, fmt.Sprintf("%s unable to migrate latest TimeSeries index in Elasticsearch", myName))
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
		result.DayToStartOnAsync = dayToStartOnAsync
		result.Earliest = *earliest
	} else {
		logrus.Debugf("No %s data to migrate", migrationLabel)
		logger.AddMigrationUpdate(migrationLabel, status.MigrationCompletedMsg)
	}

	err = migratable.postMigration()
	if err != nil {
		return result, errors.Wrap(err, fmt.Sprintf("%s unable to clean up with PostMigration index in Elasticsearch", myName))
	}

	return result, nil
}

type MigrationWorkflowExecutor struct {
}

type MigrationWorkflowState struct {
	ErrMsg               string
	Logs                 []LogEntry
	StartupMigrationDone bool
}

func mustEnqueueTask(w workflow.WorkflowInstance, taskName string, params interface{}) {
	if err := w.EnqueueTask(taskName, params); err != nil {
		panic(err)
	}
}

func (state *MigrationWorkflowState) AppendLogs(logs []LogEntry) {
	state.Logs = append(state.Logs, logs...)
}

func (m *MigrationWorkflowExecutor) OnStart(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
	mustEnqueueTask(w, StartupMigrationTaskName, nil)
	return w.Continue(MigrationWorkflowState{
		StartupMigrationDone: false,
	})
}

func (m *MigrationWorkflowExecutor) OnTaskComplete(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision {
	state := MigrationWorkflowState{}
	if err := w.GetPayload(&state); err != nil {
		logrus.WithError(err).Error("Failed to get MigrationWorkflow payload")
		return w.Complete(workflow.WithResult(
			MigrationWorkflowState{
				ErrMsg: err.Error(),
			},
		))
	}

	switch ev.TaskName {
	case StartupMigrationTaskName:
		res := StartupMigrationTaskResult{}
		if ev.Result.Err() != nil {
			logrus.WithError(err).Error("Failed startup migration task")
			state.ErrMsg = ev.Result.Err().Error()
			return w.Complete(workflow.WithResult(state))
		}

		if resErr := ev.Result.Get(&res); resErr != nil {
			logrus.WithError(resErr).Error("Failed to get startup migration result")
			state.ErrMsg = resErr.Error()
			return w.Complete(workflow.WithResult(state))
		}

		state.AppendLogs(res.Logs)
		if res.ErrMsg != "" {
			logrus.WithField("err", res.ErrMsg).Error("startup migration failed")
			state.ErrMsg = res.ErrMsg
			return w.Complete(workflow.WithResult(state))
		}

		for k, v := range res.TSRanges {
			mustEnqueueTask(w, TimeseriesMigrationTaskName, TimeseriesMigrationTaskParams{
				MigrationLabel: k,
				TSRange:        v,
			})
		}
		state.StartupMigrationDone = true
		return w.Continue(state)
	case TimeseriesMigrationTaskName:
		params := TimeseriesMigrationTaskParams{}

		if err := ev.Result.GetParameters(&params); err != nil {
			logrus.WithError(err).Error("failed to get TimeseriesMigrationTaskName parameters")
			state.ErrMsg = err.Error()
			return w.Complete(workflow.WithResult(state))
		}

		res := TimeseriesMigrationTaskResult{}
		if err := ev.Result.Get(&res); err != nil {
			logrus.WithError(err).Error("failed to get TimeseriesMigrationTaskName result")
			state.ErrMsg = err.Error()
			return w.Complete(workflow.WithResult(state))
		}

		if w.TotalEnqueuedTasks() == w.TotalCompletedTasks() {
			return w.Complete(workflow.WithResult(state))
		}
		return w.Continue(state)
	}
	return w.Complete()
}

func (m *MigrationWorkflowExecutor) OnCancel(w workflow.WorkflowInstance, ev workflow.CancelEvent) workflow.Decision {
	return w.Complete()
}

type StartupMigrationTask struct {
	backend *ES2Backend
}

type StartupMigrationTaskResult struct {
	ErrMsg   string
	Logs     []LogEntry
	TSRanges map[string]TimeseriesMigrateRange
}

func (res *StartupMigrationTaskResult) AddMigrationUpdate(label string, text string) {
	res.Logs = append(res.Logs, LogEntry{
		Label:     label,
		Text:      text,
		Timestamp: time.Now(),
	})
}

func (st *StartupMigrationTask) Run(ctx context.Context, task workflow.Task) (interface{}, error) {
	myName := "StartupMigrationTask"
	a1Indices := A1ElasticSearchIndices{backend: st.backend}
	a2V1Indices := A2V1ElasticSearchIndices{backend: st.backend}
	a2V2Indices := A2V2ElasticSearchIndices{backend: st.backend}
	result := &StartupMigrationTaskResult{
		TSRanges: make(map[string]TimeseriesMigrateRange),
	}

	if tsRange, err := migrateStartup(st.backend, a1Indices, result, status.MigrationLabelESa1); err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, status.MigrationLabelESa1))
		result.ErrMsg = errMsg.Error()
		result.AddMigrationUpdate(status.MigrationLabelESa1, errMsg.Error())
		result.AddMigrationUpdate(status.MigrationLabelESa1, status.MigrationFailedMsg)
		return result, nil
	} else {
		result.TSRanges[status.MigrationLabelESa1] = tsRange
	}

	if tsRange, err := migrateStartup(st.backend, a2V1Indices, result, status.MigrationLabelESa2v1); err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, status.MigrationLabelESa2v1))
		result.ErrMsg = errMsg.Error()
		result.AddMigrationUpdate(status.MigrationLabelESa2v1, errMsg.Error())
		result.AddMigrationUpdate(status.MigrationLabelESa2v1, status.MigrationFailedMsg)
		return result, nil
	} else {
		result.TSRanges[status.MigrationLabelESa2v1] = tsRange
	}

	if tsRange, err := migrateStartup(st.backend, a2V2Indices, result, status.MigrationLabelESa2v2); err != nil {
		errMsg := errors.Wrap(err, fmt.Sprintf("%s, migration failed for %s", myName, status.MigrationLabelESa2v2))
		result.ErrMsg = errMsg.Error()
		result.AddMigrationUpdate(status.MigrationLabelESa2v2, errMsg.Error())
		result.AddMigrationUpdate(status.MigrationLabelESa2v2, status.MigrationFailedMsg)
		return result, nil
	} else {
		result.TSRanges[status.MigrationLabelESa2v2] = tsRange
	}

	return result, nil
}

type TimeseriesMigrationTask struct {
	backend *ES2Backend
}
type TimeseriesMigrationTaskParams struct {
	MigrationLabel string
	TSRange        TimeseriesMigrateRange
}

type TimeseriesMigrationTaskResult struct {
	Logs []LogEntry
}

func (res *TimeseriesMigrationTaskResult) AddMigrationUpdate(label string, text string) {
	res.Logs = append(res.Logs, LogEntry{
		Label:     label,
		Text:      text,
		Timestamp: time.Now(),
	})
}

func (tsm *TimeseriesMigrationTask) Run(ctx context.Context, task workflow.Task) (interface{}, error) {
	params := TimeseriesMigrationTaskParams{}
	result := TimeseriesMigrationTaskResult{}
	if err := task.GetParameters(&params); err != nil {
		time.Sleep(10 * time.Minute)
		return result, err
	}

	tsRange := params.TSRange
	if params.MigrationLabel == "" {
		return result, errors.New("invalid TimeSeriesMigrationParams")
	}

	if params.TSRange.Earliest.IsZero() {
		logrus.WithField("label", params.MigrationLabel).Info("No data to migrate")
		result.AddMigrationUpdate(params.MigrationLabel, status.MigrationCompletedMsg)
		return result, nil
	}

	var migratable esMigratable
	switch params.MigrationLabel {
	case status.MigrationLabelESa1:
		migratable = A1ElasticSearchIndices{backend: tsm.backend}
	case status.MigrationLabelESa2v1:
		migratable = A2V1ElasticSearchIndices{backend: tsm.backend}
	case status.MigrationLabelESa2v2:
		migratable = A2V2ElasticSearchIndices{backend: tsm.backend}
	}

	err := migrateTimeSeries(tsm.backend, migratable, &result, params.MigrationLabel, tsRange.Earliest, tsRange.DayToStartOnAsync)

	return result, err
}

const (
	MigrationWorkflowName       = "RelaxtingMigrationWorkflowV1"
	StartupMigrationTaskName    = "RelaxtingStartupMigrationTaskV1"
	TimeseriesMigrationTaskName = "RelaxtingTimeseriesMigrationTaskV1"
)

func InitializeWorkflowManager(workflowManager *workflow.WorkflowManager, backend *ES2Backend) error {
	err := workflowManager.RegisterWorkflowExecutor(
		MigrationWorkflowName,
		&MigrationWorkflowExecutor{},
	)
	if err != nil {
		return err
	}

	err = workflowManager.RegisterTaskExecutor(
		StartupMigrationTaskName,
		&StartupMigrationTask{
			backend: backend,
		},
		workflow.TaskExecutorOpts{
			Workers: 1,
		},
	)
	if err != nil {
		return err
	}

	err = workflowManager.RegisterTaskExecutor(
		TimeseriesMigrationTaskName,
		&TimeseriesMigrationTask{
			backend: backend,
		},
		workflow.TaskExecutorOpts{
			Workers: 1,
		},
	)
	if err != nil {
		return err
	}

	return nil
}

func RunMigrations(ctx context.Context, workflowManager *workflow.WorkflowManager) error {
	err := workflowManager.EnqueueWorkflow(ctx, MigrationWorkflowName, "singleton", nil)
	if err != nil {
		if err != workflow.ErrWorkflowInstanceExists {
			return err
		}
	}
OUTER:
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(6 * time.Second):
			logrus.Info("Getting workflow instance")
			w, err := workflowManager.GetWorkflowInstanceByName(ctx, "singleton", MigrationWorkflowName)
			if err != nil {
				logrus.WithError(err).Error("failed to get relaxting migration workflow instance")
				if err == workflow.ErrWorkflowInstanceNotFound {
					return err
				}
			}
			state := MigrationWorkflowState{}
			if w.IsRunning() {
				logrus.Info("workflow running")
				err := w.GetPayload(&state)
				if err != nil {
					logrus.WithError(err).Error("failed to get relaxting migration workflow payload for status")
					continue
				}
				if state.StartupMigrationDone {
					break OUTER
				}
			} else {
				if err := w.GetResult(&state); err != nil {
					return err
				}
				logrus.WithField("state", state).Info("Migration workflow is complete")

				if state.StartupMigrationDone {
					break OUTER
				}
				if state.ErrMsg != "" {
					return errors.New(state.ErrMsg)
				}

				return errors.New("Migration workflow completed in unknown state")
			}
		}
	}
	// TODO (jaym): we need to check that the migration that ran was the one we needed
	return nil
}

var ErrNotReady = errors.New("relaxting migration result not ready")

func GetMigrationLogs(ctx context.Context, workflowManager *workflow.WorkflowManager) ([]LogEntry, error) {
	w, err := workflowManager.GetWorkflowInstanceByName(ctx, "singleton", MigrationWorkflowName)
	if err != nil {
		logrus.WithError(err).Error("failed to get relaxting migration workflow instance")
		if err == workflow.ErrWorkflowInstanceNotFound {
			return nil, ErrNotReady
		}
	}
	state := MigrationWorkflowState{}
	if w.IsRunning() {
		err := w.GetPayload(&state)
		if err != nil {
			logrus.WithError(err).Error("failed to get relaxting migration workflow payload for status")
			return nil, err
		}
		return state.Logs, nil
	}
	if err := w.GetResult(&state); err != nil {
		return nil, err
	}
	logrus.WithField("state", state).Info("Migration workflow is complete")
	if state.ErrMsg != "" {
		err := errors.New(state.ErrMsg)
		logrus.WithError(err).Error("relaxting migration workflow failed")
		return nil, err
	}

	return state.Logs, nil

}
