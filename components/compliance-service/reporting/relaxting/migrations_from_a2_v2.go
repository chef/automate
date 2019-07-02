package relaxting

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

const (
	a2V2IndexPrefix    = "comp-2-"
	a2V2SumIndexPrefix = a2V2IndexPrefix + "s-"
)

type A2V2ElasticSearchIndices struct {
	backend *ES2Backend
}

type missingControlMeta struct {
	Title  string
	Impact float32
}
type missingProfileMeta struct {
	Title    string
	Version  string
	Controls map[string]missingControlMeta
}

// Cache to store the profile metadata needed to enrich the summary and report documents
// without searching to the profiles index for every profile in every document we migrate
var profilesMetaMap map[string]*missingProfileMeta

func (migratable A2V2ElasticSearchIndices) getSourceSummaryIndexPrefix() string {
	return a2V2SumIndexPrefix
}

func (migratable A2V2ElasticSearchIndices) migrateProfiles() error {
	myName := "A2V2ElasticSearchIndices::migrateProfiles"
	defer util.TimeTrack(time.Now(), myName)

	src := a2V2IndexPrefix + "profiles"
	_, _, err := migratable.backend.reindex(src, CompProfilesIndex, noScript, "_doc")
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s unable to reindex %s", src, myName))
	}

	return nil
}

func (migratable A2V2ElasticSearchIndices) migrateFeeds() error { return nil }

// Migrates the summary and report indices for a specific date
func (migratable A2V2ElasticSearchIndices) migrateTimeSeries(dateToMigrate time.Time) error {
	ctx := context.Background()
	esClient, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.New("migrateTimeSeries cannot connect to ElasticSearch")
	}

	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	srcSumIndex := fmt.Sprintf("%ss-%s", a2V2IndexPrefix, dateToMigrateAsString)
	srcSumType := "_doc"
	srcRepIndex := fmt.Sprintf("%sr-%s", a2V2IndexPrefix, dateToMigrateAsString)
	srcRepType := "_doc"

	err = migrateTimeSeriesDate(ctx, esClient, dateToMigrateAsString, srcSumIndex, srcSumType, srcRepIndex, srcRepType)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("migrateTimeSeries error"))
	}

	return nil
}

// Used to add profiles metadata from the ES profile to profilesMetaMap
func addProfileToMap(esProfile *ESInSpecReportProfileA2v2, profileId string) {
	profilesMetaMap[profileId] = &missingProfileMeta{
		Title:    esProfile.Title,
		Version:  esProfile.Version,
		Controls: make(map[string]missingControlMeta, 0),
	}
	for _, control := range esProfile.Controls {
		profilesMetaMap[profileId].Controls[control.ID] = missingControlMeta{
			control.Title,
			control.Impact,
		}
	}
}

func getProfileA2v2(client *elastic.Client, ctx context.Context, profileId string) (*ESInSpecReportProfileA2v2, error) {
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(profileId)

	searchSource := elastic.NewSearchSource().
		Query(idsQuery).
		Size(1)

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index("compliance-profiles*", "comp-1-profiles*", "comp-2-profiles*", "comp-3-profiles*").
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
		).Do(ctx)

	if err != nil {
		return nil, errors.Wrap(err, "getProfileA2v2 unable to complete search")
	}

	var esProfile ESInSpecReportProfileA2v2
	if searchResult.TotalHits() > 0 && len(searchResult.Hits.Hits) > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esProfile = ESInSpecReportProfileA2v2{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esProfile)
				if err != nil {
					return nil, errors.Wrap(err, fmt.Sprintf("getProfileA2v2 unable to unmarshall profile with ID=%s", hit.Id))
				}

			}
		}
	}
	return &esProfile, nil
}

func getReportsA2v2(client *elastic.Client, ctx context.Context, esIndex string, docType string, reportIds []string) (map[string]*ESInSpecReportA2v2, error) {
	esReports := make(map[string]*ESInSpecReportA2v2, len(reportIds))

	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportIds...)

	searchSource := elastic.NewSearchSource().
		Query(idsQuery).
		Size(len(reportIds))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Type(docType).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
		).Do(ctx)

	if err != nil {
		return nil, errors.Wrap(err, "getReportsA2v2 unable to complete search")
	}

	if searchResult.TotalHits() > 0 && len(searchResult.Hits.Hits) > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esReports[hit.Id] = &ESInSpecReportA2v2{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, esReports[hit.Id])
				if err != nil {
					return nil, errors.Wrap(err, fmt.Sprintf("getReportsA2v2 unable to unmarshall report with ID=%s", hit.Id))
				}
			}
		}
	} else {
		logrus.Errorf("getReportsA2v2 call to get reports from esIndex=%s returned no hits", esIndex)
	}
	return esReports, nil
}

// Converts A1, A2v1, A2v2 ElasticSearch compliance summary documents to the Latest format we support
func convertA2v2SummaryDocToLatest(src *ESInSpecSummaryA2v2) *ESInSpecSummary {
	var dstSum ESInSpecSummary
	dstSum.NodeID = src.NodeID
	dstSum.ReportID = src.ReportID
	dstSum.DailyLatest = src.DailyLatest
	dstSum.NodeName = src.NodeName
	dstSum.Environment = src.Environment
	dstSum.EndTime = src.EndTime
	dstSum.Status = src.Status
	dstSum.JobID = src.JobID
	dstSum.Roles = src.Roles
	dstSum.Recipes = src.Recipes
	dstSum.Platform.Name = src.Platform.Name
	dstSum.Platform.Release = src.Platform.Release
	dstSum.Platform.Full = src.Platform.Name + " " + src.Platform.Release
	dstSum.ControlsSums.Total = src.Controls.Total
	dstSum.ControlsSums.Failed = src.Controls.Failed
	dstSum.ControlsSums.Passed = src.Controls.Passed
	dstSum.ControlsSums.Skipped = src.Controls.Skipped
	dstSum.ESTimestamp = src.ESTimestamp
	dstSum.DocVersion = src.DocVersion
	dstSum.Profiles = make([]ESInSpecSummaryProfile, len(src.ProfilesSums))
	for i, srcProfileSum := range src.ProfilesSums {
		dstControlSums := reporting.NodeControlSummary{
			Total:   srcProfileSum.Controls.Total,
			Failed:  srcProfileSum.Controls.Failed,
			Passed:  srcProfileSum.Controls.Passed,
			Skipped: srcProfileSum.Controls.Skipped,
		}
		profileName, profileId := rightSplit(srcProfileSum.Profile, "|")
		profileTitle := ""
		profileVersion := ""
		if profilesMetaMap[profileId] != nil {
			profileTitle = profilesMetaMap[profileId].Title
			profileVersion = profilesMetaMap[profileId].Version
		}
		profileStatus := srcProfileSum.Status
		if profileStatus == "" || profileStatus == "loaded" {
			// Needed because A1 data didn't have profile level status
			// also overwriting the inspec report "loaded" state with actual profile run status
			profileStatus = ReportComplianceStatus(&dstControlSums)
		}
		dstSum.Profiles[i] = ESInSpecSummaryProfile{
			Profile:      srcProfileSum.Profile,
			Status:       profileStatus,
			ControlsSums: dstControlSums,
			Name:         profileName,
			Title:        profileTitle,
			SHA256:       profileId,
			Version:      profileVersion,
		}
	}
	return &dstSum
}

// Converts A1, A2v1, A2v2 ElasticSearch compliance report documents to the latest format we support
func convertA2v2ReportDocToLatest(src *ESInSpecReportA2v2, dstSum *ESInSpecSummary) *ESInSpecReport {
	// controls_sums were not in report documents before, but we are harmonizing that now and
	// bringing those per profile calculations from summary to report documents as well
	profilesSumsMap := make(map[string]reporting.NodeControlSummary, len(src.ProfilesMin))
	profilesStatusMap := make(map[string]string, len(src.ProfilesMin))
	for _, dstSumProfileSum := range dstSum.Profiles {
		profilesSumsMap[dstSumProfileSum.SHA256] = dstSumProfileSum.ControlsSums
		profilesStatusMap[dstSumProfileSum.SHA256] = dstSumProfileSum.Status
	}

	if src.ReportID == "" {
		// A2v1 data didn't have the report_id field in elasticsearch docs
		src.ReportID = dstSum.ReportID
	}

	var dstRep ESInSpecReport
	dstRep.NodeID = src.NodeID
	dstRep.ReportID = src.ReportID
	dstRep.DailyLatest = src.DailyLatest
	dstRep.NodeName = src.NodeName
	dstRep.Environment = src.Environment
	dstRep.EndTime = src.EndTime
	dstRep.Status = src.Status
	dstRep.JobID = src.JobID
	dstRep.Roles = src.Roles
	dstRep.Recipes = src.Recipes
	dstRep.Platform.Name = src.Platform.Name
	dstRep.Platform.Release = src.Platform.Release
	dstRep.Platform.Full = src.Platform.Name + " " + src.Platform.Release
	dstRep.ControlsSums.Total = src.Controls.Total
	dstRep.ControlsSums.Failed = src.Controls.Failed
	dstRep.ControlsSums.Passed = src.Controls.Passed
	dstRep.ControlsSums.Skipped = src.Controls.Skipped
	dstRep.ESTimestamp = src.ESTimestamp
	dstRep.DocVersion = src.DocVersion
	dstRep.InSpecVersion = src.InSpecVersion
	dstRep.Statistics.Duration = src.Statistics.Duration
	dstRep.Profiles = make([]ESInSpecReportProfile, len(src.ProfilesMin))
	// Convert the profile within a report
	for i, srcProfileMin := range src.ProfilesMin {
		profileTitle := ""
		profileStatus := srcProfileMin.Status
		if profilesMetaMap[srcProfileMin.SHA256] != nil {
			profileTitle = profilesMetaMap[srcProfileMin.SHA256].Title
			if profileStatus == "" || profileStatus == "loaded" {
				// needed because A1 data didn't have profile level status
				// also overwriting the inspec report "loaded" state with actual profile run status
				profileStatus = profilesStatusMap[srcProfileMin.SHA256]
			}
		}
		dstRep.Profiles[i] = ESInSpecReportProfile{
			Profile:      fmt.Sprintf("%s|%s", srcProfileMin.Name, srcProfileMin.SHA256),
			Status:       profileStatus,
			ControlsSums: profilesSumsMap[srcProfileMin.SHA256],
			Name:         srcProfileMin.Name,
			Title:        profileTitle,
			SHA256:       srcProfileMin.SHA256,
			Version:      srcProfileMin.Version,
		}

		// Convert the controls within a profile
		dstRep.Profiles[i].Controls = make([]ESInSpecReportControl, len(srcProfileMin.Controls))
		for j, srcProfileMinControl := range srcProfileMin.Controls {
			dstRep.Profiles[i].Controls[j] = ESInSpecReportControl{
				ID:     srcProfileMinControl.ID,
				Title:  profilesMetaMap[srcProfileMin.SHA256].Controls[srcProfileMinControl.ID].Title,
				Impact: profilesMetaMap[srcProfileMin.SHA256].Controls[srcProfileMinControl.ID].Impact,
				Status: srcProfileMinControl.Status,
			}

			// Convert the results within a control
			dstRep.Profiles[i].Controls[j].Results = make([]*ESInSpecReportControlsResult, len(srcProfileMinControl.Results))
			for k, srcProfileMinControlResult := range srcProfileMinControl.Results {
				dstRep.Profiles[i].Controls[j].Results[k] = &ESInSpecReportControlsResult{
					Status:      srcProfileMinControlResult.Status,
					CodeDesc:    srcProfileMinControlResult.CodeDesc,
					RunTime:     srcProfileMinControlResult.RunTime,
					Message:     srcProfileMinControlResult.Message,
					SkipMessage: srcProfileMinControlResult.SkipMessage,
				}
			}
		}
	}
	return &dstRep
}

func (migratable A2V2ElasticSearchIndices) postTimeSeriesMigration(dateToMigrate time.Time) error {
	myName := "A2V2ElasticSearchIndices::postTimeSeriesMigration"
	defer util.TimeTrack(time.Now(), fmt.Sprintf("%s date: %s", myName, dateToMigrate))

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}
	dateToMigrateAsString := dateToMigrate.Format("2006.01.02")

	//Cannot do wild card deletion of indices in prod.. so must do the following deletions one by one.
	indexToDelete := fmt.Sprintf("%ss-%s", a2V2IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	indexToDelete = fmt.Sprintf("%sr-%s", a2V2IndexPrefix, dateToMigrateAsString)
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index: %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V2ElasticSearchIndices) postProfilesMigration() error {
	myName := "A2V2ElasticSearchIndices::postProfilesMigration"
	defer util.TimeTrack(time.Now(), myName)

	client, err := migratable.backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s cannot connect to ElasticSearch", myName))
	}

	indexToDelete := a2V2IndexPrefix + "profiles"
	_, _, err = deleteIndex(client, indexToDelete)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("%s error deleting index %s", myName, indexToDelete))
	}

	return nil
}

func (migratable A2V2ElasticSearchIndices) postFeedsMigration() error { return nil }

//final cleanup - after all migrations run.  use this to clean up any extra indices that are not to be migrated
func (migratable A2V2ElasticSearchIndices) postMigration() error {
	return nil
}
