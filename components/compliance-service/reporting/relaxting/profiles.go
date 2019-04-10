package relaxting

import (
	"bytes"
	"encoding/json"
	"fmt"
	"time"

	"github.com/golang/protobuf/jsonpb"
	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/api/stats"
	ingestinspec "github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/stringutils"
)

type ESInspecControl struct {
	ID             string                `json:"id"`
	Code           string                `json:"code"`
	Desc           string                `json:"desc"`
	Impact         float32               `json:"impact"`
	Title          string                `json:"title"`
	SourceLocation inspec.SourceLocation `json:"source_location"`
	Refs           string                `json:"refs"`
	Tags           string                `json:"tags"`
	Results        []reportingapi.Result `json:"results,omitempty"`
}

type ESInspecAttribute struct {
	Name    string `json:"name"`
	Options struct {
		Description string `json:"description,omitempty"`
		Default     string `json:"default"`
	} `json:"options,omitempty"`
}

type ESInspecProfile struct {
	Name           string                  `json:"name"`
	Title          string                  `json:"title"`
	Version        string                  `json:"version"`
	Summary        string                  `json:"summary"`
	Maintainer     string                  `json:"maintainer"`
	License        string                  `json:"license"`
	Copyright      string                  `json:"copyright"`
	CopyrightEmail string                  `json:"copyright_email"`
	Controls       []ESInspecControl       `json:"controls"`
	Supports       []*ingestinspec.Support `json:"supports"`
	Attributes     []ESInspecAttribute     `json:"attributes"`
	Dependencies   []inspec.Dependency     `json:"depends,omitempty"`
	Sha256         string                  `json:"sha256"`
	Groups         []inspec.Group          `json:"groups"`
	// ES specific not required by code, harmonized with logstash
	DocVersion  string `json:"doc_version"`
	ESTimestamp string `json:"@timestamp"`
	Status      string `json:"status,omitempty"`
	SkipMessage string `json:"skip_message,omitempty"`
}

func (report *ESInspecProfile) toJSON() ([]byte, error) {
	return json.Marshal(report)
}

// set default values to mimic logstash
func (esprofile *ESInspecProfile) setDefaultValues() {
	esprofile.DocVersion = "1"
	esprofile.ESTimestamp = time.Now().UTC().Format(time.RFC3339)
}

// Converts from inspec.Profile to ESInspecProfile
func (esprofile *ESInspecProfile) parseInspecProfile(profile inspec.Profile) error {
	esprofile.Name = profile.Name
	esprofile.Title = profile.Title
	esprofile.Version = profile.Version
	esprofile.Summary = profile.Summary
	esprofile.Maintainer = profile.Maintainer
	esprofile.License = profile.License
	esprofile.Copyright = profile.Copyright
	esprofile.CopyrightEmail = profile.CopyrightEmail
	esprofile.Supports = convertMapSupportsToInspecSupports(profile.Supports)
	esprofile.Dependencies = convertRSProfileDependenciesToInspecDependencies(profile.Dependencies)
	esprofile.Sha256 = profile.Sha256
	// no need for Status and SkipMessage here as it's not static metadata of the profile
	var groups []inspec.Group
	for _, group := range groups {
		eGroup := inspec.Group{
			ID:       group.ID,
			Title:    group.Title,
			Controls: group.Controls,
		}
		groups = append(groups, eGroup)
	}
	esprofile.Groups = groups

	esprofile.Attributes = make([]ESInspecAttribute, 0)
	for _, attribute := range profile.Attributes {
		esAttribute := ESInspecAttribute{
			Name: attribute.Name,
		}
		esAttribute.Options.Description = attribute.Options.Description
		var defaultVal string
		data, err := json.Marshal(attribute.Options.Default)
		if err != nil {
			return errors.Wrap(err, "parseInspecProfile unable to marshal options")
		}
		json.Unmarshal(data, defaultVal) // nolint: errcheck
		esAttribute.Options.Default = defaultVal

		esprofile.Attributes = append(esprofile.Attributes, esAttribute)
	}

	esprofile.Controls = make([]ESInspecControl, 0)

	// we need to marshal refs and tags for each control
	for _, control := range profile.Controls {
		esControl := ESInspecControl{
			Code:   control.Code,
			Desc:   control.Desc,
			Impact: control.Impact,
			ID:     control.ID,
			Title:  control.Title,
		}
		var results []reportingapi.Result
		for _, res := range control.Results {
			eRes := reportingapi.Result{
				Status:      res.Status,
				CodeDesc:    res.CodeDesc,
				RunTime:     res.RunTime,
				StartTime:   res.StartTime,
				Message:     res.Message,
				SkipMessage: res.SkipMessage,
			}
			results = append(results, eRes)
		}
		esControl.Results = results

		esControl.SourceLocation = inspec.SourceLocation{
			Ref:  control.SourceLocation.Ref,
			Line: int(control.SourceLocation.Line),
		}
		var refs string
		byteRefs, _ := json.Marshal(control.Refs)
		json.Unmarshal(byteRefs, refs) // nolint: errcheck
		esControl.Refs = refs

		var tags string
		bytetags, _ := json.Marshal(control.Tags)
		json.Unmarshal(bytetags, tags) // nolint: errcheck
		esControl.Tags = tags

		esprofile.Controls = append(esprofile.Controls, esControl)
	}

	// set default values to mimic logstash
	esprofile.setDefaultValues()
	return nil
}

// convertToInspecProfile takes a profile from ElasticSearch and converts it
// to a profile as returned by inspec
func (esprofile *ESInspecProfile) convertToInspecProfile() (reportingapi.Profile, error) {
	var inspecProfile reportingapi.Profile
	inspecProfile.Name = esprofile.Name
	inspecProfile.Title = esprofile.Title
	inspecProfile.Version = esprofile.Version
	inspecProfile.Summary = esprofile.Summary
	inspecProfile.Maintainer = esprofile.Maintainer
	inspecProfile.License = esprofile.License
	inspecProfile.Copyright = esprofile.Copyright
	inspecProfile.CopyrightEmail = esprofile.CopyrightEmail
	inspecProfile.Supports = convertInspecSupportsToRSSupports(esprofile.Supports)
	inspecProfile.Depends = convertInspecDependenciesToRSDependencies(esprofile.Dependencies)
	inspecProfile.Sha256 = esprofile.Sha256
	inspecProfile.Status = esprofile.Status
	inspecProfile.SkipMessage = esprofile.SkipMessage
	var groups []*reportingapi.Group
	for _, group := range esprofile.Groups {
		eGroup := reportingapi.Group{
			Id:       group.ID,
			Controls: group.Controls,
		}
		if group.Title != nil {
			eGroup.Title = *group.Title
		}
		groups = append(groups, &eGroup)
	}
	inspecProfile.Groups = groups

	// we need to unmarshal attributes
	inspecProfile.Attributes = make([]*reportingapi.Attribute, 0)
	for _, esAttribute := range esprofile.Attributes {
		attribute := reportingapi.Attribute{
			Name: esAttribute.Name,
		}
		esOption := attribute.Options
		if esOption != nil {
			options := reportingapi.Option{
				Description: esOption.Description,
			}
			if err := json.Unmarshal([]byte(esOption.Default), options.Default); err != nil {
				return inspecProfile, errors.Wrap(err, "convertToInspecProfile unable to unmarshal options")
			}

			attribute.Options = &options
		}

		inspecProfile.Attributes = append(inspecProfile.Attributes, &attribute)
	}

	// we need to unmarshal refs and tags for each control
	inspecProfile.Controls = make([]*reportingapi.Control, 0)
	for _, esControl := range esprofile.Controls {
		control := reportingapi.Control{
			Code:   esControl.Code,
			Desc:   esControl.Desc,
			Impact: esControl.Impact,
			Id:     esControl.ID,
			Title:  esControl.Title,
		}
		var results []*reportingapi.Result
		for _, res := range esControl.Results {
			eRes := reportingapi.Result{
				Status:      res.Status,
				CodeDesc:    res.CodeDesc,
				RunTime:     res.RunTime,
				StartTime:   res.StartTime,
				Message:     res.Message,
				SkipMessage: res.SkipMessage,
			}
			results = append(results, &eRes)
		}
		control.Results = results

		control.SourceLocation = &reportingapi.SourceLocation{
			Ref:  esControl.SourceLocation.Ref,
			Line: int32(esControl.SourceLocation.Line),
		}
		var refs []*reportingapi.Ref
		byteRefs, _ := json.Marshal(esControl.Refs)
		json.Unmarshal(byteRefs, refs) // nolint: errcheck
		control.Refs = refs

		var tags map[string]string
		byteTags, _ := json.Marshal(esControl.Tags)
		json.Unmarshal(byteTags, tags) // nolint: errcheck
		control.Tags = tags

		inspecProfile.Controls = append(inspecProfile.Controls, &control)
	}
	return inspecProfile, nil
}

// internal helper method to get profile information
func (backend *ES2Backend) GetProfile(hash string) (reportingapi.Profile, error) {
	var profile reportingapi.Profile
	client, err := backend.ES2Client()

	if err != nil {
		return profile, errors.Wrap(err, "GetProfile, cannot connect to ElasticSearch")
	}

	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(hash)

	searchSource := elastic.NewSearchSource().
		Query(idsQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return profile, errors.Wrap(err, "GetProfile unable to get Source")
	}
	LogQueryPartMin(CompProfilesIndex, source, "GetProfile query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(CompProfilesIndex).
		Do(context.Background())

	if err != nil {
		return profile, errors.Wrap(err, "GetProfile unable to complete search")
	}

	logrus.Debugf("GetProfile got %d profiles in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)
	LogQueryPartMin(CompProfilesIndex, searchResult, "GetProfile query results")

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var esProfile ESInspecProfile
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esProfile)
				if err == nil {
					return esProfile.convertToInspecProfile()
				}
				logrus.Errorf("Unmarshal error: %s", err.Error())
			}
		}
	}

	return profile, utils.ProcessNotFound(nil, profile.Name)
}

//GetProfileSummaryByProfileId across nodes - report 8 - top
func (backend ES2Backend) GetProfileSummaryByProfileId(profileId string, filters map[string][]string) (*stats.ProfileSummary, error) {
	esIndex, err := GetEsIndex(filters, false, false)
	if err != nil {
		return nil, errors.Wrap(err, "GetProfileSummaryByProfileId, unable to get index")
	}

	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()
	if err != nil {
		return nil, errors.Wrap(err, "GetProfileSummaryByProfileId, cannot connect to ElasticSearch")
	}

	// We are not filtering by ProfileID, we are passing it in as a uri resource.
	//filtQuery := backend.getFiltersQuery(filters)

	profileIDQuery := elastic.NewTermQuery("profiles.sha256", profileId)

	reportIdsAndProfileIDQuery := elastic.NewBoolQuery()
	reportIdsAndProfileIDQuery = reportIdsAndProfileIDQuery.Must(profileIDQuery)

	profilesMinQuery := elastic.NewNestedQuery("profiles", reportIdsAndProfileIDQuery)

	passedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "skipped"))

	controls := elastic.NewNestedAggregation().Path("profiles.controls")
	controls.SubAggregation("passed", passedFilterAgg)
	controls.SubAggregation("failed", failedFilterAgg)
	controls.SubAggregation("skipped", skippedFilterAgg)

	profilesFilter := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.sha256", profileId))
	profilesFilter.SubAggregation("controls", controls)

	profilesAgg := elastic.NewNestedAggregation().Path("profiles")
	profilesAgg.SubAggregation("profiles_filter", profilesFilter)

	searchSource := elastic.NewSearchSource().
		Query(profilesMinQuery).
		Aggregation("profiles", profilesAgg).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "GetProfileSummaryByProfileId unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetProfileSummaryByProfileId query")

	searchResult, err := client.Search().
		Index(esIndex).
		FilterPath("took,aggregations").
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		return nil, errors.Wrap(err, "GetProfileSummaryByProfileId unable to complete search")
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetProfileSummaryByProfileId - search results controls")

	logrus.Debugf("GetProfileSummaryByProfileId got %d control failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	var profilesAggResult, profilesFilterAggResult, controlsAggResult *elastic.AggregationSingleBucket

	profilesAggResult, _ = searchResult.Aggregations.Nested("profiles")
	if profilesAggResult != nil {
		profilesFilterAggResult, _ = profilesAggResult.Aggregations.Filter("profiles_filter")
	}
	if profilesFilterAggResult != nil {
		controlsAggResult, _ = profilesFilterAggResult.Aggregations.Nested("controls")
	}
	if controlsAggResult != nil {
		profileMeta, err := backend.getProfileMetadata(profileId)
		if err != nil {
			return nil, errors.Wrap(err, "GetProfileSummaryByProfileId unable to retrieve profile metadata")
		}
		passedCount, _ := controlsAggResult.Filter("passed")
		failedCount, _ := controlsAggResult.Filter("failed")
		skippedCount, _ := controlsAggResult.Filter("skipped")

		profileMeta.Stats.Passed = int32(passedCount.DocCount)
		profileMeta.Stats.Failed = int32(failedCount.DocCount)
		profileMeta.Stats.Skipped = int32(skippedCount.DocCount)

		return profileMeta, nil
	}
	return nil, utils.ProcessNotFound(nil, profileId)

}

// internal helper method to get profile meta information for report 8
func (backend *ES2Backend) getProfileMetadata(profileID string) (*stats.ProfileSummary, error) {
	client, err := backend.ES2Client()

	if err != nil {
		return nil, errors.Wrap(err, "getProfileMetadata, cannot connect to ElasticSearch")
	}
	esIndex := CompProfilesIndex

	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(profileID)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"took",
		"name",
		"version",
		"title",
		"license",
		"supports",
		"maintainer",
		"copyright",
		"copyright_email",
		"summary",
	)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(idsQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getProfileMetadata unable to get Source")
	}

	LogQueryPartMin(esIndex, source, "getProfileMetadata query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult, "getProfileMetadata - search results")

	logrus.Debugf("getProfileMetadata got %d profiles in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		hit := searchResult.Hits.Hits[0]

		//for _, hit := range searchResult.Hits.Hits {
		prof := &stats.ProfileSummary{Stats: &stats.ProfileSummaryStats{}}
		unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
		if err = unmarshaler.Unmarshal(bytes.NewReader(*hit.Source), prof); err != nil {
			logrus.Errorf("getProfileMetadata unmarshal error: %s", err.Error())
		}

		//leveraging this [expensive] call to get failure/total for profile summary
		profileIDFilterMap := make(map[string][]string)
		profileIDFilterMap["profile_id"] = []string{profileID}

		//todo todoId1 start
		//  IMPORTANT - do we even need this  here?  this never succeeds.. it always returns an error.. need to figure
		// out if we really want this to work and if so, fix it, if not then don't call it!
		// taking this out until we know more.
		//nodes, _, err := backend.getAllNodesForProfile(
		//	0, 0, profileIDFilterMap, "latest_report.end_time", false)
		//
		//if err != nil {
		//	logrus.Error("getProfileMetadata could not retrieve nodes info")
		//}
		//
		//failures := 0
		//for _, node := range nodes {
		//	if node.LatestReport.Status == "failed" {
		//		failures++
		//	}
		//}
		//
		//prof.Stats.TotalNodes = int32(len(nodes))
		//prof.Stats.FailedNodes = int32(failures)
		//todo ^todoId1 end

		return prof, nil
	}

	return nil, utils.ProcessNotFound(nil, profileID)
}

// internal helper method to get control meta information
func (backend *ES2Backend) getControlsMetadata(profileId string) (map[string]ControlMeta, error) {
	controlMetaMap := make(map[string]ControlMeta)
	client, err := backend.ES2Client()

	esIndex := CompProfilesIndex

	if err != nil {
		return nil, errors.Wrap(err, "getControlsMetadata, cannot connect to ElasticSearch")
	}

	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(profileId)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"took",
		"controls.impact",
		"controls.title",
		"controls.id",
	)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(idsQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getControlsMetadata unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getControlsMetadata query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(context.Background())

	if err != nil {
		return nil, errors.Wrap(err, "getControlsMetadata unable to complete search")
	}

	LogQueryPartMin(esIndex, searchResult, "getControlsMetadata - search results")

	logrus.Debugf("getControlsMetadata got %d profiles in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var esProfile ESInspecProfile
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esProfile)
				if err != nil {
					logrus.Errorf("getControlsMetadata unmarshal error: %s", err.Error())
				} else {
					for _, control := range esProfile.Controls {
						controlMetaMap[control.ID] = ControlMeta{
							Title:  control.Title,
							Name:   control.ID,
							Impact: control.Impact}
					}
					return controlMetaMap, nil
				}
			}
		}
	}

	return controlMetaMap, utils.ProcessNotFound(nil, profileId)
}

// TODO: header with amount of results
//GetAllProfilesFromNodes - list all of the profiles from scan data
func (backend *ES2Backend) GetAllProfilesFromNodes(from int32, size int32, filters map[string][]string, sort_field string,
	sort_asc bool) ([]*reportingapi.ProfileMin, *reportingapi.ProfileCounts, error) {

	client, err := backend.ES2Client()

	if err != nil {
		return nil, nil, errors.Wrap(err, "GetAllProfilesFromNodes, cannot connect to ElasticSearch")
	}

	var inspecProfilesQuery elastic.Query

	query := elastic.NewIdsQuery(mappings.DocType)
	//if one of the "other" filters are sent in, regardless of profile_id, we need to get the ids from scans
	profileMins, counts, err := backend.getProfileMinsFromNodes(filters)
	if err != nil {
		return nil, nil, errors.Wrap(err, "GetAllProfilesFromNodes, cannot get profileIDs from nodes")
	}

	profileIDs := make([]string, 0)
	for _, profileMin := range profileMins {
		profileIDs = append(profileIDs, profileMin.ID)
		logrus.Debugf("profile id: %s", profileMin.ID)
	}

	query.Ids(profileIDs...)
	inspecProfilesQuery = query

	fsc := elastic.NewFetchSourceContext(true).Include(
		"name",
		"title",
		"version")

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(inspecProfilesQuery).
		Sort(sort_field, sort_asc).
		From(int(from)).
		Size(int(size))

	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Wrap(err, "GetAllProfilesFromNodes unable to get Source")
	}

	esIndex := CompProfilesIndex
	LogQueryPartMin(esIndex, source, "GetAllProfilesFromNodes query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source").
		Do(context.Background())

	LogQueryPartMin(esIndex, searchResult, "GetAllProfilesFromNodes - search result")

	profiles := make([]*reportingapi.ProfileMin, 0)
	if err != nil {
		// should we be returning the error here...instead of logging it and then returning nil for the error?
		logrus.Errorf("GetAllProfilesFromNodes: Error while trying to get data from ES for index: %s error: %s",
			esIndex, err.Error())
		return profiles, nil, nil
	}

	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var profile reportingapi.ProfileMin
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &profile)
				if err == nil {
					profile.Id = hit.Id
					profile.Status = profileMins[profile.Id].Status
					profiles = append(profiles, &profile)
				} else {
					logrus.Errorf("GetAllProfilesFromNodes unmarshal error: %s", err.Error())
				}
			}
		}
		return profiles, counts, nil
	}

	logrus.Debugf("GetAllProfilesFromNodes, found no profiles\n")
	return profiles, counts, nil
}

func (backend ES2Backend) getProfileMinsFromNodes(
	filters map[string][]string) (map[string]reporting.ProfileMin, *reportingapi.ProfileCounts, error) {

	profileMins := make(map[string]reporting.ProfileMin)

	for filterName, filterValue := range filters {
		logrus.Debugf("getProfileMinsFromNodes, filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()
	if err != nil {
		return profileMins, nil, errors.Wrap(err, "getProfileMinsFromNodes, cannot connect to ElasticSearch")
	}

	statusFilters := filters["status"]
	// clearing the filters because we want to filter profiles based on their status not the overall scan status
	filters["status"] = make([]string, 0)

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return profileMins, nil, errors.Wrap(err, "getProfileMinsFromNodes")
	}

	filtQuery := backend.getFiltersQuery(filters, true, true)

	termsQuery := elastic.NewTermsAggregation().
		Field("profiles.profile").Size(reporting.ESize)
	termsQuery.SubAggregation("failures", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.total"))
	termsQuery.SubAggregation("passed", elastic.NewSumAggregation().
		Field("profiles.controls_sums.passed.total"))
	termsQuery.SubAggregation("skipped", elastic.NewSumAggregation().
		Field("profiles.controls_sums.skipped.total"))
	termsQuery.SubAggregation("status", elastic.NewTermsAggregation().
		Field("profiles.status"))

	aggs := elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("totals", termsQuery)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("profiles", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Wrap(err, "getProfileMinsFromNodes unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getProfileMinsFromNodes query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"aggregations.profiles.totals.buckets.key",
			"aggregations.profiles.totals.buckets").
		Do(context.Background())

	if err != nil {
		return profileMins, nil, errors.Wrap(err, "getProfileMinsFromNodes unable to complete search")
	}
	LogQueryPartMin(esIndex, searchResult, "getProfileMinsFromNodes - search results")

	statusMap := make(map[string]int, 3)
	outermostAgg, _ := searchResult.Aggregations.Nested("profiles")
	if outermostAgg != nil {
		totalsAgg, _ := outermostAgg.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				profile_name, profile_id := rightSplit(string(bucket.KeyNumber), "|") // bucket.KeyNumber

				if profilesFilterArray, found := filters["profile_id"]; found {
					if !stringutils.SliceContains(profilesFilterArray, profile_id) {
						continue
					}
				}

				// Using the status of the profile, introduced with inspec 3.0 to overwrite the status calculations from totals
				profileStatusHash := make(map[string]int64, 0)
				statuses, _ := bucket.Aggregations.Terms("status")
				if statuses.Buckets != nil {
					for _, statusBucket := range statuses.Buckets {
						status := statusBucket.Key.(string)
						profileStatusHash[status] = statusBucket.DocCount
					}
				}

				var profileStatus string
				if profileStatusHash["skipped"] > 0 && profileStatusHash["loaded"] == 0 && profileStatusHash[""] == 0 {
					profileStatus = "skipped"
					logrus.Debugf("getProfileMinsFromNodes profile_name=%q, status=%q", profile_name, profileStatus)
				} else {
					sumFailures, _ := bucket.Aggregations.Sum("failures")
					sumPassed, _ := bucket.Aggregations.Sum("passed")
					sumSkipped, _ := bucket.Aggregations.Sum("skipped")
					profileStatus = computeStatus(int32(*sumFailures.Value), int32(*sumPassed.Value), int32(*sumSkipped.Value))
					logrus.Debugf("getProfileMinsFromNodes profile_name=%s, status=%s (sumFailures=%d, sumPassed=%d, sumSkipped=%d)", profile_name, profileStatus, int32(*sumFailures.Value), int32(*sumPassed.Value), int32(*sumSkipped.Value))
				}

				if len(statusFilters) > 0 && !stringutils.SliceContains(statusFilters, profileStatus) {
					continue
				}

				statusMap[profileStatus]++

				summary := reporting.ProfileMin{
					Name:   profile_name,
					ID:     profile_id,
					Status: profileStatus,
				}
				profileMins[profile_id] = summary
			}
		}
	}
	logrus.Debugf("Done with statusMap=%+v", statusMap)
	logrus.Debugf("Done with statusMap['something']=%+v", statusMap["passed"])
	counts := &reportingapi.ProfileCounts{
		Total:   int32(statusMap["failed"] + statusMap["passed"] + statusMap["skipped"]),
		Failed:  int32(statusMap["failed"]),
		Passed:  int32(statusMap["passed"]),
		Skipped: int32(statusMap["skipped"]),
	}
	return profileMins, counts, nil
}

func computeStatus(failed int32, passed int32, skipped int32) string {
	if failed > 0 {
		return "failed"
	} else if passed > 0 || skipped == 0 {
		return "passed"
	} else if passed == 0 && skipped > 0 {
		return "skipped"
	}
	return "unknown"
}

// StoreProfile stores an InSpec profile to ES
func (backend ES2Backend) StoreProfile(profile inspec.Profile) error {
	// we reject storing a profile where the sha sum is missing
	if profile.Sha256 == "" {
		return fmt.Errorf("profile " + profile.Name + " does not include the required sha256 sum")
	}

	client, err := backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, "StoreProfile, cannot connect to ElasticSearch")
	}

	var esProfile ESInspecProfile
	err = esProfile.parseInspecProfile(profile)
	if err != nil {
		return errors.Wrap(err, "StoreProfile, unable to parse inspec profile")
	}

	// Add a document to the index
	_, err = client.Index().
		Index(CompProfilesIndex).
		Type(mappings.DocType).
		Id(esProfile.Sha256).
		BodyJson(esProfile).
		Refresh("true").
		Do(context.Background())
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("StoreProfile, Could not store Profile %s", esProfile.Sha256))
	}
	logrus.Debugf("Stored profile %s", esProfile.Sha256)
	return nil
}

func convertRSProfileDependenciesToInspecDependencies(profileDependencies []*reportingapi.Dependency) (supports []inspec.Dependency) {
	for _, profileDependency := range profileDependencies {
		supports = append(supports, rsProfilesDependencyToInspecDependency(profileDependency))
	}
	return
}

func rsProfilesDependencyToInspecDependency(profilesDependency *reportingapi.Dependency) (dependency inspec.Dependency) {
	return inspec.Dependency{
		Name:        profilesDependency.Name,
		URL:         profilesDependency.Url,
		Path:        profilesDependency.Path,
		Git:         profilesDependency.Git,
		Branch:      profilesDependency.Branch,
		Tag:         profilesDependency.Tag,
		Commit:      profilesDependency.Commit,
		Version:     profilesDependency.Version,
		Supermarket: profilesDependency.Supermarket,
		Github:      profilesDependency.Github,
		Compliance:  profilesDependency.Compliance,
		// no need for Status and SkipMessage here as it's not static metadata of the profile
	}
}

func convertInspecSupportsToRSSupports(supports []*ingestinspec.Support) (inspecSupports []*reportingapi.Support) {
	for _, support := range supports {
		inspecSupports = append(inspecSupports, inspecSupportToRSSupport(support))
	}
	return
}

func inspecSupportToRSSupport(support *ingestinspec.Support) (inspecSupport *reportingapi.Support) {
	return &reportingapi.Support{
		OsName:   support.OsName,
		OsFamily: support.OsFamily,
		Platform: support.Platform,
		Release:  support.Release,
	}
}

func convertMapSupportsToRSSupports(inspecSupports []map[string]string) (supports []*reportingapi.Support) {
	for _, inspecSupport := range inspecSupports {
		supports = append(supports, mapSupportToRSSupport(inspecSupport))
	}
	return
}

func mapSupportToRSSupport(inspecSupport map[string]string) (support *reportingapi.Support) {
	family := inspecSupport["os-family"]
	if len(family) == 0 {
		family = inspecSupport["platform-family"]
	}
	name := inspecSupport["os-name"]
	if len(name) == 0 {
		name = inspecSupport["platform-name"]
	}
	return &reportingapi.Support{
		OsName:        name,
		OsFamily:      family,
		Platform:      inspecSupport["platform"],
		Release:       inspecSupport["release"],
		InspecVersion: inspecSupport["inspec-version"],
	}
}

func convertMapSupportsToInspecSupports(inspecSupports []map[string]string) (supports []*ingestinspec.Support) {
	for _, inspecSupport := range inspecSupports {
		supports = append(supports, mapSupportToInspecSupport(inspecSupport))
	}
	return
}

func mapSupportToInspecSupport(inspecSupport map[string]string) (support *ingestinspec.Support) {
	family := inspecSupport["os-family"]
	if len(family) == 0 {
		family = inspecSupport["platform-family"]
	}
	name := inspecSupport["os-name"]
	if len(name) == 0 {
		name = inspecSupport["platform-name"]
	}
	return &ingestinspec.Support{
		OsName:   name,
		OsFamily: family,
		Platform: inspecSupport["platform"],
		Release:  inspecSupport["release"],
	}
}

func convertInspecDependenciesToRSDependencies(inspecDependencies []inspec.Dependency) (supports []*reportingapi.Dependency) {
	for _, inspecDependency := range inspecDependencies {
		supports = append(supports, inspecDependencyToRSDependency(&inspecDependency))
	}
	return
}

func inspecDependencyToRSDependency(inspecDependency *inspec.Dependency) (dependency *reportingapi.Dependency) {
	return &reportingapi.Dependency{
		Name:        inspecDependency.Name,
		Url:         inspecDependency.URL,
		Path:        inspecDependency.Path,
		Git:         inspecDependency.Git,
		Branch:      inspecDependency.Branch,
		Tag:         inspecDependency.Tag,
		Commit:      inspecDependency.Commit,
		Version:     inspecDependency.Version,
		Supermarket: inspecDependency.Supermarket,
		Github:      inspecDependency.Github,
		Compliance:  inspecDependency.Compliance,
		Status:      inspecDependency.Status,
		SkipMessage: inspecDependency.SkipMessage,
	}
}
