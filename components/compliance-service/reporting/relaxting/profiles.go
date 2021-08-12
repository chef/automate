package relaxting

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/golang/protobuf/jsonpb"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/external/lib/errorutils"
	ingestinspec "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
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

	groups := make([]inspec.Group, 0, len(profile.Groups))
	for _, group := range profile.Groups {
		eGroup := inspec.Group{
			ID:       group.Id,
			Title:    &group.Title,
			Controls: group.Controls,
		}
		groups = append(groups, eGroup)
	}
	esprofile.Groups = groups

	esprofile.Attributes = make([]ESInspecAttribute, 0, len(profile.Attributes))
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
		json.Unmarshal(data, &defaultVal) // nolint: errcheck
		esAttribute.Options.Default = defaultVal

		esprofile.Attributes = append(esprofile.Attributes, esAttribute)
	}

	esprofile.Controls = make([]ESInspecControl, 0, len(profile.Controls))
	// we need to marshal refs and tags for each control
	for _, control := range profile.Controls {
		esControl := ESInspecControl{
			Code:   control.Code,
			Desc:   control.Desc,
			Impact: control.Impact,
			ID:     control.ID,
			Title:  control.Title,
		}

		results := make([]reportingapi.Result, 0, len(control.Results))
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
		json.Unmarshal(byteRefs, &refs) // nolint: errcheck
		esControl.Refs = refs

		bytetags, _ := json.Marshal(control.Tags)
		esControl.Tags = string(bytetags)

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

	groups := make([]*reportingapi.Group, 0, len(esprofile.Groups))
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
	inspecProfile.Attributes = make([]*reportingapi.Attribute, 0, len(esprofile.Attributes))
	for _, esAttribute := range esprofile.Attributes {
		attribute := reportingapi.Attribute{
			Name: esAttribute.Name,
		}
		esOption := attribute.Options
		if esOption != nil {
			options := reportingapi.Option{
				Description: esOption.Description,
			}
			if err := json.Unmarshal([]byte(esOption.Default), &options.Default); err != nil {
				return inspecProfile, errors.Wrap(err, "convertToInspecProfile unable to unmarshal options")
			}

			attribute.Options = &options
		}

		inspecProfile.Attributes = append(inspecProfile.Attributes, &attribute)
	}

	// we need to unmarshal refs and tags for each control
	inspecProfile.Controls = make([]*reportingapi.Control, 0, len(esprofile.Controls))
	for _, esControl := range esprofile.Controls {
		control := reportingapi.Control{
			Code:   esControl.Code,
			Desc:   esControl.Desc,
			Impact: esControl.Impact,
			Id:     esControl.ID,
			Title:  esControl.Title,
		}
		results := make([]*reportingapi.Result, 0, len(esControl.Results))
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
		json.Unmarshal(byteRefs, &refs) // nolint: errcheck
		control.Refs = refs

		var tags string
		byteTags, _ := json.Marshal(esControl.Tags)
		json.Unmarshal(byteTags, &tags) // nolint: errcheck
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

	// This is too verbose even for debug logging. Keeping it off unless needed for troubleshooting
	// LogQueryPartMin(CompProfilesIndex, searchResult, "GetProfile query results")

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

	return profile, errorutils.ProcessNotFound(nil, profile.Name)
}

//GetProfileSummaryByProfileId across nodes - report 8 - top
//this is the summary that appears at the top of the page when you select a profile from profiles list at the moment
// this one does not immediately need to be deep aware as the only things that are used from it in a2 api are version, maintainer and license
// todo - deep filtering - this should be made depth aware as this still needs to be consumed by api users
// todo - do we need to handle waiver info in here too?
func (backend ES2Backend) GetProfileSummaryByProfileId(profileId string, filters map[string][]string) (*stats.ProfileSummary, error) {
	// Only end_time matters for this call
	filters["start_time"] = []string{}
	esIndex, err := GetEsIndex(filters, false)
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
	filtQuery := backend.getFiltersQuery(filters, true)

	profileIDQuery := elastic.NewTermQuery("profiles.sha256", profileId)

	reportIdsAndProfileIDQuery := elastic.NewBoolQuery()
	reportIdsAndProfileIDQuery = reportIdsAndProfileIDQuery.Must(profileIDQuery)

	profilesMinQuery := elastic.NewNestedQuery("profiles", reportIdsAndProfileIDQuery)
	filtQuery = filtQuery.Must(profilesMinQuery)

	waivedQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes", "yes_run")
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "passed")).
		MustNot(waivedQuery))

	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "failed")).
		MustNot(waivedQuery))

	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "skipped")).
		MustNot(waivedQuery))

	waivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	controls := elastic.NewNestedAggregation().Path("profiles.controls")
	controls.SubAggregation("passed", passedFilter)
	controls.SubAggregation("failed", failedFilter)
	controls.SubAggregation("skipped", skippedFilter)
	controls.SubAggregation("waived", waivedFilter)

	profilesFilter := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.sha256", profileId))
	profilesFilter.SubAggregation("controls", controls)

	profilesAgg := elastic.NewNestedAggregation().Path("profiles")
	profilesAgg.SubAggregation("profiles_filter", profilesFilter)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
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
		waivedCount, _ := controlsAggResult.Filter("waived")

		profileMeta.Stats.Passed = int32(passedCount.DocCount)
		profileMeta.Stats.Failed = int32(failedCount.DocCount)
		profileMeta.Stats.Skipped = int32(skippedCount.DocCount)
		profileMeta.Stats.Waived = int32(waivedCount.DocCount)

		return profileMeta, nil
	}
	return nil, errorutils.ProcessNotFound(nil, profileId)

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
		"depends",
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

		return prof, nil
	}

	return nil, errorutils.ProcessNotFound(nil, profileID)
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

	return controlMetaMap, errorutils.ProcessNotFound(nil, profileId)
}

// TODO: header with amount of results
//GetAllProfilesFromNodes - list all of the profiles from scan data
func (backend *ES2Backend) GetAllProfilesFromNodes(from int32, size int32, filters map[string][]string, sort_field string,
	sort_asc bool) ([]*reportingapi.ProfileMin, *reportingapi.ProfileCounts, error) {
	myName := "GetAllProfilesFromNodes"
	client, err := backend.ES2Client()

	if err != nil {
		return nil, nil, errors.Wrapf(err, "%s, cannot connect to ElasticSearch", myName)
	}

	//if one of the "other" filters are sent in, regardless of profile_id, we need to get the ids from scans
	profileMins, counts, err := backend.getProfileMinsFromNodes(filters)
	if err != nil {
		return nil, nil, errors.Wrapf(err, "%s, cannot get profileIDs from nodes", myName)
	}
	logrus.Debugf("Got from nodes profileMins=%+v", profileMins)

	profileIDs := make([]string, len(profileMins))
	profileIDsStatusMap := make(map[string]string, len(profileMins))
	for i, profileMin := range profileMins {
		profileIDs[i] = profileMin.ID
		profileIDsStatusMap[profileMin.ID] = profileMin.Status
	}

	logrus.Debugf("%s querying the profiles index with ids: %v", myName, profileIDs)
	query := elastic.NewIdsQuery(mappings.DocType)
	query.Ids(profileIDs...)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"name",
		"title",
		"version")

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(query).
		Sort(sort_field, sort_asc).
		From(int(from)).
		Size(int(size))

	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Wrapf(err, "%s unable to get Source", myName)
	}

	esIndex := CompProfilesIndex
	LogQueryPartMin(esIndex, source, fmt.Sprintf("%s query searchSource", myName))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source").
		Do(context.Background())
	if err != nil {
		return nil, nil, err
	}

	LogQueryPartMin(esIndex, searchResult, fmt.Sprintf("%s - search result", myName))

	profiles := make([]*reportingapi.ProfileMin, 0)
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		// Loop over the data from the compliance-profiles metadata index
		for _, hit := range searchResult.Hits.Hits {
			var profile reportingapi.ProfileMin
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &profile)
				if err == nil {
					profile.Id = hit.Id
					profile.Status = profileIDsStatusMap[profile.Id]
					profiles = append(profiles, &profile)
				} else {
					logrus.Errorf("%s unmarshal error: %s", myName, err.Error())
				}
			}
		}
		logrus.Debugf("%s returning profiles=%+v with counts %+v", myName, profiles, counts)
		return profiles, counts, nil
	}

	logrus.Debugf("%s, found no profiles\n", myName)
	return profiles, counts, nil
}

func (backend ES2Backend) getProfileMinsFromNodes(
	filters map[string][]string) ([]reporting.ProfileMin, *reportingapi.ProfileCounts, error) {
	myName := "getProfileMinsFromNodes"

	for filterName, filterValue := range filters {
		logrus.Debugf("%s, filter: name=>%s value=>%s\n", myName, filterName, filterValue)
	}

	statusFilters := filters["status"]
	// clearing the filters because we want to filter profiles based on their status not the overall scan status
	filters["status"] = make([]string, 0)

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getProfileMinsFromNodesAggs(filters) {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Wrapf(err, "%s unable to get Source", myName)
	}
	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		Index(queryInfo.esIndex).
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		return nil, nil, errors.Wrapf(err, "%s unable to complete search", myName)
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult, fmt.Sprintf("%s - search results", myName))

	return depth.getProfileMinsFromNodesResults(filters, searchResult, statusFilters)
}

func computeStatus(failed int32, passed int32, skipped int32, waived int32) string {
	if failed > 0 {
		return "failed"
	} else if passed == 0 && skipped == 0 && waived > 0 {
		return "waived"
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
