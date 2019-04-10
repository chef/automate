//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package mappings

import (
	"fmt"
	"time"
)

// AllMappings is the list of all mappings that we currently have
// and it is used by our `InitializeStore()` interface, if we add
// more mappings to this list, the function will automatically
// initialize them
var AllMappings = []Mapping{
	ComplianceRepDate,
	ComplianceSumDate,
	ComplianceProfiles,
	ComplianceFeeds,
}

const (

	//N.B. - the two IndicesVersion consts may very well be different from one another.  it's important to only change the one(s) that need migration.
	//if we tied both to the same version, then a simple change to profiles structure would require us to do a full migration of timeseries (which is time consuming)
	//the net result of separating them is that we can migrate them when one or both change by simply incrementing one or both.  cool, right!?
	//ComplianceCurrentTimeSeriesIndicesVersion allows us to know, for any version of compliance, what level we are at with our timeseries indices
	ComplianceCurrentTimeSeriesIndicesVersion = "3"
	//ComplianceCurrentProfilesIndicesVersion allows us to know, for any version of compliance, what level we are at with our profiles and profiles-mappings indices
	ComplianceCurrentProfilesIndicesVersion = "3"
	ComplianceCurrentFeedsIndicesVersion    = "2"

	compAndVersionTimeSeries = "comp-" + ComplianceCurrentTimeSeriesIndicesVersion
	compAndVersionProfiles   = "comp-" + ComplianceCurrentProfilesIndicesVersion
	compAndVersionFeeds      = "comp-" + ComplianceCurrentFeedsIndicesVersion

	DocType        = "_doc"
	IndexNameProf  = compAndVersionProfiles + "-profiles"
	IndexNameFeeds = compAndVersionFeeds + "-feeds"

	IndexNameRep = compAndVersionTimeSeries + "-r"
	IndexNameSum = compAndVersionTimeSeries + "-s"
)

// Mapping type is the representation of an ES mapping, it contains
// all the necessary fields you need to create a mapping and to insert
// documents to it
type Mapping struct {
	Index      string
	Alias      string
	Type       string
	Timeseries bool
	Mapping    string
}

// Index will return the name of the index plus a date string associated to the Mapping.
// The format will be (index-name) -YYYY.MM.DD
func (m *Mapping) IndexTimeseriesFmt(date time.Time) string {
	utcDate := date.UTC().Format("2006.01.02")
	return fmt.Sprintf("%s-%s", m.Index, utcDate)
}
