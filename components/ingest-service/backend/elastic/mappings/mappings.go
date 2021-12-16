//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package mappings

import (
	"fmt"
	"time"
)

const (
	// The format of the date string on the end of the index name
	TimeseriesDateFmt = "2006.01.02"

	DocType = "_doc"
	//run info for telematics
	NodeCurrentRunInfoVersion = "1"
	nodeAndVersionRunInfo     = "node-" + NodeCurrentRunInfoVersion
	IndexNameNodeRunInfo      = nodeAndVersionRunInfo + "-run-info"
)

// AllMappings is the list of all mappings that we currently have
// and it is used by our `InitializeStore()` interface, if we add
// more mappings to this list, the function will automatically
// initialize them
var AllMappings = []Mapping{
	NodeState,
	ConvergeHistory,
	NodeAttribute,
	NodeRunInfo,
}

// Mapping type is the representation of an ES mapping, it contains
// all the necessary fields you need to create a mapping and to insert
// documents to it
type Mapping struct {
	Index      string
	Alias      string
	Type       string
	Timeseries bool
	Mapping    string
	Properties string
}

// Index will return the name of the index plus a date string associated to the Mapping.
// The format will be (index-name) -YYYY.MM.DD
func (m *Mapping) IndexTimeseriesFmt(date time.Time) string {
	utcDate := date.UTC().Format(TimeseriesDateFmt)
	return fmt.Sprintf("%s-%s",
		m.Index,
		utcDate,
	)
}

func (m *Mapping) IndexTimeseriesScanDate(indexName string) (time.Time, error) {
	var dateStr string
	if _, err := fmt.Sscanf(indexName, m.Index+"-%s", &dateStr); err != nil {
		return time.Time{}, err
	}

	return time.Parse("2006.01.02", dateStr)
}
