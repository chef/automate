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
)

// AllMappings is the list of all mappings that we currently have
// and it is used by our `InitializeStore()` interface, if we add
// more mappings to this list, the function will automatically
// initialize them
var AllMappings = []Mapping{
	Actions,
	NodeState,
	ConvergeHistory,
	NodeAttribute,
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
