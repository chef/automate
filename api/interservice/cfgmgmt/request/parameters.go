//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//
//
// This file is extending the functionality of our parameters
// messages that we defined in `parameters.proto`

package request

import "github.com/chef/automate/components/config-mgmt-service/params"

const (
	DEFAULT_PAGE           int32 = 1
	DEFAULT_PAGE_SIZE      int32 = 10
	DEFAULT_SORT_FIELD           = "name"
	DEFAULT_SORT_ASCENDING       = true
)

func (m *Pagination) GetParameters() (int32, int32) {
	var (
		page = DEFAULT_PAGE
		size = DEFAULT_PAGE_SIZE
	)

	if m != nil {
		if m.Page > 0 {
			page = m.Page
		}
		if m.Size > 0 {
			size = m.Size
		}
	}

	return page, size
}

func (m *Sorting) GetParameters() (string, bool) {
	var (
		sortField = DEFAULT_SORT_FIELD
		sortAsc   = DEFAULT_SORT_ASCENDING
	)

	if m != nil {
		// TODO (afiune) Do we wanna validate the provided sort field?
		if m.Field != "" {
			sortField = m.Field
		}
		if m.Order.String() == "desc" {
			sortAsc = false
		}
	}

	return params.ConvertParamToNodeRunBackend(sortField), sortAsc
}
