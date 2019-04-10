package params

import "github.com/chef/automate/api/external/common/query"

const (
	DEFAULT_SORT_FIELD     = "name"
	DEFAULT_SORT_ASCENDING = true
)

func GetSortParams(s *query.Sorting) (string, bool) {
	var (
		sortField = DEFAULT_SORT_FIELD
		sortAsc   = DEFAULT_SORT_ASCENDING
	)

	if s != nil {
		if s.GetField() != "" {
			sortField = s.Field
		}
		if s.GetOrder().String() == "DESC" {
			sortAsc = false
		}
	}
	return sortField, sortAsc
}

func IsValidSortField(s *query.Sorting) bool {
	if s != nil {
		switch s.Field {
		case "name":
			return true
		case "percent_ok":
			return true
		case "": // Use default sorting
			return true
		default:
			return false
		}
	} // If there is no sort field we will use default sorting
	return true
}
