package params

import "github.com/chef/automate/api/external/common/query"

const (
	DefaultPage     int32 = 1
	DefaultPageSize int32 = 25
)

func GetPageParams(p *query.Pagination) (int32, int32) {
	var (
		page = DefaultPage
		size = DefaultPageSize
	)

	if p != nil {
		if p.GetPage() > 0 {
			page = p.Page
		}
		if p.GetSize() > 0 {
			size = p.Size
		}
	}

	return page, size
}
