package params

import "github.com/chef/automate/api/external/common/query"

const (
	DEFAULT_PAGE      int32 = 1
	DEFAULT_PAGE_SIZE int32 = 25
)

func GetPageParams(p *query.Pagination) (int32, int32) {
	var (
		page = DEFAULT_PAGE
		size = DEFAULT_PAGE_SIZE
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
