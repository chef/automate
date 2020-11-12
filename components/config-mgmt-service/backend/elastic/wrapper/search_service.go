package wrapper

import (
	"context"

	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type SearchService interface {
	SearchSource(SearchSource) SearchService
	Query(Query) SearchService
	Index(...string) SearchService
	Do(context.Context) (SearchResult, error)
	Size(int) SearchService
	Sort(string, bool) SearchService
	FetchSourceContext(FetchSourceContext) SearchService
	SearchAfter(sortValues ...interface{}) SearchService
	From(int) SearchService
	Aggregation(name string, agg Aggregation) SearchService
}

type Query interface {
	Source() (interface{}, error)
}

type Olivere7SearchService struct {
	searchService *elastic7.SearchService
}

type Olivere6SearchService struct {
	searchService *elastic6.SearchService
}

func (ss *Olivere7SearchService) SearchSource(searchSource SearchSource) SearchService {
	ss.searchService.SearchSource(searchSource.OlivereSource().(*elastic7.SearchSource))

	return ss
}

func (ss *Olivere6SearchService) SearchSource(searchSource SearchSource) SearchService {
	ss.searchService.SearchSource(searchSource.OlivereSource().(*elastic6.SearchSource))

	return ss
}

func (ss *Olivere7SearchService) Index(index ...string) SearchService {
	ss.searchService.Index(index...)

	return ss
}

func (ss *Olivere6SearchService) Index(index ...string) SearchService {
	ss.searchService.Index(index...)

	return ss
}

func (ss *Olivere7SearchService) Query(query Query) SearchService {
	ss.searchService.Size(3)
	ss.searchService.Query(query)

	return ss
}

func (ss *Olivere6SearchService) Query(query Query) SearchService {
	ss.searchService.Query(query)

	return ss
}

func (ss *Olivere7SearchService) Size(size int) SearchService {
	ss.searchService.Size(size)

	return ss
}

func (ss *Olivere6SearchService) Size(size int) SearchService {
	ss.searchService.Size(size)

	return ss
}

func (ss *Olivere7SearchService) Sort(field string, ascending bool) SearchService {
	ss.searchService.Sort(field, ascending)

	return ss
}

func (ss *Olivere6SearchService) Sort(field string, ascending bool) SearchService {
	ss.searchService.Sort(field, ascending)

	return ss
}

func (ss *Olivere7SearchService) FetchSourceContext(fetchSourceContext FetchSourceContext) SearchService {
	ss.searchService.FetchSourceContext(
		fetchSourceContext.OlivereSource().(*elastic7.FetchSourceContext))

	return ss
}

func (ss *Olivere6SearchService) FetchSourceContext(fetchSourceContext FetchSourceContext) SearchService {
	ss.searchService.FetchSourceContext(
		fetchSourceContext.OlivereSource().(*elastic6.FetchSourceContext))

	return ss
}

func (ss *Olivere7SearchService) SearchAfter(sortValues ...interface{}) SearchService {
	ss.searchService.SearchAfter(sortValues...)
	return ss
}

func (ss *Olivere6SearchService) SearchAfter(sortValues ...interface{}) SearchService {
	ss.searchService.SearchAfter(sortValues...)
	return ss
}

func (ss *Olivere7SearchService) From(from int) SearchService {
	ss.searchService.From(from)
	return ss
}

func (ss *Olivere6SearchService) From(from int) SearchService {
	ss.searchService.From(from)
	return ss
}

func (ss *Olivere7SearchService) Aggregation(name string, agg Aggregation) SearchService {
	ss.searchService.Aggregation(name, agg)
	return ss
}

func (ss *Olivere6SearchService) Aggregation(name string, agg Aggregation) SearchService {
	ss.searchService.Aggregation(name, agg)
	return ss
}

func (ss *Olivere7SearchService) Do(ctx context.Context) (SearchResult, error) {
	result, err := ss.searchService.Do(ctx)
	return &Olivere7SearchResult{result}, err
}

func (ss *Olivere6SearchService) Do(ctx context.Context) (SearchResult, error) {
	result, err := ss.searchService.Do(ctx)
	return &Olivere6SearchResult{result}, err
}
