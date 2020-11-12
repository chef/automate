package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type SearchSource interface {
	OlivereSource() interface{}
	Aggregation(string, Aggregation) SearchSource
	Source() (interface{}, error)
}

type Aggregation interface {
	Source() (interface{}, error)
}

type Olivere7SearchSource struct {
	searchSource *elastic7.SearchSource
}

type Olivere6SearchSource struct {
	searchSource *elastic6.SearchSource
}

func (ss Olivere7SearchSource) OlivereSource() interface{} {
	return ss.searchSource
}

func (ss Olivere6SearchSource) OlivereSource() interface{} {
	return ss.searchSource
}

func (ss Olivere7SearchSource) Aggregation(term string, agg Aggregation) SearchSource {
	ss.searchSource.Aggregation(term, agg)
	return ss
}

func (ss Olivere6SearchSource) Aggregation(term string, agg Aggregation) SearchSource {
	ss.searchSource.Aggregation(term, agg)
	return ss
}

func (ss Olivere7SearchSource) Source() (interface{}, error) {
	return ss.searchSource.Source()
}

func (ss Olivere6SearchSource) Source() (interface{}, error) {
	return ss.searchSource.Source()
}
