package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type SearchResult interface {
	TotalHits() int64
	Aggregations() Aggregations
	HitsHits() []SearchHit
}

type Olivere7SearchResult struct {
	searchResult *elastic7.SearchResult
}

type Olivere6SearchResult struct {
	searchResult *elastic6.SearchResult
}

func (sr *Olivere7SearchResult) TotalHits() int64 {
	return sr.searchResult.TotalHits()
}

func (sr *Olivere6SearchResult) TotalHits() int64 {
	return sr.searchResult.TotalHits()
}

func (sr *Olivere7SearchResult) Aggregations() Aggregations {
	return &Olivere7Aggregations{&sr.searchResult.Aggregations}
}

func (sr *Olivere6SearchResult) Aggregations() Aggregations {
	return &Olivere6Aggregations{&sr.searchResult.Aggregations}
}

func (sr *Olivere7SearchResult) HitsHits() []SearchHit {
	shs := make([]SearchHit, len(sr.searchResult.Hits.Hits))
	for index, hit := range sr.searchResult.Hits.Hits {
		shs[index] = &Olivere7SearchHit{hit}
	}

	return shs
}

func (sr *Olivere6SearchResult) HitsHits() []SearchHit {
	shs := make([]SearchHit, len(sr.searchResult.Hits.Hits))
	for index, hit := range sr.searchResult.Hits.Hits {
		shs[index] = &Olivere6SearchHit{hit}
	}

	return shs
}
