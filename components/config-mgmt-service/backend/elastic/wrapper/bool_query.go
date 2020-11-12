package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type BoolQuery interface {
	Must(queries ...elastic6.Query) BoolQuery
	MustNot(queries ...elastic6.Query) BoolQuery
	Should(queries ...elastic6.Query) BoolQuery
	Filter(filters ...elastic6.Query) BoolQuery
	Source() (interface{}, error)
}

type Olivere7BoolQuery struct {
	boolQuery *elastic7.BoolQuery
}

type Olivere6BoolQuery struct {
	boolQuery *elastic6.BoolQuery
}

func (bq *Olivere7BoolQuery) Must(queriesOlivere6 ...elastic6.Query) BoolQuery {
	queries := make([]elastic7.Query, len(queriesOlivere6))
	for index, q := range queriesOlivere6 {
		queries[index] = q.(elastic7.Query)
	}
	bq.boolQuery.Must(queries...)

	return bq
}

func (bq *Olivere6BoolQuery) Must(queries ...elastic6.Query) BoolQuery {
	bq.boolQuery.Must(queries...)

	return bq
}

func (bq *Olivere7BoolQuery) MustNot(queriesOlivere6 ...elastic6.Query) BoolQuery {
	queries := make([]elastic7.Query, len(queriesOlivere6))
	for index, q := range queriesOlivere6 {
		queries[index] = q.(elastic7.Query)
	}
	bq.boolQuery.MustNot(queries...)

	return bq
}

func (bq *Olivere6BoolQuery) MustNot(queries ...elastic6.Query) BoolQuery {
	bq.boolQuery.MustNot(queries...)

	return bq
}

func (bq *Olivere7BoolQuery) Should(queriesOlivere6 ...elastic6.Query) BoolQuery {
	queries := make([]elastic7.Query, len(queriesOlivere6))
	for index, q := range queriesOlivere6 {
		queries[index] = q.(elastic7.Query)
	}
	bq.boolQuery.Should(queries...)

	return bq
}

func (bq *Olivere6BoolQuery) Should(queries ...elastic6.Query) BoolQuery {
	bq.boolQuery.Should(queries...)

	return bq
}

func (bq *Olivere7BoolQuery) Filter(filtersOlivere6 ...elastic6.Query) BoolQuery {
	filters := make([]elastic7.Query, len(filtersOlivere6))
	for index, q := range filtersOlivere6 {
		filters[index] = q.(elastic7.Query)
	}
	bq.boolQuery.Filter(filters...)

	return bq
}

func (bq *Olivere6BoolQuery) Filter(filters ...elastic6.Query) BoolQuery {
	bq.boolQuery.Filter(filters...)

	return bq
}

func (bq *Olivere7BoolQuery) Source() (interface{}, error) {
	return bq.boolQuery.Source()
}

func (bq *Olivere6BoolQuery) Source() (interface{}, error) {
	return bq.boolQuery.Source()
}
