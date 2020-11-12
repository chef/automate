package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type AggregationSingleBucket interface {
	DocCount() int64
}

type Olivere7AggregationSingleBucket struct {
	aggSingleBucket *elastic7.AggregationSingleBucket
}

type Olivere6AggregationSingleBucket struct {
	aggSingleBucket *elastic6.AggregationSingleBucket
}

func (agg *Olivere7AggregationSingleBucket) DocCount() int64 {
	return agg.aggSingleBucket.DocCount
}

func (agg *Olivere6AggregationSingleBucket) DocCount() int64 {
	return agg.aggSingleBucket.DocCount
}
