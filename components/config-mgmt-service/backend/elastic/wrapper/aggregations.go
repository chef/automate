package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type Aggregations interface {
	Terms(string) (AggregationBucketKeyItems, bool)
	Range(string) (AggregationBucketRangeItems, bool)
	Filter(string) (AggregationSingleBucket, bool)
	Composite(string) (AggregationBucketCompositeItems, bool)
}

type Olivere7Aggregations struct {
	aggregations *elastic7.Aggregations
}

type Olivere6Aggregations struct {
	aggregations *elastic6.Aggregations
}

func (agg *Olivere7Aggregations) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregations.Terms(name)
	return &Olivere7AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere6Aggregations) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregations.Terms(name)
	return &Olivere6AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere7Aggregations) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregations.Range(name)
	return &Olivere7AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere6Aggregations) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregations.Range(name)
	return &Olivere6AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere7Aggregations) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregations.Filter(name)
	return &Olivere7AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere6Aggregations) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregations.Filter(name)
	return &Olivere6AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere7Aggregations) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregations.Composite(name)
	return &Olivere7AggregationBucketCompositeItems{aggItems}, ok
}

func (agg *Olivere6Aggregations) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregations.Composite(name)
	return &Olivere6AggregationBucketCompositeItems{aggItems}, ok
}
