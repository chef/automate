package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type AggregationBucketRangeItems interface {
	Aggregations
	Buckets() []AggregationBucketRangeItem
}

type AggregationBucketRangeItem interface {
	Key() string
	DocCount() int64
}

type Olivere7AggregationBucketRangeItems struct {
	aggregationBucketRangeItems *elastic7.AggregationBucketRangeItems
}

type Olivere6AggregationBucketRangeItems struct {
	aggregationBucketRangeItems *elastic6.AggregationBucketRangeItems
}

func (agg *Olivere7AggregationBucketRangeItems) Buckets() []AggregationBucketRangeItem {
	items := make([]AggregationBucketRangeItem, len(agg.aggregationBucketRangeItems.Buckets))
	for index, b := range agg.aggregationBucketRangeItems.Buckets {
		items[index] = &Olivere7AggregationBucketRangeItem{b}
	}
	return items
}

func (agg *Olivere6AggregationBucketRangeItems) Buckets() []AggregationBucketRangeItem {
	items := make([]AggregationBucketRangeItem, len(agg.aggregationBucketRangeItems.Buckets))
	for index, b := range agg.aggregationBucketRangeItems.Buckets {
		items[index] = &Olivere6AggregationBucketRangeItem{b}
	}
	return items
}

func (agg *Olivere7AggregationBucketRangeItems) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Range(name)
	return &Olivere7AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketRangeItems) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Range(name)
	return &Olivere6AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere7AggregationBucketRangeItems) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Terms(name)
	return &Olivere7AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketRangeItems) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Terms(name)
	return &Olivere6AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere7AggregationBucketRangeItems) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Filter(name)
	return &Olivere7AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere6AggregationBucketRangeItems) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Filter(name)
	return &Olivere6AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere7AggregationBucketRangeItems) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Composite(name)
	return &Olivere7AggregationBucketCompositeItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketRangeItems) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregationBucketRangeItems.Composite(name)
	return &Olivere6AggregationBucketCompositeItems{aggItems}, ok
}

type Olivere7AggregationBucketRangeItem struct {
	aggregationBucketRangeItem *elastic7.AggregationBucketRangeItem
}

type Olivere6AggregationBucketRangeItem struct {
	aggregationBucketRangeItem *elastic6.AggregationBucketRangeItem
}

func (item *Olivere7AggregationBucketRangeItem) Key() string {
	return item.aggregationBucketRangeItem.Key
}

func (item *Olivere6AggregationBucketRangeItem) Key() string {
	return item.aggregationBucketRangeItem.Key
}

func (item *Olivere7AggregationBucketRangeItem) DocCount() int64 {
	return item.aggregationBucketRangeItem.DocCount
}

func (item *Olivere6AggregationBucketRangeItem) DocCount() int64 {
	return item.aggregationBucketRangeItem.DocCount
}
