package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type AggregationBucketKeyItems interface {
	Aggregations
	Buckets() []AggregationBucketKeyItem
}

type AggregationBucketKeyItem interface {
	Aggregations
	Key() interface{}
	DocCount() int64
}

type Olivere7AggregationBucketKeyItems struct {
	aggregationBucketKeyItems *elastic7.AggregationBucketKeyItems
}

type Olivere6AggregationBucketKeyItems struct {
	aggregationBucketKeyItems *elastic6.AggregationBucketKeyItems
}

func (agg *Olivere7AggregationBucketKeyItems) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Terms(name)
	return &Olivere7AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketKeyItems) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Terms(name)
	return &Olivere6AggregationBucketKeyItems{aggItems}, ok
}

func (agg *Olivere7AggregationBucketKeyItems) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Range(name)
	return &Olivere7AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketKeyItems) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Range(name)
	return &Olivere6AggregationBucketRangeItems{aggItems}, ok
}

func (agg *Olivere7AggregationBucketKeyItems) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Filter(name)
	return &Olivere7AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere6AggregationBucketKeyItems) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Filter(name)
	return &Olivere6AggregationSingleBucket{aggItems}, ok
}

func (agg *Olivere7AggregationBucketKeyItems) Buckets() []AggregationBucketKeyItem {
	items := make([]AggregationBucketKeyItem, len(agg.aggregationBucketKeyItems.Buckets))
	for index, b := range agg.aggregationBucketKeyItems.Buckets {
		items[index] = &Olivere7AggregationBucketKeyItem{b}
	}
	return items
}

func (agg *Olivere6AggregationBucketKeyItems) Buckets() []AggregationBucketKeyItem {
	items := make([]AggregationBucketKeyItem, len(agg.aggregationBucketKeyItems.Buckets))
	for index, b := range agg.aggregationBucketKeyItems.Buckets {
		items[index] = &Olivere6AggregationBucketKeyItem{b}
	}
	return items
}

func (agg *Olivere7AggregationBucketKeyItems) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Composite(name)
	return &Olivere7AggregationBucketCompositeItems{aggItems}, ok
}

func (agg *Olivere6AggregationBucketKeyItems) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := agg.aggregationBucketKeyItems.Composite(name)
	return &Olivere6AggregationBucketCompositeItems{aggItems}, ok
}

type Olivere7AggregationBucketKeyItem struct {
	aggregationBucketKeyItem *elastic7.AggregationBucketKeyItem
}

type Olivere6AggregationBucketKeyItem struct {
	aggregationBucketKeyItem *elastic6.AggregationBucketKeyItem
}

func (item *Olivere7AggregationBucketKeyItem) Key() interface{} {
	return item.aggregationBucketKeyItem.Key
}

func (item *Olivere6AggregationBucketKeyItem) Key() interface{} {
	return item.aggregationBucketKeyItem.Key
}

func (item *Olivere7AggregationBucketKeyItem) DocCount() int64 {
	return item.aggregationBucketKeyItem.DocCount
}

func (item *Olivere6AggregationBucketKeyItem) DocCount() int64 {
	return item.aggregationBucketKeyItem.DocCount
}

func (item *Olivere7AggregationBucketKeyItem) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Range(name)
	return &Olivere7AggregationBucketRangeItems{aggItems}, ok
}

func (item *Olivere6AggregationBucketKeyItem) Range(name string) (AggregationBucketRangeItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Range(name)
	return &Olivere6AggregationBucketRangeItems{aggItems}, ok
}

func (item *Olivere7AggregationBucketKeyItem) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Terms(name)
	return &Olivere7AggregationBucketKeyItems{aggItems}, ok
}

func (item *Olivere6AggregationBucketKeyItem) Terms(name string) (AggregationBucketKeyItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Terms(name)
	return &Olivere6AggregationBucketKeyItems{aggItems}, ok
}

func (item *Olivere7AggregationBucketKeyItem) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Filter(name)
	return &Olivere7AggregationSingleBucket{aggItems}, ok
}

func (item *Olivere6AggregationBucketKeyItem) Filter(name string) (AggregationSingleBucket, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Filter(name)
	return &Olivere6AggregationSingleBucket{aggItems}, ok
}

func (item *Olivere7AggregationBucketKeyItem) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Composite(name)
	return &Olivere7AggregationBucketCompositeItems{aggItems}, ok
}

func (item *Olivere6AggregationBucketKeyItem) Composite(name string) (AggregationBucketCompositeItems, bool) {
	aggItems, ok := item.aggregationBucketKeyItem.Composite(name)
	return &Olivere6AggregationBucketCompositeItems{aggItems}, ok
}
