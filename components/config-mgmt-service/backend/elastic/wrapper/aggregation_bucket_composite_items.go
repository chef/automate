package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type AggregationBucketCompositeItems interface {
	Buckets() []AggregationBucketCompositeItem
	AfterKey() map[string]interface{}
}

type AggregationBucketCompositeItem interface {
	Key() map[string]interface{}
	DocCount() int64
}

type Olivere7AggregationBucketCompositeItems struct {
	aggregationBucketCompositeItems *elastic7.AggregationBucketCompositeItems
}

type Olivere6AggregationBucketCompositeItems struct {
	aggregationBucketCompositeItems *elastic6.AggregationBucketCompositeItems
}

func (agg *Olivere7AggregationBucketCompositeItems) Buckets() []AggregationBucketCompositeItem {
	items := make([]AggregationBucketCompositeItem, len(agg.aggregationBucketCompositeItems.Buckets))
	for index, b := range agg.aggregationBucketCompositeItems.Buckets {
		items[index] = &Olivere7AggregationBucketCompositeItem{b}
	}
	return items
}

func (agg *Olivere6AggregationBucketCompositeItems) Buckets() []AggregationBucketCompositeItem {
	items := make([]AggregationBucketCompositeItem, len(agg.aggregationBucketCompositeItems.Buckets))
	for index, b := range agg.aggregationBucketCompositeItems.Buckets {
		items[index] = &Olivere6AggregationBucketCompositeItem{b}
	}
	return items
}

func (agg *Olivere7AggregationBucketCompositeItems) AfterKey() map[string]interface{} {
	return agg.aggregationBucketCompositeItems.AfterKey
}

func (agg *Olivere6AggregationBucketCompositeItems) AfterKey() map[string]interface{} {
	return agg.aggregationBucketCompositeItems.AfterKey
}

type Olivere7AggregationBucketCompositeItem struct {
	aggregationBucketCompositeItem *elastic7.AggregationBucketCompositeItem
}

type Olivere6AggregationBucketCompositeItem struct {
	aggregationBucketCompositeItem *elastic6.AggregationBucketCompositeItem
}

func (item *Olivere7AggregationBucketCompositeItem) Key() map[string]interface{} {
	return item.aggregationBucketCompositeItem.Key
}

func (item *Olivere6AggregationBucketCompositeItem) Key() map[string]interface{} {
	return item.aggregationBucketCompositeItem.Key
}

func (item *Olivere7AggregationBucketCompositeItem) DocCount() int64 {
	return item.aggregationBucketCompositeItem.DocCount
}

func (item *Olivere6AggregationBucketCompositeItem) DocCount() int64 {
	return item.aggregationBucketCompositeItem.DocCount
}
