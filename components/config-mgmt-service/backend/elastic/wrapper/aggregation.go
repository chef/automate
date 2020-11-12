package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type TermsAggregation interface {
	Field(string) TermsAggregation
	Size(int) TermsAggregation
	Source() (interface{}, error)
}

type Olivere7TermsAggregation struct {
	termsAggregation *elastic7.TermsAggregation
}

type Olivere6TermsAggregation struct {
	termsAggregation *elastic6.TermsAggregation
}

func (ta *Olivere7TermsAggregation) Field(field string) TermsAggregation {
	ta.termsAggregation.Field(field)

	return ta
}

func (ta *Olivere6TermsAggregation) Field(field string) TermsAggregation {
	ta.termsAggregation.Field(field)

	return ta
}

func (ta *Olivere7TermsAggregation) Size(size int) TermsAggregation {
	ta.termsAggregation.Size(size)

	return ta
}

func (ta *Olivere6TermsAggregation) Size(size int) TermsAggregation {
	ta.termsAggregation.Size(size)

	return ta
}

func (ta *Olivere7TermsAggregation) Source() (interface{}, error) {
	return ta.termsAggregation.Source()
}

func (ta *Olivere6TermsAggregation) Source() (interface{}, error) {
	return ta.termsAggregation.Source()
}
