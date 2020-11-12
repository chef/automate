package wrapper

import (
	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type FetchSourceContext interface {
	OlivereSource() interface{}
	Include(includes ...string) FetchSourceContext
}

type Olivere7FetchSourceContext struct {
	fetchSourceContext *elastic7.FetchSourceContext
}

type Olivere6FetchSourceContext struct {
	fetchSourceContext *elastic6.FetchSourceContext
}

func (fsc *Olivere7FetchSourceContext) OlivereSource() interface{} {
	return fsc.fetchSourceContext
}

func (fsc *Olivere6FetchSourceContext) OlivereSource() interface{} {
	return fsc.fetchSourceContext
}

func (fsc *Olivere7FetchSourceContext) Include(includes ...string) FetchSourceContext {
	fsc.fetchSourceContext.Include(includes...)
	return fsc
}

func (fsc *Olivere6FetchSourceContext) Include(includes ...string) FetchSourceContext {
	fsc.fetchSourceContext.Include(includes...)
	return fsc
}
