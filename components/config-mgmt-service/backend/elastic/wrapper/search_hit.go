package wrapper

import (
	"encoding/json"

	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type SearchHit interface {
	Source() json.RawMessage
}

type Olivere7SearchHit struct {
	searchHit *elastic7.SearchHit
}

type Olivere6SearchHit struct {
	searchHit *elastic6.SearchHit
}

func (sh *Olivere7SearchHit) Source() json.RawMessage {
	return sh.searchHit.Source
}

func (sh *Olivere6SearchHit) Source() json.RawMessage {
	return *sh.searchHit.Source
}
