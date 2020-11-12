package wrapper

import (
	"fmt"
	"os"
	"strings"

	elastic7 "github.com/olivere/elastic/v7"
	elastic6 "gopkg.in/olivere/elastic.v6"
)

type ElasticClient interface {
	Search() SearchService
	NewBoolQuery() BoolQuery
	NewSearchSource() SearchSource
	NewTermsAggregation() TermsAggregation
	NewFetchSourceContext(bool) FetchSourceContext
}

type ElasticClient6 struct {
	Url    string `json:"url"`
	client *elastic6.Client
}

type ElasticClient7 struct {
	Url    string `json:"url"`
	client *elastic7.Client
}

func NewElasticClient(url string) ElasticClient {
	es6Client, err := elastic6.NewClient(
		elastic6.SetURL(url),
		elastic6.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch 6.X client from '%s': %s\n", url, err)
		os.Exit(1)
	}

	majorVersion, err := getElasticsearchMajorVersion(es6Client, url)
	if err != nil {
		fmt.Printf("Could not get the elasticsearch version from '%s': %s\n", url, err)
		os.Exit(1)
	}

	if majorVersion == "6" {
		return &ElasticClient6{Url: url, client: es6Client}
	}

	if majorVersion == "7" {
		es6Client.Stop()

		es7Client, err := elastic7.NewClient(
			elastic7.SetURL(url),
			elastic7.SetSniff(false),
		)
		if err != nil {
			fmt.Printf("Could not create elasticsearch 7.X client from '%s': %s\n", url, err)
			os.Exit(1)
		}
		return &ElasticClient7{Url: url, client: es7Client}
	}

	fmt.Printf("Elasticsearch major version %q from %q is not supported", majorVersion, url)
	os.Exit(1)

	return nil
}

func getElasticsearchMajorVersion(es6Client *elastic6.Client, url string) (string, error) {
	version, err := es6Client.ElasticsearchVersion(url)
	if err != nil {
		return "", fmt.Errorf("could not get the elasticsearch version from '%s': %s", url, err)
	}

	parts := strings.Split(version, ".")

	if len(parts) != 3 {
		return "", fmt.Errorf("elasticsearch version does not have three parts '%s': %s", version, url)
	}

	return parts[0], nil
}

func (eClient ElasticClient7) NewFetchSourceContext(fetchSource bool) FetchSourceContext {
	return &Olivere7FetchSourceContext{elastic7.NewFetchSourceContext(fetchSource)}
}

func (eClient ElasticClient6) NewFetchSourceContext(fetchSource bool) FetchSourceContext {
	return &Olivere6FetchSourceContext{elastic6.NewFetchSourceContext(fetchSource)}
}

// NewBoolQuery -
func (eClient ElasticClient7) NewBoolQuery() BoolQuery {
	return &Olivere7BoolQuery{elastic7.NewBoolQuery()}
}

// NewBoolQuery -
func (eClient ElasticClient6) NewBoolQuery() BoolQuery {
	return &Olivere6BoolQuery{elastic6.NewBoolQuery()}
}

func (eClient ElasticClient7) Search() SearchService {
	return &Olivere7SearchService{eClient.client.Search()}
}

func (eClient ElasticClient6) Search() SearchService {
	return &Olivere6SearchService{eClient.client.Search()}
}

func (eClient ElasticClient7) NewSearchSource() SearchSource {
	return &Olivere7SearchSource{elastic7.NewSearchSource()}
}

func (eClient ElasticClient6) NewSearchSource() SearchSource {
	return &Olivere6SearchSource{elastic6.NewSearchSource()}
}

func (eClient ElasticClient7) NewTermsAggregation() TermsAggregation {
	return &Olivere7TermsAggregation{elastic7.NewTermsAggregation()}
}

func (eClient ElasticClient6) NewTermsAggregation() TermsAggregation {
	return &Olivere6TermsAggregation{elastic6.NewTermsAggregation()}
}
