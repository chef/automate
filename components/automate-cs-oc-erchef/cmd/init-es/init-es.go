// init-es creates an elasticsearch index from the given configuration
// file. Since it is intended to run unconditionally on application
// startup, it does not fail if the index already exists.
package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
)

var defaultTimeout = 15 * time.Second

func usage() {
	fmt.Fprint(os.Stderr, "usage: es-init ES_HOST INDEX_NAME INDEX_CONFIG_FILE")
}

func main() {
	if len(os.Args) < 4 {
		usage()
		os.Exit(1)
	}

	host := os.Args[1]
	indexName := os.Args[2]
	indexFile := os.Args[3]

	logrus.Infof("Initializing ElasticSearch index %q with configuration from %q", indexName, indexFile)

	indexData, err := ioutil.ReadFile(indexFile)
	if err != nil {
		logrus.Fatalf("failed to read index configuration from file %q: %s", indexFile, err.Error())
	}

	client, err := elastic.NewClient(
		elastic.SetSniff(false),
		elastic.SetURL(fmt.Sprintf("http://%s", host)))
	if err != nil {
		logrus.Fatalf("could not create elasticsearch client: %s", err.Error())
	}

	ctx, cancel := context.WithTimeout(context.Background(), defaultTimeout)
	defer cancel()

	_, err = client.CreateIndex(indexName).
		BodyString(string(indexData)).
		Do(ctx)

	if err != nil {
		if isAlreadyExists(err) {
			logrus.Infof("Index %q already exists", indexName)
			os.Exit(0)
		} else {
			logrus.Fatalf("Index Creation Failed: %s", err.Error())
		}
	}

	logrus.Infof("Index %q created successfully", indexName)
	os.Exit(0)
}

func isAlreadyExists(err error) bool {
	if eErr, ok := err.(*elastic.Error); ok {
		if eErr.Status != 400 {
			return false
		}

		if eErr.Details != nil && eErr.Details.Type == "resource_already_exists_exception" {
			return true
		}
	}
	return false
}
