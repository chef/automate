// Copyright © 2017 Chef Software

package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"sync"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

type Indices []struct {
	Health       string `json:"health"`
	Status       string `json:"status"`
	Index        string `json:"index"`
	UUID         string `json:"uuid"`
	Pri          string `json:"pri"`
	Rep          string `json:"rep"`
	DocsCount    string `json:"docs.count"`
	DocsDeleted  string `json:"docs.deleted"`
	StoreSize    string `json:"store.size"`
	PriStoreSize string `json:"pri.store.size"`
}

type IndexSettings struct {
	Settings struct {
		Index struct {
			Version struct {
				CreatedString  string `json:"created_string"`
				UpgradedString string `json:"upgraded_string"`
			} `json:"version"`
		} `json:"index"`
	} `json:"settings"`
}

var infoReindexing = `
Reindexing! Of Elasticsearch/OpenSearch indices if needed. 
`

func init() {
	infoCmd.SetUsageTemplate(infoReindexing)
	RootCmd.AddCommand(reindexCmd)
}

var reindexCmd = &cobra.Command{
	Use:               "reindex",
	Short:             "Reindex Elasticsearch indices if needed",
	PersistentPreRunE: checkLicenseStatusForExpiry,
	RunE:              runReindex,
}

var isReindexing bool
var mu sync.Mutex

func runReindex(cmd *cobra.Command, args []string) error {
	fmt.Println("Reindexing! Of Elasticsearch/OpenSearch indice test.")
	mu.Lock()
	if isReindexing {
		mu.Unlock()
		logrus.Infoln("Reindexing is already in progress. Please wait for it to complete.")
		return nil
	}
	isReindexing = true
	mu.Unlock()

	defer func() {
		mu.Lock()
		isReindexing = false
		mu.Unlock()
	}()

	indices, err := fetchIndices()
	if err != nil {
		return err
	}

	for _, index := range indices {
		settings, err := fetchIndexSettings(index.Index)
		if err != nil {
			logrus.Errorf("Error fetching settings for index %s: %v\n", index, err)
			continue
		}

		if settings.Settings.Index.Version.CreatedString != settings.Settings.Index.Version.UpgradedString {
			logrus.Errorf("Reindexing required for index: %s\n", index)
			if err := triggerReindex(index.Index); err != nil {
				logrus.Errorf("Error reindexing index %s: %v\n", index, err)
			}
		} else {
			logrus.Infof("Index %s is up to date. Skipping reindex.\n", index)
		}
	}

	logrus.Infoln("Reindexing process completed.")
	return nil
}

func fetchIndices() (Indices, error) {
	resp, err := http.Get("http://127.0.0.1:10144/_cat/indices?format=json")
	if err != nil {
		return nil, fmt.Errorf("failed to fetch indices: %w", err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read indices response: %w", err)
	}

	var indices Indices
	if err = json.Unmarshal(body, &indices); err != nil {
		return nil, fmt.Errorf("failed to unmarshal indices response: %w", err)
	}
	return indices, nil
}

func fetchIndexSettings(index string) (*IndexSettings, error) {
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_settings?pretty&human", index)
	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch settings for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read settings response for index %s: %w", index, err)
	}

	var settings map[string]IndexSettings
	if err := json.Unmarshal(body, &settings); err != nil {
		return nil, fmt.Errorf("failed to unmarshal settings for index %s: %w", index, err)
	}

	setting, exists := settings[index]
	if !exists {
		return nil, errors.New("index settings not found in response")
	}
	return &setting, nil
}

func triggerReindex(index string) error {
	logrus.Infof("Initiating reindex for index %s\n", index)
	// TODO: CAll the reindex API to reindex the index

	logrus.Infof("Reindexing completed for index %s\n", index)
	return nil
}
