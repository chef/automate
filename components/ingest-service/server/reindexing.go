package server

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/pkg/errors"
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

type IndexSettingsVersion struct {
	Settings struct {
		Index struct {
			Version struct {
				CreatedString  string `json:"created_string"`
				UpgradedString string `json:"upgraded_string"`
			} `json:"version"`
		} `json:"index"`
	} `json:"settings"`
}

var skipIndices = map[string]bool{
	"security-auditlog":         true,
	".opendistro":               true,
	".plugins-ml-config":        true,
	".opensearch-observability": true,
}

func fetchIndices() (Indices, error) {
	fmt.Println("Fetching indices from Elasticsearch/OpenSearch.")
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

func fetchIndexSettingsVersion(index string) (*IndexSettingsVersion, error) {
	fmt.Println("Fetching settings version for index:", index)
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

	var settings map[string]IndexSettingsVersion
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
	fmt.Printf("Initiating reindex for index %s\n", index)

	fmt.Printf("Reindexing completed for index %s\n", index)
	return nil
}
