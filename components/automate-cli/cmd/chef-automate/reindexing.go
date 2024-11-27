package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
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

type Settings struct {
	Index Index `json:"index"`
}

type Index struct {
	Routing          Routing  `json:"routing"`
	RefreshInterval  string   `json:"refresh_interval"`
	NumberOfShards   string   `json:"number_of_shards"`
	RoutingPartition string   `json:"routing_partition_size"`
	Blocks           Blocks   `json:"blocks"`
	ProvidedName     string   `json:"provided_name"`
	Resize           Resize   `json:"resize"`
	CreationDate     string   `json:"creation_date"`
	Analysis         Analysis `json:"analysis"`
	NumberOfReplicas string   `json:"number_of_replicas"`
	UUID             string   `json:"uuid"`
	Version          Version  `json:"version"`
}

type Routing struct {
	Allocation Allocation `json:"allocation"`
}

type Allocation struct {
	InitialRecovery InitialRecovery `json:"initial_recovery"`
}

type InitialRecovery struct {
	ID interface{} `json:"_id"`
}

type Blocks struct {
	Write string `json:"write"`
}

type Resize struct {
	Source Source `json:"source"`
}

type Source struct {
	Name string `json:"name"`
	UUID string `json:"uuid"`
}

type Analysis struct {
	Normalizer map[string]Normalizer `json:"normalizer"`
	Analyzer   map[string]Analyzer   `json:"analyzer"`
	Tokenizer  map[string]Tokenizer  `json:"tokenizer"`
}

type Normalizer struct {
	Filter     []string `json:"filter"`
	Type       string   `json:"type"`
	CharFilter []string `json:"char_filter"`
}

type Analyzer struct {
	Filter    []string `json:"filter"`
	Tokenizer string   `json:"tokenizer"`
}

type Tokenizer struct {
	TokenChars []string `json:"token_chars"`
	MinGram    string   `json:"min_gram"`
	Type       string   `json:"type"`
	MaxGram    string   `json:"max_gram"`
}

type Version struct {
	Created  string `json:"created"`
	Upgraded string `json:"upgraded"`
}

// Define the main structure to hold the dynamic index names
type IndexSettings map[string]struct {
	Settings Settings `json:"settings"`
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
	fmt.Println("Reindexing Elasticsearch/OpenSearch indices.")
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
		if index.Index != "node-state-7" {
			continue
		}

		settings, err := fetchIndexSettingsVersion(index.Index)
		if err != nil {
			logrus.Errorf("Error fetching settings for index %s: %v", index.Index, err)
			continue
		}

		if settings.Settings.Index.Version.CreatedString != settings.Settings.Index.Version.UpgradedString {
			logrus.Infof("Reindexing required for index: %s", index.Index)
			if err := triggerReindex(index.Index); err != nil {
				logrus.Errorf("Error reindexing index %s: %v", index.Index, err)
			}
		} else {
			logrus.Infof("Index %s is up to date. Skipping reindex.", index.Index)
		}
	}

	logrus.Infoln("Reindexing process completed.")
	return nil
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

func fetchIndexSettings(index string) (map[string]interface{}, error) {
	fmt.Println("Fetching settings for index:", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_settings?pretty&human", index)
	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch settings for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("failed to fetch settings for index %s: received status code %d", index, resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read settings response for index %s: %w", index, err)
	}

	var rawSettings map[string]interface{}
	if err := json.Unmarshal(body, &rawSettings); err != nil {
		return nil, fmt.Errorf("failed to unmarshal settings for index %s: %w", index, err)
	}
	fmt.Printf("rawSettings: %v\n", rawSettings)

	return rawSettings, nil
}

func fetchIndexMappings(index string) (map[string]interface{}, error) {
	fmt.Println("Fetching mappings for index:", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_mapping", index)
	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch mappings for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read mappings response for index %s: %w", index, err)
	}

	var mappings map[string]interface{}
	if err := json.Unmarshal(body, &mappings); err != nil {
		return nil, fmt.Errorf("failed to unmarshal mappings for index %s: %w", index, err)
	}

	indexMappings, ok := mappings[index]
	if !ok {
		return nil, errors.New("mappings not found for index")
	}
	indexMappingsMap, ok := indexMappings.(map[string]interface{})
	if !ok {
		return nil, errors.New("invalid mappings format for index")
	}
	mappingsData, ok := indexMappingsMap["mappings"]
	if !ok {
		return nil, errors.New("mappings key not found in index mappings")
	}
	mappingsMap, ok := mappingsData.(map[string]interface{})
	if !ok {
		return nil, errors.New("invalid mappings format")
	}

	return mappingsMap, nil
}

func triggerReindex(index string) error {
	logrus.Infof("Initiating reindex for index %s\n", index)

	tempIndex := fmt.Sprintf("%s_temp", index)

	settings, err := fetchIndexSettings(index)
	if err != nil {
		return fmt.Errorf("failed to fetch settings for index %s: %w", index, err)
	}

	mappings, err := fetchIndexMappings(index)
	if err != nil {
		return fmt.Errorf("failed to fetch mappings for index %s: %w", index, err)
	}

	if err := createIndex(tempIndex, settings, mappings, index); err != nil {
		return fmt.Errorf("failed to create temporary index %s: %w", tempIndex, err)
	}

	if err := reindexData(index, tempIndex); err != nil {
		return fmt.Errorf("failed to reindex data to temp index %s: %w", tempIndex, err)
	}

	if err := deleteIndex(index); err != nil {
		return fmt.Errorf("failed to delete original index %s: %w", index, err)
	}

	if err := createIndex(index, settings, mappings, index); err != nil {
		return fmt.Errorf("failed to recreate index %s: %w", index, err)
	}

	if err := reindexData(tempIndex, index); err != nil {
		return fmt.Errorf("failed to reindex data from temp index %s back to %s: %w", tempIndex, index, err)
	}

	// if err := deleteIndex(tempIndex); err != nil {
	// 	logrus.Warnf("Failed to delete temporary index %s: %v", tempIndex, err)
	// }

	logrus.Infof("Reindexing completed for index %s\n", index)
	return nil
}

func sanitizeSettings(settings map[string]interface{}, indexName string) (map[string]interface{}, error) {
	fmt.Printf("Pre sanitized settings: %+v\n", settings)

	indexData, ok := settings[indexName]
	if !ok {
		return nil, fmt.Errorf("settings for index %s not found", indexName)
	}

	indexDataMap, ok := indexData.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("invalid format for settings of index %s", indexName)
	}

	settingsMap, ok := indexDataMap["settings"].(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("settings key not found in index data for %s", indexName)
	}

	indexSettings, ok := settingsMap["index"].(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("index key not found in settings for index %s", indexName)
	}

	// Remove unwanted keys
	for _, key := range []string{"creation_date", "creation_date_string", "uuid", "version", "provided_name"} {
		delete(indexSettings, key)
	}

	// Update settingsMap with sanitized index settings
	settingsMap["index"] = indexSettings

	fmt.Printf("Post sanitized settings: %+v\n", settingsMap)
	return settingsMap, nil
}

func createIndex(index string, originalSettings map[string]interface{}, mappings map[string]interface{}, originalIndexName string) error {
	fmt.Printf("Creating index: %s\n", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s", index)

	sanitizedSettings, err := sanitizeSettings(originalSettings, originalIndexName)
	if err != nil {
		return fmt.Errorf("failed to sanitize settings for original index %s: %w", originalIndexName, err)
	}

	payload := map[string]interface{}{
		"settings": sanitizedSettings,
		"mappings": mappings,
	}

	payloadBytes, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("failed to marshal payload for index %s: %w", index, err)
	}

	req, err := http.NewRequest(http.MethodPut, url, bytes.NewReader(payloadBytes))
	if err != nil {
		return fmt.Errorf("failed to create PUT request for index %s: %w", index, err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to execute PUT request for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to create index %s: %s", index, string(body))
	}

	fmt.Printf("Index %s created successfully.\n", index)
	return nil
}

func reindexData(source, destination string) error {
	fmt.Println("Reindexing data from:", source, "to:", destination)
	url := "http://127.0.0.1:10144/_reindex"
	payload := fmt.Sprintf(`{
        "source": { "index": "%s" },
        "dest": { "index": "%s" }
    }`, source, destination)

	resp, err := http.Post(url, "application/json", strings.NewReader(payload))
	if err != nil {
		return fmt.Errorf("failed to reindex data from %s to %s: %w", source, destination, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to reindex data: %s", string(body))
	}
	return nil
}

func deleteIndex(index string) error {
	fmt.Println("Deleting index:", index)

	url := fmt.Sprintf("http://127.0.0.1:10144/%s", index)
	req, err := http.NewRequest(http.MethodDelete, url, nil)
	if err != nil {
		return fmt.Errorf("failed to create delete request for index %s: %w", index, err)
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to delete index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to delete index %s: %s", index, string(body))
	}
	return nil
}

func cloneIndex(source, destination string) error {
	fmt.Println("Cloning index from:", source, "to:", destination)
	if err := reindexData(source, destination); err != nil {
		return fmt.Errorf("failed to clone data from %s to %s: %w", source, destination, err)
	}
	return nil
}
