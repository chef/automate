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

// Define the main structure to hold the dynamic index names
type IndexSettings map[string]interface{}

var infoReindexing = `
Reindexing of Elasticsearch/OpenSearch indices if needed.
`

func init() {
	reindexCmd.SetUsageTemplate(infoReindexing)
	RootCmd.AddCommand(reindexCmd)
}

var reindexCmd = &cobra.Command{
	Use:   "reindex",
	Short: "Reindex Elasticsearch indices if needed",
	RunE:  runReindex,
}

var isReindexing bool
var mu sync.Mutex

func runReindex(cmd *cobra.Command, args []string) error {
	fmt.Println("Reindexing Elasticsearch/OpenSearch indices.")
	mu.Lock()
	if isReindexing {
		mu.Unlock()
		fmt.Println("Reindexing is already in progress. Please wait for it to complete.")
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
		settings, err := fetchIndexSettingsVersion(index.Index)
		if err != nil {
			fmt.Printf("Error fetching settings for index %s: %v\n", index.Index, err)
			continue
		}

		if settings.Settings.Index.Version.CreatedString != settings.Settings.Index.Version.UpgradedString {
			fmt.Printf("Reindexing required for index: %s\n", index.Index)
			if err := triggerReindex(index.Index); err != nil {
				fmt.Printf("Error reindexing index %s: %v\n", index.Index, err)
			}
		} else {
			fmt.Printf("Index %s is up to date. Skipping reindex.\n", index.Index)
		}
	}

	fmt.Println("Reindexing process completed.")
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
	fmt.Printf("Initiating reindex for index %s\n", index)

	tempIndex := fmt.Sprintf("%s_temp", index)

	settings, err := fetchIndexSettings(index)
	if err != nil {
		return fmt.Errorf("failed to fetch settings for index %s: %w", index, err)
	}

	mappings, err := fetchIndexMappings(index)
	if err != nil {
		return fmt.Errorf("failed to fetch mappings for index %s: %w", index, err)
	}

	// Fetch aliases for the index
	aliases, err := fetchAliases(index)
	if err != nil {
		fmt.Printf("Warning: failed to fetch aliases for index %s: %v\n", index, err)
		aliases = []string{}
	}
	fmt.Printf("aliases of index %s: %v\n", index, aliases)

	if err := createIndex(tempIndex, settings, mappings, index); err != nil {
		return fmt.Errorf("failed to create temporary index %s: %w", tempIndex, err)
	}

	fmt.Println("Temporary index created successfully.")

	if err := reindexData(index, tempIndex); err != nil {
		return fmt.Errorf("failed to reindex data to temp index %s: %w", tempIndex, err)
	}

	fmt.Println("Data reindexed to temporary index successfully.")

	if err := setIndexWriteBlock(tempIndex, true); err != nil {
		return fmt.Errorf("failed to set write block on temporary index %s: %w", tempIndex, err)
	}

	fmt.Println("Write block set on temporary index.")

	if err := deleteIndex(index); err != nil {
		return fmt.Errorf("failed to delete original index %s: %w", index, err)
	}

	fmt.Println("Original index deleted successfully.")

	if err := cloneIndex(tempIndex, index); err != nil {
		return fmt.Errorf("failed to clone temp index %s to %s: %w", tempIndex, index, err)
	}

	fmt.Println("Temporary index cloned to original index name successfully.")

	// Reassign aliases to the cloned index
	if len(aliases) > 0 {
		if err := updateAliases(index, aliases); err != nil {
			return fmt.Errorf("failed to update aliases for index %s: %w", index, err)
		}
		fmt.Println("Aliases updated successfully.")
	}

	if err := setIndexWriteBlock(tempIndex, false); err != nil {
		return fmt.Errorf("failed to remove write block on temporary index %s: %w", tempIndex, err)
	}

	if err := deleteIndex(tempIndex); err != nil {
		fmt.Printf("Failed to delete temporary index %s: %v\n", tempIndex, err)
	} else {
		fmt.Println("Temporary index deleted successfully.")
	}

	fmt.Printf("Reindexing completed for index %s\n", index)
	return nil
}

func sanitizeSettings(settings map[string]interface{}, indexName string) (map[string]interface{}, error) {
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

	sanitizedIndexSettings := make(map[string]interface{})

	for key, value := range indexSettings {
		// Remove only settings that are not allowed during index creation
		switch key {
		case "creation_date", "creation_date_string", "uuid", "version", "provided_name", "resize", "routing", "store", "warmer", "flush", "merge", "sync", "translog", "query_string", "verified_before_close":
			// Skip these keys
			continue
		default:
			sanitizedIndexSettings[key] = value
		}
	}

	settingsMap["index"] = sanitizedIndexSettings
	indexDataMap["settings"] = settingsMap
	settings[indexName] = indexDataMap

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
	url := "http://127.0.0.1:10144/_reindex?wait_for_completion=true&refresh=true"
	payload := fmt.Sprintf(`{
        "source": { "index": "%s" },
        "dest": { "index": "%s" }
    }`, source, destination)

	req, err := http.NewRequest(http.MethodPost, url, strings.NewReader(payload))
	if err != nil {
		return fmt.Errorf("failed to create reindex request from %s to %s: %w", source, destination, err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to execute reindex request from %s to %s: %w", source, destination, err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("failed to reindex data: %s", string(body))
	}

	// Parse the response to check for failures
	var reindexResponse struct {
		Took     int           `json:"took"`
		Failures []interface{} `json:"failures"`
	}
	if err := json.Unmarshal(body, &reindexResponse); err != nil {
		return fmt.Errorf("failed to parse reindex response: %w", err)
	}

	if len(reindexResponse.Failures) > 0 {
		fmt.Printf("Reindexing completed with failures: %+v\n", reindexResponse.Failures)
	} else {
		fmt.Println("Reindexing completed successfully with no failures.")
	}

	fmt.Printf("Reindexing response: %s\n", string(body))
	return nil
}

func updateAliases(index string, aliases []string) error {
	fmt.Printf("Updating aliases for index %s\n", index)
	actions := []map[string]interface{}{}

	for _, alias := range aliases {
		action := map[string]interface{}{
			"add": map[string]interface{}{
				"index": index,
				"alias": alias,
			},
		}
		actions = append(actions, action)
	}

	payload := map[string]interface{}{
		"actions": actions,
	}

	payloadBytes, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("failed to marshal alias update payload: %w", err)
	}

	url := "http://127.0.0.1:10144/_aliases"

	req, err := http.NewRequest(http.MethodPost, url, bytes.NewReader(payloadBytes))
	if err != nil {
		return fmt.Errorf("failed to create alias update request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to update aliases: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to update aliases: %s", string(body))
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

// Close an index
func closeIndex(index string) error {
	fmt.Printf("Closing index: %s\n", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_close", index)
	req, err := http.NewRequest(http.MethodPost, url, nil)
	if err != nil {
		return fmt.Errorf("failed to create close request for index %s: %w", index, err)
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to close index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to close index %s: %s", index, string(body))
	}
	return nil
}

func cloneIndex(sourceIndex, targetIndex string) error {
	fmt.Printf("Cloning index from %s to %s\n", sourceIndex, targetIndex)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_clone/%s", sourceIndex, targetIndex)
	payload := `{ "settings": { "number_of_replicas": 1 } }` // Adjust settings if needed

	req, err := http.NewRequest(http.MethodPost, url, strings.NewReader(payload))
	if err != nil {
		return fmt.Errorf("failed to create clone request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to clone index: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusAccepted {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to clone index: %s", string(body))
	}

	return nil
}

// Open an index
func openIndex(index string) error {
	fmt.Printf("Opening index: %s\n", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_open", index)
	req, err := http.NewRequest(http.MethodPost, url, nil)
	if err != nil {
		return fmt.Errorf("failed to create open request for index %s: %w", index, err)
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to open index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to open index %s: %s", index, string(body))
	}
	return nil
}

func setIndexWriteBlock(index string, readOnly bool) error {
	fmt.Printf("Setting index.blocks.write to %v for index: %s\n", readOnly, index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_settings", index)
	payload := fmt.Sprintf(`{
        "index": {
            "blocks": {
                "write": %v
            }
        }
    }`, readOnly)

	req, err := http.NewRequest(http.MethodPut, url, strings.NewReader(payload))
	if err != nil {
		return fmt.Errorf("failed to create request to set index.blocks.write for index %s: %w", index, err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("failed to set index.blocks.write for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to set index.blocks.write for index %s: %s", index, string(body))
	}

	return nil
}

func fetchAliases(index string) ([]string, error) {
	fmt.Printf("Fetching aliases for index: %s\n", index)
	url := fmt.Sprintf("http://127.0.0.1:10144/%s/_alias", index)
	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch aliases for index %s: %w", index, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("failed to fetch aliases for index %s: %s", index, string(body))
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read aliases response for index %s: %w", index, err)
	}

	var aliasesResponse map[string]struct {
		Aliases map[string]interface{} `json:"aliases"`
	}
	if err := json.Unmarshal(body, &aliasesResponse); err != nil {
		return nil, fmt.Errorf("failed to unmarshal aliases for index %s: %w", index, err)
	}

	indexAliases, exists := aliasesResponse[index]
	if !exists {
		return nil, fmt.Errorf("index %s not found in aliases response", index)
	}

	aliasesMap := indexAliases.Aliases
	aliases := make([]string, 0, len(aliasesMap))
	for alias := range aliasesMap {
		aliases = append(aliases, alias)
	}

	return aliases, nil
}
