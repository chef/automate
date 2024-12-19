package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type OpensearchInfo struct {
	Name        string `json:"name"`
	ClusterName string `json:"cluster_name"`
	ClusterUUID string `json:"cluster_uuid"`
	Version     struct {
		Distribution                     string `json:"distribution"`
		Number                           string `json:"number"`
		BuildType                        string `json:"build_type"`
		BuildHash                        string `json:"build_hash"`
		BuildSnapshot                    bool   `json:"build_snapshot"`
		LuceneVersion                    string `json:"lucene_version"`
		MinimumWireCompatibilityVersion  string `json:"minimum_wire_compatibility_version"`
		MinimumIndexCompatibilityVersion string `json:"minimum_index_compatibility_version"`
	} `json:"version"`
	Tagline string `json:"tagline"`
}

type FailedIndex struct {
	IndexName    string `json:"index_name"`
	ErrorMessage string `json:"error_message,omitempty"`
}

type Status struct {
	Status            string        `json:"status"`
	CompletedIndex    []string      `json:"completed_index"`
	FailedIndex       []FailedIndex `json:"failed_index"`
	OpenSearchVersion string        `json:"opensearch_version"`
	LastUpdatedAt     string        `json:"last_updated_at"`
}

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

const lockFile = "/tmp/reindex.lock"

var (
	statusFile = "/hab/status.json"
	taskMutex  sync.Mutex
)

var reindexStatus = &Status{}

func init() {
	reindexCmd.SetUsageTemplate(infoReindexing)
	reindexCmd.Flags().Bool("background", false, "Run reindexing in the background")
	RootCmd.AddCommand(reindexCmd)
	RootCmd.AddCommand(statusCmd)
}

var reindexCmd = &cobra.Command{
	Use:   "reindex",
	Short: "Reindex Elasticsearch indices if needed",
	RunE:  runReindex,
}

var statusCmd = &cobra.Command{
	Use:   "index-status",
	Short: "Check the reindexing status",
	RunE:  checkStatus,
}

var skipIndices = map[string]bool{
	"security-auditlog":         true,
	".opendistro":               true,
	".plugins-ml-config":        true,
	".opensearch-observability": true,
}

func checkStatus(cmd *cobra.Command, args []string) error {
	reindexStatus, err := readStatus()
	if err != nil {
		fmt.Printf("Error reading status: %v\n", err)
		return err
	}

	fmt.Printf("Current Status:\n")
	fmt.Printf("- Status: %s\n", reindexStatus.Status)
	fmt.Printf("- OpenSearch Version: %s\n", reindexStatus.OpenSearchVersion)
	fmt.Printf("- Last Updated At: %s\n", reindexStatus.LastUpdatedAt)
	fmt.Printf("- Completed Indices:\n")
	for _, index := range reindexStatus.CompletedIndex {
		fmt.Printf("  - %s\n", index)
	}

	fmt.Printf("- Failed Indices:\n")
	for _, task := range reindexStatus.FailedIndex {
		fmt.Printf("  - Index: %s\n", task.IndexName)
		if task.ErrorMessage != "" {
			fmt.Printf(", Error: %s", task.ErrorMessage)
		}
		fmt.Println()
	}

	return nil
}

// Reads the current task status from the JSON file
func readStatus() (Status, error) {
	taskMutex.Lock()
	defer taskMutex.Unlock()

	file, err := os.ReadFile(statusFile)
	if os.IsNotExist(err) {
		return Status{}, nil
	} else if err != nil {
		return Status{}, err
	}

	var status Status
	err = json.Unmarshal(file, &status)
	if err != nil {
		return Status{}, err
	}
	return status, nil
}

func updateStatus(status *Status) error {
	status.LastUpdatedAt = time.Now().Format(time.RFC3339)

	file, err := json.MarshalIndent(status, "", "  ")
	if err != nil {
		return err
	}

	return os.WriteFile(statusFile, file, 0644)
}

func isProcessRunning() bool {
	data, err := os.ReadFile(lockFile)
	if err != nil {
		if os.IsNotExist(err) {
			return false // No lock file means no process is running
		}
		fmt.Printf("Error reading lock file: %v\n", err)
		return false
	}

	pid := strings.TrimSpace(string(data))
	pidInt, err := strconv.Atoi(pid)
	if err != nil {
		fmt.Printf("Error converting PID to integer: %v\n", err)
		return false
	}
	process, err := os.FindProcess(pidInt)
	if err != nil {
		return false // Process not found
	}

	// Check if the process is still alive
	err = process.Signal(syscall.Signal(0))
	return err == nil
}

func createLockFile() error {
	pid := fmt.Sprintf("%d\n", os.Getpid())
	return os.WriteFile(lockFile, []byte(pid), 0644)
}

func removeLockFile() {
	os.Remove(lockFile) // Ignore errors during cleanup
}

func runInBackground() {
	cmd := exec.Command(os.Args[0], append(os.Args[1:], "--background")...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true} // Detach process group
	cmd.Stdout = nil
	cmd.Stderr = nil
	cmd.Stdin = nil
	if err := cmd.Start(); err != nil {
		fmt.Println("Error starting process in background:", err)
		os.Exit(1)
	}
	fmt.Println("Task is now running in the background with PID:", cmd.Process.Pid)
	os.Exit(0)
}

func runReindex(cmd *cobra.Command, args []string) error {
	backgroundFlag, _ := cmd.Flags().GetBool("background")

	if !backgroundFlag {
		if isProcessRunning() {
			fmt.Println("Reindexing process is already running. Run `chef-automate index-status` to get the status.")
			return nil
		}

		runInBackground()
	}

	if err := createLockFile(); err != nil {
		return fmt.Errorf("failed to create lock file: %v", err)
	}
	defer removeLockFile() // Ensure cleanup after reindexing completes

	fmt.Println("Starting reindexing process...")

	osInfo, err := getOpenSearchInfo()
	if err != nil {
		fmt.Printf("Error fetching OpenSearch info: %v\n", err)
	}

	reindexStatus.OpenSearchVersion = osInfo.Version.Number
	reindexStatus.Status = "Running"

	indices, err := fetchIndices()
	if err != nil {
		fmt.Printf("Error fetching indices: %v\n", err)
		return err
	}

OuterLoop:
	for _, index := range indices {
		for prefix := range skipIndices {
			if strings.HasPrefix(index.Index, prefix) {
				fmt.Printf("Skipping index %s\n", index.Index)
				continue OuterLoop
			}
		}

		if err := triggerReindex(index.Index); err != nil {
			reindexStatus.FailedIndex = append(reindexStatus.FailedIndex, FailedIndex{
				IndexName:    index.Index,
				ErrorMessage: err.Error(),
			})
		} else {
			reindexStatus.CompletedIndex = append(reindexStatus.CompletedIndex, index.Index)
		}

		fmt.Printf("reindexStatus: %v\n", reindexStatus)
	}

	reindexStatus.Status = "Completed"
	updateStatus(reindexStatus)
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

	if err := setIndexWriteBlock(index, false); err != nil {
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

func getOpenSearchInfo() (*OpensearchInfo, error) {
	resp, err := http.Get("http://127.0.0.1:10144/")
	if err != nil {
		return nil, fmt.Errorf("failed to fetch OpenSearch info: %w", err)
	}
	defer resp.Body.Close()

	// Parse the response
	var info OpensearchInfo
	if err := json.NewDecoder(resp.Body).Decode(&info); err != nil {
		return nil, fmt.Errorf("failed to decode OpenSearch info: %w", err)
	}

	return &info, nil
}
