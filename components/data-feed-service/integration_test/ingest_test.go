package integration_test

import (
	"bytes"
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"testing"
	"time"

	"github.com/chef/automate/api/external/ingest/request"

	"github.com/stretchr/testify/assert"
)

func TestIngest(t *testing.T) {
	data, err := buildClientRun(t, "../../ingest-service/examples/chef_client_run.json")
	if assert.Nil(t, err) {
		suite.registerAssetHandler(t)
		addIngestRequest(t, data, 200)
		t.Log("waiting 2 minutes for datafeed loop to execute")
		time.Sleep(2 * time.Minute)
	}
}

func addIngestRequest(t *testing.T, data []byte, statusCode int) *http.Response {
	ingest, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/ingest/events/chef/run", bytes.NewBuffer(data))
	assert.Nil(t, err, "Error creating http request: %v", err)
	ingest.Header.Add("api-token", automateApiToken)
	response, err := client.Do(ingest)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.Equal(t, statusCode, response.StatusCode, "Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func (handler *AssetHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	node := make(map[string]interface{})
	handler.t.Log("Data feed received")
	hasErrors := false

	reader, err := gzip.NewReader(r.Body)
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		return
	}
	buf, err := ioutil.ReadAll(reader)
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		return
	}

	err = json.Unmarshal(buf, &node)
	assert.Nil(handler.t, err, "%v", err)
	clientRun := node["client_run"].(map[string]interface{})

	result, message := testStringValue(clientRun, "node_name", "example.com")
	hasErrors = assert.True(handler.t, result, message) && hasErrors

	if !assert.False(handler.t, hasErrors, "Errors parsing Datafeed JSON") {
		http.Error(w, "Errors parsing Datafeed JSON", http.StatusBadRequest)
		return
	}
}

func buildClientRun(t *testing.T, filePath string) ([]byte, error) {
	var data []byte
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return data, err
	}
	var clientRun request.Run
	err = json.Unmarshal(data, &clientRun)
	if err != nil {
		return data, err
	}
	layout := "2006-01-02T15:04:05Z"
	clientRun.StartTime = time.Now().Format(layout)
	clientRun.EndTime = clientRun.StartTime
	data, err = json.Marshal(clientRun)
	if err != nil {
		return data, err
	}
	return data, err
}
