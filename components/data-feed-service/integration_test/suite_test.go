package integration_test

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

type Suite struct {
	ctx                   context.Context
	server                *http.Server
	mux                   *http.ServeMux
	secretId              string
	assetDestination      []byte
	complianceDestination []byte
	assetComplete         bool
	complianceComplete    bool
}

var (
	secretData   = []byte(`{"name":"integration test secret","type":"data_feed","data":[{"key":"username","value":"user"},{"key":"password","value":"password"}]}`)
	secretFilter = []byte(`{"filters": [{"key":"name", "values":["integration test secret"]}]}"}`)
	reportId     string
	clientRunId  string
)

// Just returns a new struct. You have to call GlobalSetup() to setup the
// backend connections and such.
func NewSuite() *Suite {
	suite := new(Suite)
	mux := http.NewServeMux()
	server := http.Server{Addr: ":38080", Handler: mux}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	mux.HandleFunc("/success", func(w http.ResponseWriter, r *http.Request) {
		_, _ = w.Write([]byte("OK"))
	})
	mux.Handle("/fails", http.NotFoundHandler())
	suite.ctx = ctx
	suite.server = &server
	suite.mux = mux
	suite.assetComplete = false
	suite.complianceComplete = false

	return suite
}

// GlobalSetup makes backend connections to elastic and postgres. It also sets
// global vars to usable values.
func (suite *Suite) GlobalSetup() error {
	var err error
	// get the array of secrets, should only be 0 or 1 and get the id
	response, err := getSecretsRequest(secretFilter)
	log.Printf("%v", response)
	if err != nil {
		log.Fatalf("error getting secret id %v", err)
	} else if response.StatusCode != 200 {
		log.Fatalf("error getting secret id, status code %d, %v", response.StatusCode, err)
	} else {
		body, err := parseHttpBody(response.Body)
		secrets := body["secrets"].([]interface{})
		for _, secret := range secrets {
			secretId := secret.(map[string]interface{})["id"].(string)
			_ = deleteSecretRequest(secretId, 200)
		}
		suite.secretId, err = addSecretRequest(secretData, 200)
		if err != nil {
			log.Fatalf("secret not added %v", err)
		}
	}
	destinations, err := listDestinationsRequest(200)

	for _, d := range destinations {
		destination := d.(map[string]interface{})
		_, _ = deleteDestinationRequest(destination["id"].(string), 200)
	}

	suite.assetDestination = []byte(`{"name":"integration_asset_test", "url":"http://localhost:38080/asset", "secret":"` + suite.secretId + `"}`)
	suite.complianceDestination = []byte(`{"name":"integration_compliance_test", "url":"http://localhost:38080/compliance", "secret":"` + suite.secretId + `"}`)

	go func() {
		if err := suite.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()
	return nil
}

// GlobalTeardown is the place where you tear everything down after we have finished
// executing all our test suite, at the moment we are just deleting ES Indices
func (suite *Suite) GlobalTeardown() {
	count := 0
	for !suite.assetComplete || !suite.complianceComplete {
		if count == 30 {
			break
		}
		time.Sleep(6 * time.Second)
		count++
	}
	_ = suite.server.Shutdown(suite.ctx)

	if !suite.assetComplete || !suite.complianceComplete {
		log.Fatalf("Asset test complete: %v, and compliance test complete: %v", suite.assetComplete, suite.complianceComplete)
	}

}

func parseHttpBody(body io.ReadCloser) (map[string]interface{}, error) {
	m := make(map[string]interface{})
	err := json.NewDecoder(body).Decode(&m)
	if err != nil {
		return nil, err
	}
	return m, err
}

type AssetHandler struct {
	t *testing.T
}
type ComplianceHandler struct {
	t *testing.T
}

func (suite *Suite) registerAssetHandler(t *testing.T) {
	suite.mux.Handle("/asset", &AssetHandler{t: t})
}

func (suite *Suite) registerComplianceHandler(t *testing.T) {
	suite.mux.Handle("/compliance", &ComplianceHandler{t: t})
}

func (suite *Suite) setAssetComplete() {
	suite.assetComplete = true
}

func (suite *Suite) setComplianceComplete() {
	suite.complianceComplete = true
}

func testStringValue(t *testing.T, body map[string]interface{}, key string, expected string, errs []error) []error {
	if value, ok := body[key]; ok {
		assert.Equal(t, expected, value, "expected %s for key: %s, got %s", expected, key, value)
	} else {
		errMsg := fmt.Sprintf("key not found: %s", key)
		errs = append(errs, errors.New(errMsg))
		t.Error(errMsg)
	}
	return errs
}

func testFloatValue(t *testing.T, body map[string]interface{}, key string, expected float64, errs []error) []error {
	if value, ok := body[key]; ok {
		assert.Equal(t, expected, value, "expected %s for key: %s, got %s", expected, key, value)
	} else {
		errMsg := fmt.Sprintf("key not found: %s", key)
		errs = append(errs, errors.New(errMsg))
		t.Error(errMsg)
	}
	return errs
}

func addSecretRequest(data []byte, statusCode int) (string, error) {
	add, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/secrets", bytes.NewBuffer(data))
	if err != nil {
		return "", err
	}
	add.Header.Add("api-token", automateApiToken)
	response, err := client.Do(add)
	if err != nil {
		return "", err
	}
	if response.StatusCode != statusCode {
		return "", fmt.Errorf("expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	responseBody, err := parseHttpBody(response.Body)
	if err != nil {
		return "", fmt.Errorf("could not parse response body: %v", err)
	}
	secretId, ok := responseBody["id"].(string)
	if !ok {
		return "", errors.New("response body has no id")
	}
	return secretId, nil
}

func getSecretsRequest(data []byte) (*http.Response, error) {
	add, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/secrets/search", bytes.NewBuffer(data))
	if err != nil {
		return nil, err
	}
	add.Header.Add("api-token", automateApiToken)
	response, err := client.Do(add)
	if err != nil {
		return response, err
	}
	return response, nil
}

func listDestinationsRequest(statusCode int) ([]interface{}, error) {
	list, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destinations", nil)
	if err != nil {
		return nil, err
	}
	list.Header.Add("api-token", automateApiToken)
	response, err := client.Do(list)
	if err != nil {
		return nil, err
	}
	if response.StatusCode != statusCode {
		return nil, errors.New(fmt.Sprintf("status code %d", response.StatusCode))
	}
	body, err := parseHttpBody(response.Body)
	if err != nil {
		return nil, err
	}
	return body["destinations"].([]interface{}), nil
}

func addDestinationRequest(data []byte, statusCode int) (*http.Response, error) {
	add, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destination", bytes.NewBuffer(data))
	if err != nil {
		return nil, err
	}
	add.Header.Add("api-token", automateApiToken)
	response, err := client.Do(add)
	if err != nil {
		return response, err
	}
	if statusCode != response.StatusCode {
		return response, fmt.Errorf("expected status code %d, got: %d", statusCode, response.StatusCode)
	}

	return response, nil
}

func updateDestinationRequest(t *testing.T, destinationId string, data []byte, statusCode int) *http.Response {
	update, err := http.NewRequest("PATCH", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, bytes.NewBuffer(data))
	if err != nil {
		log.Printf("error %v", err)
	}
	update.Header.Add("api-token", automateApiToken)
	response, err := client.Do(update)
	if assert.Nil(t, err, "error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode, "expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func deleteDestinationRequest(destinationId string, statusCode int) (*http.Response, error) {
	del, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, nil)
	if err != nil {
		log.Printf("error %v", err)
	}
	del.Header.Add("api-token", automateApiToken)
	response, err := client.Do(del)
	if err != nil {
		return response, err
	}
	if response.StatusCode != statusCode {
		return response, fmt.Errorf("expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response, nil
}

func testDestinationRequest(t *testing.T, data []byte) (*http.Response, error) {
	test, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destinations/test", bytes.NewBuffer(data))
	assert.Nil(t, err, "error creating http request: %v", err)
	test.Header.Add("api-token", automateApiToken)
	return client.Do(test)
}

func deleteSecretRequest(secretId string, statusCode int) error {
	del, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/secrets/id/"+secretId, nil)
	if err != nil {
		return err
	}

	del.Header.Add("api-token", automateApiToken)
	response, err := client.Do(del)
	if err != nil {
		return err
	}
	if response.StatusCode != statusCode {
		return fmt.Errorf("expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return nil
}
