package integration_test

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"io"
	"net/http"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

var (
	automateApiToken = os.Getenv("AUTOMATE_API_TOKEN")
	addData          = []byte(`{"name":"test", "url":"https://test.com", "secret":"secret"}`)
	addDataValues    = []string{"test", "https://test.com", "secret"}
	emptyAddData     = []byte(`{}`)
	updateData       = []byte(`{"name":"test update", "url":"https://update.test.com", "secret":"updated secret"}`)
	updateDataValues = []string{"test update", "https://update.test.com", "updated secret"}
	testSuccessData  = []byte(`{"url":"http://localhost:38080/success", "username_password": {"username":"user", "password":"password"}}`)
	testFailsData    = []byte(`{"url":"http://localhost:38080/fails", "username_password": {"username":"user", "password":"password"}}`)
	secretData       = []byte(`{"name":"integration test secret","type":"data_feed","data":[{"key":"username","value":"user"},{"key":"password","value":"password"}]}`)
	client           = NewClient()
)

func NewClient() *http.Client {
	transport := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	return &http.Client{Transport: transport}
}

func TestDataFeedAPI(t *testing.T) {
	// Add destination
	destinationId := addDestination(t, addData, addDataValues)
	// Get destination
	getDestination(t, destinationId, 200)
	//update destiantion
	updateDestination(t, destinationId, updateData, updateDataValues)
	//Delete destination
	deleteDestination(t, destinationId, updateDataValues)
	// Get should now be 404
	getDestination(t, destinationId, 404)
}

func TestAddDuplicate(t *testing.T) {
	destinationId := addDestination(t, addData, addDataValues)
	addDuplicateDestination(t, addData)
	//Delete destination
	deleteDestination(t, destinationId, addDataValues)
	// Get should now be 404
	getDestination(t, destinationId, 404)
}

func TestAddError(t *testing.T) {
	addDestinationRequest(t, emptyAddData, 500)
}

func TestUpdateUniqueViolation(t *testing.T) {
	destinationId1 := addDestination(t, addData, addDataValues)
	destinationId2 := addDestination(t, updateData, updateDataValues)
	// update should return 400 Bad Request destinationId1 and destinationId2 cannot have the same name
	updateDestinationRequest(t, destinationId1, updateData, 400)
	// clean up the test data
	deleteDestination(t, destinationId1, addDataValues)
	deleteDestination(t, destinationId2, updateDataValues)
}

func TestListDestinations(t *testing.T) {
	// Add 2 destinations
	destinationId1 := addDestination(t, addData, addDataValues)
	destinationId2 := addDestination(t, updateData, updateDataValues)
	destinations := listDestinationRequest(t, 200)
	// check we have 2 destinations
	if assert.Equal(t, 2, len(destinations), "Expected 2 responses got: %v", len(destinations)) {
		// validate the destinations
		dest1 := destinations[0].(map[string]interface{})
		dest2 := destinations[1].(map[string]interface{})
		isValid, err := validateResponseBody(t, dest1, addDataValues)
		assert.NotNil(t, err)
		assert.True(t, isValid, "destination 1 is not valid")
		isValid, err = validateResponseBody(t, dest2, updateDataValues)
		assert.NotNil(t, err)
		assert.True(t, isValid, "destination 2 is not valid")
	}
	// test data clean up
	deleteDestination(t, destinationId1, addDataValues)
	deleteDestination(t, destinationId2, updateDataValues)
}

func TestListNoDestinations(t *testing.T) {
	destinations := listDestinationRequest(t, 200)
	assert.Equal(t, 0, len(destinations), "Expected 0 responses got: %v", len(destinations))
}

func TestDeleteNonExistent(t *testing.T) {
	deleteDestinationRequest(t, "10000", 404)
}

func TestTestDestination(t *testing.T) {
	testDestinationRequestSuccess(t, testSuccessData)
}

func TestTestDestinationError(t *testing.T) {
	testDestinationRequestFail(t, testFailsData)
}

func TestDestinationWithSecret(t *testing.T) {
	secretId := addSecretRequest(t, secretData, 200)
	dataWithSecret := []byte(`{"url":"http://localhost:38080/success", "secret_id": {"id":"` + secretId + `"}}`)
	testDestinationRequestSuccess(t, dataWithSecret)
	deleteSecretRequest(t, secretId, 200)
}

func TestDestinationWithSecretError(t *testing.T) {
	secretId := addSecretRequest(t, secretData, 200)
	dataWithSecret := []byte(`{"url":"http://localhost:38080/fails", "secret_id": {"id":"` + secretId + `"}}`)
	testDestinationRequestFail(t, dataWithSecret)
	deleteSecretRequest(t, secretId, 200)
}

func validateResponseBody(t *testing.T, responseBody map[string]interface{}, values []string) (bool, string) {
	isValid := validateResponseBodyFields(t, responseBody, values)
	destinationId, _ := responseBody["id"].(string)
	isValid = assert.NotEqual(t, "", destinationId, "Expected an ID value, got: %v", destinationId)
	return isValid, destinationId
}

func validateResponseBodyFields(t *testing.T, responseBody map[string]interface{}, values []string) bool {
	isValid := true
	if responseBody["name"] != values[0] {
		t.Errorf("Expected name to be %v, got: %v", values[0], responseBody["name"])
		isValid = false
	}
	if responseBody["url"] != values[1] {
		t.Errorf("Expected url to be %v, got: %v", values[1], responseBody["url"])
		isValid = false
	}
	if responseBody["secret"] != values[2] {
		t.Errorf("Expected secret to be %v, got: %v", values[2], responseBody["secret"])
		isValid = false
	}
	return isValid
}

func parseResponse(body io.ReadCloser) (map[string]interface{}, error) {
	m := make(map[string]interface{})
	err := json.NewDecoder(body).Decode(&m)
	if err != nil {
		return nil, err
	}
	return m, err
}

func addDestination(t *testing.T, data []byte, expectedValues []string) string {
	destinationId := ""
	response := addDestinationRequest(t, data, 200)
	if response != nil {
		responseBody, err := parseResponse(response.Body)
		if err != nil {
			t.Errorf("Error parsing response body %v", err)
		}
		var isValid bool
		isValid, destinationId = validateResponseBody(t, responseBody, expectedValues)
		assert.True(t, isValid, "Add destination response body was not valid, got %v", responseBody)
	}
	return destinationId
}

func addDestinationRequest(t *testing.T, data []byte, statusCode int) *http.Response {
	add, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destination", bytes.NewBuffer(data))
	assert.Nil(t, err, "Error creating http request: %v", err)
	add.Header.Add("api-token", automateApiToken)
	response, err := client.Do(add)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.Equal(t, statusCode, response.StatusCode, "Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func addDuplicateDestination(t *testing.T, data []byte) {
	addDestinationRequest(t, data, 400)
}

func getDestination(t *testing.T, destinationId string, statusCode int) {
	get, err := http.NewRequest("GET", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, nil)
	get.Header.Add("api-token", automateApiToken)
	response, err := client.Do(get)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode, "Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	if statusCode != 404 {
		responseBody, err := parseResponse(response.Body)
		assert.Nil(t, err, "Error parsing response body %v", err)
		isValid, getDestinationId := validateResponseBody(t, responseBody, addDataValues)
		assert.True(t, isValid, "Get destination response body was not valid, got %v", responseBody)
		assert.Equal(t, destinationId, getDestinationId, "Expected GetDestination ID to be %s, got %s", destinationId, getDestinationId)
	}
}

func updateDestinationRequest(t *testing.T, destinationId string, data []byte, statusCode int) *http.Response {
	update, err := http.NewRequest("PATCH", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, bytes.NewBuffer(data))
	update.Header.Add("api-token", automateApiToken)
	response, err := client.Do(update)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode, "Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func updateDestination(t *testing.T, destinationId string, data []byte, expectedValues []string) {
	response := updateDestinationRequest(t, destinationId, data, 200)
	responseBody, err := parseResponse(response.Body)
	assert.Nil(t, err, "Error parsing response body %v", err)
	isValid, updateDestinationId := validateResponseBody(t, responseBody, expectedValues)
	assert.True(t, isValid, "Update destination response body was not valid, got %v", responseBody)
	assert.Equal(t, destinationId, updateDestinationId, "Expected UpdateDestination ID to be %s, got %s", destinationId, updateDestinationId)
}

func deleteDestinationRequest(t *testing.T, destinationId string, statusCode int) *http.Response {
	delete, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, nil)
	delete.Header.Add("api-token", automateApiToken)
	response, err := client.Do(delete)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode, "Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func deleteDestination(t *testing.T, destinationId string, expectedValues []string) {
	response := deleteDestinationRequest(t, destinationId, 200)
	responseBody, err := parseResponse(response.Body)
	assert.Nil(t, err, "Error parsing response body %v", err)
	isValid, _ := validateResponseBody(t, responseBody, expectedValues)
	assert.True(t, isValid, "delete destination response body was not valid, got %v", responseBody)
}

func testDestinationRequest(t *testing.T, data []byte) (*http.Response, error) {
	test, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destinations/test", bytes.NewBuffer(data))
	assert.Nil(t, err, "Error creating http request: %v", err)
	test.Header.Add("api-token", automateApiToken)
	return client.Do(test)
}

func testDestinationRequestSuccess(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 200, response.StatusCode, "Expected 200, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.True(t, responseBody["success"].(bool), "Expected success=true, got %v", responseBody["success"])
		}
	}
}

func testDestinationRequestFail(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 500, response.StatusCode, "Expected 500, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.Equal(t, "404 Not Found posting test message to: http://localhost:38080/fails", responseBody["error"], "Expected 404 Not Found posting test message to: http://localhost:38080/fails, got %v", responseBody["error"])
		}
	}
}

func addSecretRequest(t *testing.T, data []byte, statusCode int) string {
	add, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/secrets", bytes.NewBuffer(data))
	assert.Nil(t, err, "Error creating http request: %v", err)
	add.Header.Add("api-token", automateApiToken)
	response, err := client.Do(add)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode)
	}
	responseBody, err := parseResponse(response.Body)
	assert.Nil(t, err, "Could not parse response body: %v", err)
	secretId, ok := responseBody["id"].(string)
	if !ok {
		t.Errorf("Response body has no id")
	}
	return secretId
}

func deleteSecretRequest(t *testing.T, secretId string, statusCode int) {
	delete, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/secrets/id/"+secretId, nil)
	assert.Nil(t, err, "Error creating http request: %v", err)
	delete.Header.Add("api-token", automateApiToken)
	response, err := client.Do(delete)
	if assert.Nil(t, err, "Error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode)
	}
}

func listDestinationRequest(t *testing.T, statusCode int) []interface{} {
	var destinations []interface{}
	list, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destinations", nil)
	assert.Nil(t, err, "Error creating http request: %v", err)
	list.Header.Add("api-token", automateApiToken)
	response, err := client.Do(list)
	assert.Nil(t, err, "Error : %v", err)
	if assert.Equal(t, statusCode, response.StatusCode, "Expected %d got: %d", statusCode, response.StatusCode) {
		responseBody, err := parseResponse(response.Body)
		assert.Nil(t, err, "Error parsing response body: %v", err)
		destinations = responseBody["destinations"].([]interface{})
	}
	return destinations
}
