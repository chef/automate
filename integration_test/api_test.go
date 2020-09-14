package integration_test

import (
	"crypto/tls"
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
	//update destination
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
	_, err := addDestinationRequest(emptyAddData, 500)
	assert.Nil(t, err, "test add destination error failed %v", err)
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
	destinations := listDestinations(t, 200)
	// check we have 2 destinations
	if assert.Equal(t, 2, len(destinations), "expected 2 responses got: %v", len(destinations)) {
		// validate the destinations
		//destinations[0] should be the integration_test_suite destination added in global setup, don't test it
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
	destinations := listDestinations(t, 200)
	// we expect 1 which should be integration_test_suite
	assert.Equal(t, 0, len(destinations), "expected 0 responses got: %v", len(destinations))
}

func TestDeleteNonExistent(t *testing.T) {
	_, err := deleteDestinationRequest("10000", 404)
	assert.Nil(t, err, "%v", err)
}

func TestTestDestination(t *testing.T) {
	testDestinationRequestSuccess(t, testSuccessData)
}

func TestTestDestinationError(t *testing.T) {
	testDestinationRequestFail(t, testFailsData)
}

func TestDestinationWithSecret(t *testing.T) {
	secretId, err := addSecretRequest(secretData, 200)
	assert.Nil(t, err, "%v", err)
	dataWithSecret := []byte(`{"url":"http://localhost:38080/success", "secret_id": {"id":"` + secretId + `"}}`)
	testDestinationRequestSuccess(t, dataWithSecret)
	err = deleteSecretRequest(secretId, 200)
	assert.Nil(t, err, "%v", err)
}

func TestDestinationWithSecretError(t *testing.T) {
	secretId, err := addSecretRequest(secretData, 200)
	assert.Nil(t, err, "%v", err)
	dataWithSecret := []byte(`{"url":"http://localhost:38080/fails", "secret_id": {"id":"` + secretId + `"}}`)
	testDestinationRequestFail(t, dataWithSecret)
	err = deleteSecretRequest(secretId, 200)
	assert.Nil(t, err, "%v", err)
}

func validateResponseBody(t *testing.T, responseBody map[string]interface{}, values []string) (bool, string) {
	isValid := validateResponseBodyFields(t, responseBody, values)
	destinationId, _ := responseBody["id"].(string)
	isValid = assert.NotEqual(t, "", destinationId, "expected an ID value, got: %v", destinationId)
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

func addDestination(t *testing.T, data []byte, expectedValues []string) string {
	destinationId := ""
	response, err := addDestinationRequest(data, 200)
	assert.Nil(t, err, "add destination error %v", err)
	if response != nil {
		responseBody, err := parseHttpBody(response.Body)
		if err != nil {
			t.Errorf("error parsing response body %v", err)
		}
		var isValid bool
		isValid, destinationId = validateResponseBody(t, responseBody, expectedValues)
		assert.True(t, isValid, "add destination response body was not valid, got %v", responseBody)
	}
	return destinationId
}

func addDuplicateDestination(t *testing.T, data []byte) {
	_, err := addDestinationRequest(data, 400)
	assert.Nil(t, err, "add destination duplicate test failed %v", err)
}

func getDestination(t *testing.T, destinationId string, statusCode int) {
	get, err := http.NewRequest("GET", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, nil)
	assert.Nil(t, err, "error creating get request")
	get.Header.Add("api-token", automateApiToken)
	response, err := client.Do(get)
	if assert.Nil(t, err, "error sending request %v", err) {
		assert.True(t, response.StatusCode == statusCode, "expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	if statusCode != 404 {
		responseBody, err := parseHttpBody(response.Body)
		assert.Nil(t, err, "error parsing response body %v", err)
		isValid, getDestinationId := validateResponseBody(t, responseBody, addDataValues)
		assert.True(t, isValid, "get destination response body was not valid, got %v", responseBody)
		assert.Equal(t, destinationId, getDestinationId, "expected GetDestination ID to be %s, got %s", destinationId, getDestinationId)
	}
}

func updateDestination(t *testing.T, destinationId string, data []byte, expectedValues []string) {
	response := updateDestinationRequest(t, destinationId, data, 200)
	responseBody, err := parseHttpBody(response.Body)
	assert.Nil(t, err, "error parsing response body %v", err)
	isValid, updateDestinationId := validateResponseBody(t, responseBody, expectedValues)
	assert.True(t, isValid, "update destination response body was not valid, got %v", responseBody)
	assert.Equal(t, destinationId, updateDestinationId, "expected UpdateDestination ID to be %s, got %s", destinationId, updateDestinationId)
}

func deleteDestination(t *testing.T, destinationId string, expectedValues []string) {
	response, err := deleteDestinationRequest(destinationId, 200)
	assert.Nil(t, err, "%v", err)
	responseBody, err := parseHttpBody(response.Body)
	assert.Nil(t, err, "error parsing response body %v", err)
	isValid, _ := validateResponseBody(t, responseBody, expectedValues)
	assert.True(t, isValid, "delete destination response body was not valid, got %v", responseBody)
}

func testDestinationRequestSuccess(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 200, response.StatusCode, "expected 200, got %d", response.StatusCode) {
			responseBody, err := parseHttpBody(response.Body)
			assert.Nil(t, err, "error parsing response %v", err)
			assert.True(t, responseBody["success"].(bool), "expected success=true, got %v", responseBody["success"])
		}
	}
}

func testDestinationRequestFail(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 500, response.StatusCode, "expected 500, got %d", response.StatusCode) {
			responseBody, err := parseHttpBody(response.Body)
			assert.Nil(t, err, "error parsing response %v", err)
			assert.Equal(t, "404 Not Found posting test message to: http://localhost:38080/fails", responseBody["error"], "expected 404 Not Found posting test message to: http://localhost:38080/fails, got %v", responseBody["error"])
		}
	}
}

func listDestinations(t *testing.T, statusCode int) []interface{} {
	var destinations []interface{}
	list, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/datafeed/destinations", nil)
	assert.Nil(t, err, "error creating http request: %v", err)
	list.Header.Add("api-token", automateApiToken)
	response, err := client.Do(list)
	assert.Nil(t, err, "error : %v", err)
	if assert.Equal(t, statusCode, response.StatusCode, "expected %d got: %d", statusCode, response.StatusCode) {
		responseBody, err := parseHttpBody(response.Body)
		assert.Nil(t, err, "error parsing response body: %v", err)
		destinations = responseBody["destinations"].([]interface{})
	}
	return destinations
}
