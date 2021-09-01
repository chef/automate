package integration_test

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	s3 "github.com/aws/aws-sdk-go/service/s3"
	dataFeedService "github.com/chef/automate/components/data-feed-service/service"
	"github.com/stretchr/testify/assert"
)

var (
	automateApiToken             = os.Getenv("AUTOMATE_API_TOKEN")
	automateAwsRegion            = "us-west-2"
	automateAwsAccessKey         = os.Getenv("AWS_ACCESS_KEY_ID")
	automateAwsSecretAccessKey   = os.Getenv("AWS_SECRET_ACCESS_KEY")
	automateAwsBucket            = "a2-datafeed-integration-test"
	addData                      = []byte(`{"name":"test", "url":"https://test.com", "secret":"secret", "services":"ServiceNow", "integration_types": "Webhook"}`)
	addDataValues                = []string{"test", "https://test.com", "secret", "custom", "webhook"}
	emptyAddData                 = []byte(`{}`)
	updateDataValues             = []string{"test update", "https://update.test.com", "updated secret"}
	testSuccessData              = []byte(`{"url":"http://localhost:38080/success", "username_password": {"username":"user", "password":"password"}}`)
	testFailsData                = []byte(`{"url":"http://localhost:38080/fails", "username_password": {"username":"user", "password":"password"}}`)
	testSuccessHeaderData        = []byte(`{"url":"http://localhost:38080/success", "header": {"value":"{\"Authorization\":\"Splunk 6f01b869-c181-4fb2-a74c-b619e6197a85\"}"}}`)
	testFailsHeaderData          = []byte(`{"url":"http://localhost:38080/fails", "header": {"value":"{\"Authorization\":\"Splunk 6f01b869-c181-4fb2-a74c-b619e6197a85\"}"}}`)
	testSuccessAwsData           = []byte(`{"url":"null", "aws": {"access_key":"` + automateAwsAccessKey + `","secret_access_key":"` + automateAwsSecretAccessKey + `","bucket":"` + automateAwsBucket + `","region":"` + automateAwsRegion + `"}}`)
	testFailsAwsData             = []byte(`{"url":"http://localhost:38080/fails", "aws": {"access_key":"` + automateAwsAccessKey + `","secret_access_key":"` + automateAwsSecretAccessKey + `","bucket":"automateAwsBucket","region":"` + automateAwsRegion + `"}}`)
	testFailsMinioData           = []byte(`{"url":"http://127.0.0.1:9000", "aws": {"access_key":"minioadmin","secret_access_key":"minioadmin","bucket":"newtest","region":"` + automateAwsRegion + `"}}`)
	automateMinioUrl             = "http://localhost:9000"
	automateMinioAccessKey       = "minioadmin"
	automateMinioSecretAccessKey = "minioadmin"
	automateMinioBucket          = "minio-storage-test-connection"
	secretData                   = []byte(`{"name":"integration test secret","type":"data_feed","data":[{"key":"username","value":"user"},{"key":"password","value":"password"}]}`)
	updateData                   = []byte(`{"name":"test update",
										  	"url":"https://update.test.com", 
										  	"secret":"updated secret",
										  	"services": "Minio",
										  	"integration_types":"Storage",
												"enable": true,
												"meta_data":[
													{
														"key":"bucket",
														"value":"s3.to.elastic.search"
													}
												]
											}}`)
	secretDataMinio = []byte(`{
		"name": "s3val",
		"type": "data_feed",
		"data":[
			 {
				  "key":"access_key",
				  "value":"` + automateMinioAccessKey + `"
			 },
			 {
				  "key":"secret_access_key",
				  "value":"` + automateMinioSecretAccessKey + `"
			 }
		 ]
	
	}`)

	client = NewClient()
)

func NewClient() *http.Client {
	transport := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	return &http.Client{Transport: transport}
}
func CreateMinioBucket(t *testing.T) ([]byte, error) {
	cred := dataFeedService.NewS3Credentials(automateMinioAccessKey, automateMinioSecretAccessKey, "us-east-2", "null")
	sess := dataFeedService.ConnectAWS(cred, automateMinioUrl, dataFeedService.Minio)
	svc := s3.New(sess)
	input := &s3.CreateBucketInput{
		Bucket: aws.String(automateMinioBucket),
	}
	var testSuccessMinioData []byte
	_, err := svc.CreateBucket(input)
	if err != nil {
		if awserr, ok := err.(awserr.Error); ok {
			switch awserr.Code() {
			case s3.ErrCodeBucketAlreadyExists:
				t.Log(awserr.Error())
			case s3.ErrCodeBucketAlreadyOwnedByYou:
				t.Log(awserr.Error())
			default:
				return nil, awserr
			}
		} else {
			// Print the error, cast err to awserr.Error to get the Code and
			// Message from an error.
			return nil, err
		}
	}
	testSuccessMinioData = []byte(`{"url":"` + automateMinioUrl + `", "aws": {"access_key":"` + automateMinioAccessKey + `","secret_access_key":"` + automateMinioSecretAccessKey + `","bucket":"` + automateMinioBucket + `","region":"us-east-2"}}`)

	return testSuccessMinioData, nil
}
func TestDataFeedAPI(t *testing.T) {
	t.Logf("API TOKEN: %d", len(automateApiToken))
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
	_, err := addDestinationRequest(emptyAddData, 500)
	assert.Nil(t, err, "test add destination error failed %v", err)
}

func TestUpdateUniqueViolation(t *testing.T) {
	destinationId1 := addDestination(t, addData, addDataValues)
	destinationId2 := addDestination(t, updateData, updateDataValues)
	enableData := []byte(`{ "id":` + destinationId1 + `,"enable":false}`)

	// update should return 400 Bad Request destinationId1 and destinationId2 cannot have the same name
	updateDestinationRequest(t, destinationId1, updateData, 400)
	enableDestinationRequest(t, destinationId1, enableData, 400)
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
	if assert.Equal(t, 3, len(destinations), "Expected 3 responses got: %v", len(destinations)) {
		// validate the destinations
		//destinations[0] should be the integration_test_suite destination added in global setup, don't test it
		dest1 := destinations[1].(map[string]interface{})
		dest2 := destinations[2].(map[string]interface{})
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
	// we expect 1 whosh shoule be integration_test_suite
	assert.Equal(t, 1, len(destinations), "Expected 1 responses got: %v", len(destinations))
}

func TestDeleteNonExistent(t *testing.T) {
	_, err := deleteDestinationRequest("10000", 404)
	assert.Nil(t, err, "%v", err)
}

func TestTestDestination(t *testing.T) {
	testDestinationRequestSuccess(t, testSuccessData)
	testDestinationHeaderRequestSuccess(t, testSuccessHeaderData)
	// testDestinationAwsRequestSuccess(t, testSuccessAwsData)
	testSuccessMinioData, error := CreateMinioBucket(t)
	if error != nil {
		t.Log(error, "got while creating minio bucket")
		t.Fail()
	}
	testDestinationMinioRequestSuccess(t, testSuccessMinioData)

}

func TestTestDestinationError(t *testing.T) {
	testDestinationRequestFail(t, testFailsData)
	testDestinationHeaderRequestFail(t, testFailsHeaderData)
	// testDestinationAwsRequestFail(t, testFailsAwsData)
	testDestinationMinioRequestFail(t, testFailsMinioData)
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
func TestDestinationWithSecretAddon(t *testing.T) {
	secretId, err := addSecretRequest(secretDataMinio, 200)
	assert.Nil(t, err, "%v", err)
	dataWithSecret := []byte(`{
		"url":"http://127.0.0.1:9000", 
		"secret_id_with_addon": {
			"id":"` + secretId + `",
		"services":"Minio",
		"integration_types":"Storage",
		"enable":true,
		"meta_data":[
			{
				 "key":"bucket",
				  "value":"` + automateMinioBucket + `"
			}
	   ]
		}}`)
	testDestinationRequestSuccess(t, dataWithSecret)
	err = deleteSecretRequest(secretId, 200)
	assert.Nil(t, err, "%v", err)
}

func TestDestinationWithSecretAddonFail(t *testing.T) {
	secretId, err := addSecretRequest(secretDataMinio, 200)
	assert.Nil(t, err, "%v", err)
	dataWithSecret := []byte(`{
		"url":"http://127.0.0.1:9000",
		"secret_id_with_addon": {
			"id":"` + secretId + `",
		"services":"Minio",
		"integration_types":"Storage",
		"enable":true,
		"meta_data":[
			{
				 "key":"bucket",
				  "value":"automateAwsBucket"
			}
	   ]
		}}`)
	testDestinationWithAwsWithSecretAddonFail(t, dataWithSecret)
	err = deleteSecretRequest(secretId, 200)
	assert.Nil(t, err, "%v", err)
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
	response, err := addDestinationRequest(data, 200)
	assert.Nil(t, err, "add destination error %v", err)
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
		return response, fmt.Errorf("Expected status code %d, got: %d", statusCode, response.StatusCode)
	}

	return response, nil
}

func addDuplicateDestination(t *testing.T, data []byte) {
	_, err := addDestinationRequest(data, 400)
	assert.Nil(t, err, "add destination duplicate test failed %v", err)
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

func enableDestinationRequest(t *testing.T, destinationId string, data []byte, statusCode int) *http.Response {
	update, err := http.NewRequest("PATCH", "https://127.0.0.1/api/v0/datafeed/destination/enable/"+destinationId, bytes.NewBuffer(data))
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
func enableDestinationSuccess(t *testing.T, destinationId string, data []byte, expectedValues []string) {
	response := enableDestinationRequest(t, destinationId, data, 200)
	responseBody, err := parseResponse(response.Body)
	assert.Nil(t, err, "Error parsing response body %v", err)
	isValid, updateDestinationId := validateResponseBody(t, responseBody, expectedValues)
	assert.True(t, isValid, "Update destination response body was not valid, got %v", responseBody)
	assert.Equal(t, destinationId, updateDestinationId, "Expected UpdateDestination ID to be %s, got %s", destinationId, updateDestinationId)
}

func deleteDestinationRequest(destinationId string, statusCode int) (*http.Response, error) {
	delete, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/datafeed/destination/"+destinationId, nil)
	delete.Header.Add("api-token", automateApiToken)
	response, err := client.Do(delete)
	if err != nil {
		return response, err
	}
	if response.StatusCode != statusCode {
		return response, fmt.Errorf("Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response, nil
}

func deleteDestination(t *testing.T, destinationId string, expectedValues []string) {
	response, err := deleteDestinationRequest(destinationId, 200)
	assert.Nil(t, err, "%v", err)
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

func testDestinationHeaderRequestSuccess(t *testing.T, data []byte) {
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

func testDestinationAwsRequestSuccess(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	if response.StatusCode != 200 {
		fmt.Println(response)
	}
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 200, response.StatusCode, "Expected 200, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.True(t, responseBody["success"].(bool), "Expected success=true, got %v", responseBody["success"])
		}
	}
}

func testDestinationMinioRequestSuccess(t *testing.T, data []byte) {
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

func testDestinationHeaderRequestFail(t *testing.T, data []byte) {
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

func testDestinationAwsRequestFail(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 500, response.StatusCode, "Expected 500, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.Equal(t, "NotFound: Not Found\n\tstatus code: 404, request id: , host id: ", responseBody["error"], "NotFound: Not Found\n\tstatus code: 404, request id: , host id: ", responseBody["error"])
		}
	}
}

func testDestinationWithAwsWithSecretAddonFail(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 500, response.StatusCode, "Expected 500, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.Contains(t, responseBody["error"], "NoSuchBucket: The specified bucket does not exist")
		}
	}
}
func testDestinationMinioRequestFail(t *testing.T, data []byte) {
	response, err := testDestinationRequest(t, data)
	assert.Nil(t, err, "Error: %v", err)
	if assert.NotNil(t, response, "expected a response got nil") {
		if assert.Equal(t, 500, response.StatusCode, "Expected 500, got %d", response.StatusCode) {
			responseBody, err := parseResponse(response.Body)
			assert.Nil(t, err, "Error parsing response %v", err)
			assert.Contains(t, responseBody["error"], "NoSuchBucket: The specified bucket does not exist\n\tstatus code: 404")
		}
	}
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
		return "", fmt.Errorf("Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	responseBody, err := parseResponse(response.Body)
	if err != nil {
		return "", fmt.Errorf("Could not parse response body: %v", err)
	}
	secretId, ok := responseBody["id"].(string)
	if !ok {
		return "", errors.New("Response body has no id")
	}
	return secretId, nil
}

func deleteSecretRequest(secretId string, statusCode int) error {
	delete, err := http.NewRequest("DELETE", "https://127.0.0.1/api/v0/secrets/id/"+secretId, nil)
	if err != nil {
		return err
	}

	delete.Header.Add("api-token", automateApiToken)
	response, err := client.Do(delete)
	if err != nil {
		return err
	}
	if response.StatusCode != statusCode {
		return fmt.Errorf("Expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return nil
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
