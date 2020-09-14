package integration_test

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"reflect"
	"testing"
	"time"

	"github.com/gofrs/uuid"

	"github.com/chef/automate/api/external/ingest/request"

	"github.com/stretchr/testify/assert"
)

func TestClientRun(t *testing.T) {
	response, err := addDestinationRequest(suite.assetDestination, 200)
	if !assert.Nil(t, err, "error adding asset destination: %v", err) {
		suite.setAssetComplete()
		return
	}
	body, err := parseHttpBody(response.Body)
	if !assert.Nil(t, err, "error parsing add asset response: %v", err) {
		suite.setAssetComplete()
		return
	}
	destinationId := body["id"].(string)
	data, err := buildClientRun("./data/chef_client_run.json")
	if assert.Nil(t, err) {
		suite.registerAssetHandler(t)
		addIngestRequest(t, data, 200)
		t.Log("waiting 90 seconds for datafeed loop to execute")
		time.Sleep(90 * time.Second)
	}
	_, _ = deleteDestinationRequest(destinationId, 200)
}

func TestComplianceReport(t *testing.T) {
	response, err := addDestinationRequest(suite.complianceDestination, 200)
	if !assert.Nil(t, err, "error adding compliance destination: %v", err) {
		suite.setComplianceComplete()
		return
	}
	body, err := parseHttpBody(response.Body)
	if !assert.Nil(t, err, "error parsing compliance asset response: %v", err) {
		suite.setComplianceComplete()
		return
	}
	destinationId := body["id"].(string)
	data, err := buildComplianceReport("./data/redhat-alpha-nginx-apache-failed.json")
	if assert.Nil(t, err) {
		suite.registerComplianceHandler(t)
		addComplianceRequest(t, data, 200)
		t.Log("waiting 90 seconds for datafeed loop to execute")
		time.Sleep(90 * time.Second)
	}
	_, _ = deleteDestinationRequest(destinationId, 200)
}

func addIngestRequest(t *testing.T, data []byte, statusCode int) *http.Response {
	ingest, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/ingest/events/chef/run", bytes.NewBuffer(data))
	assert.Nil(t, err, "error creating http request: %v", err)
	ingest.Header.Add("api-token", automateApiToken)
	response, err := client.Do(ingest)
	if assert.Nil(t, err, "error sending request %v", err) {
		assert.Equal(t, statusCode, response.StatusCode, "expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func addComplianceRequest(t *testing.T, data []byte, statusCode int) *http.Response {
	ingest, err := http.NewRequest("POST", "https://127.0.0.1/data-collector/v0/", bytes.NewBuffer(data))
	assert.Nil(t, err, "error creating http request: %v", err)
	ingest.Header.Add("api-token", automateApiToken)
	response, err := client.Do(ingest)
	if assert.Nil(t, err, "error sending request %v", err) {
		assert.Equal(t, statusCode, response.StatusCode, "expected status code %d, got: %d", statusCode, response.StatusCode)
	}
	return response
}

func (handler *AssetHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	node := make(map[string]interface{})
	handler.t.Log("asset handler data feed received")
	reader, err := gzip.NewReader(r.Body)
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		suite.setAssetComplete()
		return
	}
	buf, err := ioutil.ReadAll(reader)
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		suite.setAssetComplete()
		return
	}

	err = json.Unmarshal(buf, &node)
	assert.Nil(handler.t, err, "%v", err)
	errs := verifyClientRun(handler.t, node)

	if !assert.True(handler.t, len(errs) == 0, "errors parsing datafeed json") {
		http.Error(w, "errors parsing Datafeed JSON", http.StatusBadRequest)
		suite.setAssetComplete()
		return
	}
	suite.setAssetComplete()
}

func (handler *ComplianceHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	node := make(map[string]interface{})
	handler.t.Log("compliance handler data feed received")
	reader, err := gzip.NewReader(r.Body)
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		suite.setComplianceComplete()
		return
	}

	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		nodeString := scanner.Text()
		err = json.Unmarshal([]byte(nodeString), &node)
		assert.Nil(handler.t, err, "%v", err)
	}
	if err != nil {
		msg := fmt.Sprintf("%v", err)
		handler.t.Error(msg)
		http.Error(w, msg, http.StatusBadRequest)
		suite.setComplianceComplete()
		return
	}

	errs := verifyComplianceReport(handler.t, node)

	if !assert.True(handler.t, len(errs) == 0, "error parsing datafeed json %v", errs) {
		http.Error(w, "errors parsing datafeed json", http.StatusBadRequest)
		suite.setComplianceComplete()
		return
	}
	suite.setComplianceComplete()
}

func buildClientRun(filePath string) ([]byte, error) {
	var data []byte
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return data, err
	}
	var clientRun *request.Run
	err = json.Unmarshal(data, &clientRun)
	if err != nil {
		return data, err
	}
	layout := "2006-01-02T15:04:05Z"
	clientRunUUID, _ := uuid.DefaultGenerator.NewV4()
	clientRunId = clientRunUUID.String()
	clientRun.Id = clientRunId
	clientRun.RunId = clientRunId
	log.Printf("client run id %s", clientRun.Id)
	clientRun.StartTime = time.Now().Format(layout)
	clientRun.EndTime = clientRun.StartTime
	data, err = json.Marshal(clientRun)
	if err != nil {
		return data, err
	}
	return data, err
}

func buildComplianceReport(filePath string) ([]byte, error) {
	var data []byte
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return data, err
	}
	var report map[string]interface{}
	err = json.Unmarshal(data, &report)
	if err != nil {
		return data, err
	}
	layout := "2006-01-02T15:04:05Z"
	// need to store this and verify against report received  as this test seems to have uncovered an issue
	reportUUID, _ := uuid.DefaultGenerator.NewV4()
	reportId = reportUUID.String()
	report["report_uuid"] = reportId
	report["end_time"] = time.Now().Format(layout)
	data, err = json.Marshal(report)
	if err != nil {
		return data, err
	}
	return data, err
}

func verifyClientRun(t *testing.T, node map[string]interface{}) []error {
	clientRun := node["client_run"].(map[string]interface{})
	errs := make([]error, 0)
	errs = testStringValue(t, clientRun, "id", clientRunId, errs)
	errs = testStringValue(t, clientRun, "node_name", "example.com", errs)
	errs = testStringValue(t, clientRun, "node_id", "0271e125-97dd-498a-b026-8448ee60aafe", errs)
	errs = testStringValue(t, clientRun, "organization", "chef_delivery", errs)
	errs = testStringValue(t, clientRun, "source", "chef_delivery", errs)
	errs = testStringValue(t, clientRun, "status", "success", errs)
	errs = testFloatValue(t, clientRun, "total_resource_count", 10, errs)
	errs = testFloatValue(t, clientRun, "updated_resource_count", 4, errs)
	errs = testStringValue(t, clientRun, "chef_version", "12.6.0", errs)
	errs = testFloatValue(t, clientRun, "uptime_seconds", 12342607, errs)
	errs = testStringValue(t, clientRun, "environment", "wilson", errs)
	errs = testStringValue(t, clientRun, "fqdn", "ip-170-76-111-224.us-west-2.compute.internal", errs)
	errs = testStringValue(t, clientRun, "source_fqdn", "chef-server.insights.co", errs)
	errs = testStringValue(t, clientRun, "ip6address", "fe80::3f:b5ff:fe7e:adff", errs)
	errs = testStringValue(t, clientRun, "timezone", "EDT", errs)
	errs = testStringValue(t, clientRun, "domain", "us-west-2.compute.internal", errs)
	errs = testStringValue(t, clientRun, "hostname", "ip-170-76-111-224", errs)
	errs = testStringValue(t, clientRun, "memory_total", "3689604kB", errs)
	errs = testStringValue(t, clientRun, "macaddress", "02:3F:B5:7E:AD:FF", errs)
	errs = testStringValue(t, clientRun, "dmi_system_serial_number", "ec2e0f80-06cc-d659-e439-34147116b49f", errs)
	errs = testStringValue(t, clientRun, "dmi_system_manufacturer", "Xen", errs)
	errs = testStringValue(t, clientRun, "virtualization_role", "guest", errs)
	errs = testStringValue(t, clientRun, "virtualization_system", "xen", errs)
	errs = testStringValue(t, clientRun, "kernel_version", "#1 SMP Thu Jan 29 18:37:38 EST 2015", errs)
	errs = testStringValue(t, clientRun, "kernel_release", "3.10.0-229.el7.x86_64", errs)
	errs = testStringValue(t, clientRun, "cloud_provider", "ec2", errs)
	errs = testStringValue(t, clientRun, "platform", "redhat 7.1", errs)
	errs = testStringValue(t, clientRun, "platform_family", "rhel", errs)
	errs = testStringValue(t, clientRun, "platform_version", "7.1", errs)
	errs = testStringValue(t, clientRun, "policy_revision", "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482", errs)
	errs = verifyJSONEquals(t, "./data/expected_json/attributes.json", node["attributes"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/chef_tags.json", clientRun["chef_tags"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/client_node.json", node["node"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/cookbooks.json", clientRun["cookbooks"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/error.json", clientRun["error"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/expanded_runlist.json", clientRun["expanded_run_list"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/recipes.json", clientRun["recipes"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/resource_names.json", clientRun["resource_names"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/resources.json", clientRun["resources"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/roles.json", clientRun["roles"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/runlist.json", clientRun["run_list"], errs)
	errs = verifyJSONEquals(t, "./data/expected_json/versioned_cookbooks.json", clientRun["versioned_cookbooks"], errs)

	return errs
}

func verifyComplianceReport(t *testing.T, node map[string]interface{}) []error {
	report := node["report"].(map[string]interface{})
	errs := make([]error, 0)
	errs = testStringValue(t, report, "id", reportId, errs)
	errs = testStringValue(t, report, "node_id", "9b9f4e51-b049-4b10-9555-10578916e111", errs)
	errs = testStringValue(t, report, "node_name", "redhat(2)-alpha-nginx(f)-apache(s)-failed", errs)
	errs = testStringValue(t, report, "status", "failed", errs)
	errs = testStringValue(t, report, "environment", "DevSec Prod Alpha", errs)
	errs = testStringValue(t, report, "version", "3.1.3", errs)
	errs = testStringValue(t, report, "job_id", "74a54a28-c628-4f82-86df-333333333334", errs)
	errs = testStringValue(t, report, "ipaddress", "8.8.8.2", errs)
	errs = testStringValue(t, report, "fqdn", "web-rh2.example.com", errs)
	errs = testStringValue(t, report, "chef_server", "localhost", errs)
	platform := report["platform"].(map[string]interface{})
	errs = testStringValue(t, platform, "name", "redhat", errs)
	errs = testStringValue(t, platform, "release", "6.11", errs)
	errs = testStringValue(t, platform, "full", "redhat 6.11", errs)
	roles := report["roles"].([]interface{})
	expectedRoles := []interface{}{"base_linux", "apache_linux", "nginx-hardening-prod", "dot.role"}
	rolesError := fmt.Sprintf("expected roles %v, got %v", expectedRoles, roles)
	if !assert.Equal(t, expectedRoles, roles, rolesError) {
		errs = append(errs, errors.New(rolesError))
	}
	profiles := report["profiles"].([]interface{})
	if !assert.Equal(t, 2, len(profiles), "profile length not 2, got %d", len(profiles)) {
		errs = append(errs, errors.New("expected two profile"))
	} else {
		profile := profiles[0].(map[string]interface{})
		errs = testStringValue(t, profile, "name", "nginx-baseline", errs)
		errs = testStringValue(t, profile, "title", "DevSec Nginx Baseline", errs)
		errs = testStringValue(t, profile, "copyright", "DevSec Hardening Framework Team", errs)
		errs = testStringValue(t, profile, "copyright_email", "hello@dev-sec.io", errs)
		errs = testStringValue(t, profile, "summary", "Test-suite for best-practice nginx hardening", errs)
		errs = testStringValue(t, profile, "version", "2.1.0", errs)
		errs = testStringValue(t, profile, "full", "DevSec Nginx Baseline, v2.1.0", errs)
		errs = testStringValue(t, profile, "sha256", "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988", errs)
		errs = verifyJSONEquals(t, "./data/expected_json/compliance.json", report["profiles"], errs)
	}

	return errs
}

func verifyJSONEquals(t *testing.T, filename string, actualJson interface{}, errs []error) []error {
	data, err := ioutil.ReadFile(filename)
	if !assert.Nil(t, err) {
		errs = append(errs, errors.New(fmt.Sprintf("couldn't read file %s", filename)))
	}
	dataBuffer := new(bytes.Buffer)
	if err = json.Compact(dataBuffer, data); err != nil {
		errs = append(errs, errors.New(fmt.Sprintf("Could not compact file %s", filename)))
		return errs
	}
	expectedBytes := dataBuffer.Bytes()
	var expectedJson interface{}
	err = json.Unmarshal(expectedBytes, &expectedJson)
	if !assert.Nil(t, err) {
		errs = append(errs, errors.New(fmt.Sprintf("couldn't unmarshal json from file %s", filename)))
	}
	if !assert.True(t, reflect.DeepEqual(expectedJson, actualJson), "json objects differ") {
		errs = append(errs, errors.New("json objects differ"))
		t.Logf("expected: %s", expectedJson)
		t.Log()
		t.Log()
		t.Log()
		t.Logf("actual: %s", actualJson)
	}
	return errs
}
