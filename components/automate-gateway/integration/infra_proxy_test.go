package integration

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/stretchr/testify/assert"
)

type parsedResponse map[string]interface{}

var client = NewClient()
var currenTimeStamp = time.Now().Second()

func NewClient() *http.Client {
	transport := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	return &http.Client{Transport: transport}
}

func (suite *GatewayTestSuite) TestInfraProxyServersAPI() {
	// add a mock server to Chef Infra
	addServer(suite)
	// check if the added server is available
	getServers(suite)
}

func addServer(suite *GatewayTestSuite) {
	ipAddress := "128.170.178.154"
	postBody, _ := json.Marshal(map[string]string{
		"id":         fmt.Sprintf("chef-server-%d-id", currenTimeStamp),
		"name":       fmt.Sprintf("chef-server-%d", currenTimeStamp),
		"fqdn":       fmt.Sprintf("chef-server-%d.com", currenTimeStamp),
		"ip_address": ipAddress,
	})
	post, err := http.NewRequest("POST", "https://127.0.0.1/api/v0/infra/servers", bytes.NewBuffer(postBody))
	post.Header.Add("api-token", suite.automateAPIToken)
	response, err := client.Do(post)
	if assert.Nil(suite.T(), err, "Error sending request %v", err) {
		assert.True(suite.T(), response.StatusCode == 200, "Expected Get Servers status code %d, got: %d", 200, response.StatusCode)
	}
}

func getServers(suite *GatewayTestSuite) {
	get, err := http.NewRequest("GET", "https://127.0.0.1/api/v0/infra/servers", nil)
	get.Header.Add("api-token", suite.automateAPIToken)
	response, err := client.Do(get)
	if assert.Nil(suite.T(), err, "Error sending request %v", err) {
		assert.True(suite.T(), response.StatusCode == 200, "Expected Get Servers status code %d, got: %d", 200, response.StatusCode)
	}
	resp, err := parseResponseBody(response.Body)
	servers := resp["servers"].([]interface{})
	found := false
	for _, s := range servers {
		m := s.(map[string]interface{})
		if m["fqdn"].(string) == fmt.Sprintf("chef-server-%d.com", currenTimeStamp) {
			found = true
			break
		}
	}

	assert.True(suite.T(), found, "Expected to get server %s but failed to receive the details", fmt.Sprintf("chef-server-%d.com", currenTimeStamp))
}

func parseResponseBody(body io.ReadCloser) (parsedResponse, error) {
	m := make(map[string]interface{})
	err := json.NewDecoder(body).Decode(&m)
	if err != nil {
		return nil, err
	}

	return m, err
}
