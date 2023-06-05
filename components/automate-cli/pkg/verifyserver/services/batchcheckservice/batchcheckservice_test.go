package batchcheckservice

import (
	"bytes"
	"encoding/json"

	"errors"
	"io/ioutil"
	"net/http"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"

	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupMockHttpRequestClient(responseJson string, err error, shouldStartMockServerOnSomeNodesOnly bool) httputils.IHttpRequestClient {
	r := ioutil.NopCloser(bytes.NewReader([]byte(responseJson)))
	return &httputils.MockHttpRequestClient{
		MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, error) {
			if shouldStartMockServerOnSomeNodesOnly && url == "http://1.2.3.4:1234/api/v1/start/mock-server" {
				return &http.Response{
					StatusCode: 500,
				}, errors.New("Error occurred")
			}
			return &http.Response{
				StatusCode: 200,
				Body:       r,
			}, err
		},
	}
}

func TestBatchCheckService(t *testing.T) {
	tests := []struct {
		description                          string
		totalIpsCount                        int
		hardwareCheckMockedResponse          []models.CheckTriggerResponse
		sshUserCheckMockedResponse           []models.CheckTriggerResponse
		externalOpenSearchMockedResponse     []models.CheckTriggerResponse
		externalPostgresMockedResponse       []models.CheckTriggerResponse
		checksToExecute                      []string
		chefServerIpArray                    []string
		statusApiResponse                    string
		statusApiError                       error
		avoidSuccessResponse                 bool
		checkForError                        bool
		shouldStartMockServerOnSomeNodesOnly bool
		expectedResponseForAutomateIp        string
		expectedResponseFromChefServerIp     string
		expectedResponseForPostgresIp1       string
		expectedResponseForPostgresIp2       string
		expectedResponseForOpenSearchIp1     string
		expectedResponseForOpenSearchIp2     string
		mockServerPort                       string
	}{
		{
			description:          "Batch check service returns error when status api fails",
			totalIpsCount:        6,
			chefServerIpArray:    []string{"1.2.3.4"},
			avoidSuccessResponse: true,
			checkForError:        true,
			mockServerPort:       "1212",
			statusApiResponse:    `{}`,
			statusApiError:       errors.New("Invalid request"),
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
		},
		{
			description:          "Batch check service returns error when status api response body parse fails",
			totalIpsCount:        6,
			chefServerIpArray:    []string{"1.2.3.4"},
			avoidSuccessResponse: true,
			checkForError:        true,
			mockServerPort:       "1234",
			statusApiResponse:    `invalid JSON`,
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
		},
		{
			description:   "Batch check server success with automate and chef-server on same IP",
			totalIpsCount: 6,
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
			chefServerIpArray: []string{"1.2.3.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-count-check-1",
							},
							{
								Title: "hardware-resource-count-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"Certificate Check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":true,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Certificate Check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-count-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-count-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with nodes on different ip",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"Certificate Check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with error on some nodes",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Failed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{},
						Error:  fiber.NewError(fiber.StatusServiceUnavailable, "Error while Performing Hardware resource count check from batch Check API"),
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[],\"error\":{\"code\":503,\"message\":\"Error while Performing Hardware resource count check from batch Check API\"}},{\"passed\":false,\"msg\":\"Certificate Check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"SSH User Access Check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"External Postgresql Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with nodes on different ip",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"certificate-check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with error on some nodes",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Failed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks:  []models.Checks{},
						Error:   fiber.NewError(fiber.StatusServiceUnavailable, "Error while Performing Hardware resource count check from batch Check API"),
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[],\"error\":{\"code\":503,\"message\":\"Error while Performing Hardware resource count check from batch Check API\"}},{\"passed\":false,\"msg\":\"certificate-check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with nodes on different ip",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"certificate-check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:                          "Batch check service returns error mock server in not started successfully on all nodes",
			totalIpsCount:                        6,
			chefServerIpArray:                    []string{"1.2.3.4"},
			avoidSuccessResponse:                 true,
			checkForError:                        true,
			mockServerPort:                       "1234",
			shouldStartMockServerOnSomeNodesOnly: true,
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
		},
		{
			description:       "Batch check server success with error on some nodes",
			totalIpsCount:     7,
			chefServerIpArray: []string{"1.2.8.4"},
			statusApiResponse: `{
				"status": "SUCCESS",
				"result": {
					"status": "OK",
					"services": [],
					"error": "error getting services from hab svc status"
				}
			}`,
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Failed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks:  []models.Checks{},
						Error:   fiber.NewError(fiber.StatusServiceUnavailable, "Error while Performing Hardware resource count check from batch Check API"),
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[],\"error\":{\"code\":503,\"message\":\"Error while Performing Hardware resource count check from batch Check API\"}},{\"passed\":false,\"msg\":\"certificate-check\",\"check\":\"certificate\",\"checks\":[{\"title\":\"certificate-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"certificate-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
	}

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			ss := NewBatchCheckService(trigger.NewCheckTrigger(SetupMockHardwareResourceCountCheck(test.hardwareCheckMockedResponse),
				SetupMockSshUserAccessCheck(test.sshUserCheckMockedResponse),
				SetupMockCertificateCheck(),
				SetupMockExternalOpenSearchCheck(test.externalOpenSearchMockedResponse),
				SetupMockExternalPostgresCheck(test.externalPostgresMockedResponse),
				SetupMockFirewallCheck(),
				SetupMockFqdnCheck(),
				SetupMockNfsBackupConfigCheck(),
				SetupMockOpenSearchS3BucketAccessCheck(),
				SetupMockS3BackupConfigCheck(),
				SetupMockSoftwareVersionCheck(),
				SetupMockSystemResourceCheck(),
				SetupMockSystemUserCheck(),
			), logger.NewTestLogger(), "1234")

			ss.httpRequestClient = SetupMockHttpRequestClient(test.statusApiResponse, test.statusApiError, test.shouldStartMockServerOnSomeNodesOnly)
			checksToExecute := []string{
				constants.FIREWALL,
				constants.FQDN,
				constants.SSH_USER,
				constants.HARDWARE_RESOURCE_COUNT,
				constants.EXTERNAL_OPENSEARCH,
				constants.EXTERNAL_POSTGRESQL,
				constants.NFS_BACKUP_CONFIG,
				constants.S3_BACKUP_CONFIG,
				constants.CERTIFICATE,
				constants.SOFTWARE_VERSIONS,
				constants.SYSTEM_RESOURCES,
				constants.SYSTEM_USER,
				constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS,
			}

			if len(test.checksToExecute) > 0 {
				checksToExecute = test.checksToExecute
			}
			resp, err := ss.BatchCheck(checksToExecute, models.Config{
				Hardware: models.Hardware{
					AutomateNodeCount:        1,
					AutomateNodeIps:          []string{"1.2.3.4"},
					ChefInfraServerNodeCount: 1,
					ChefInfraServerNodeIps:   test.chefServerIpArray,
					PostgresqlNodeCount:      1,
					PostgresqlNodeIps:        []string{"1.2.3.7", "1.2.3.8"},
					OpenSearchNodeCount:      1,
					OpenSearchNodeIps:        []string{"1.2.3.5", "1.2.3.6"},
				},
			})

			if test.checkForError {
				assert.NotEqual(t, err, nil)
			}

			if !test.avoidSuccessResponse {
				assert.Equal(t, resp.Status, "SUCCESS")
				assert.Equal(t, len(resp.Result), test.totalIpsCount)
				assert.Equal(t, test.expectedResponseForAutomateIp, getResponseForIp(resp.Result, "1.2.3.4", "automate"))
				assert.Equal(t, test.expectedResponseFromChefServerIp, getResponseForIp(resp.Result, test.chefServerIpArray[0], "chef-infra-server"))
				assert.Equal(t, test.expectedResponseForPostgresIp1, getResponseForIp(resp.Result, "1.2.3.7", "postgresql"))
				assert.Equal(t, test.expectedResponseForPostgresIp2, getResponseForIp(resp.Result, "1.2.3.8", "postgresql"))
				assert.Equal(t, test.expectedResponseForOpenSearchIp1, getResponseForIp(resp.Result, "1.2.3.5", "opensearch"))
				assert.Equal(t, test.expectedResponseForOpenSearchIp2, getResponseForIp(resp.Result, "1.2.3.6", "opensearch"))
			}
		})
	}

}

func TestStartMockServer(t *testing.T) {
	ss := getBatchCheckServiceInstance()
	ss.httpRequestClient = SetupMockHttpRequestClient(`{
		"status": "SUCCESS",
		"result": {
			"status": "OK",
			"services": [],
			"error": "error getting services from hab svc status"
		}
	}`, nil, false)
	startedServers, failedServers := ss.StartMockServer([]string{constants.FIREWALL, constants.FQDN},
		models.Hardware{
			AutomateNodeCount:        1,
			AutomateNodeIps:          []string{"1.2.3.4"},
			ChefInfraServerNodeCount: 1,
			ChefInfraServerNodeIps:   []string{"1.2.3.4"},
			PostgresqlNodeCount:      1,
			PostgresqlNodeIps:        []string{"1.2.3.7", "1.2.3.8"},
			OpenSearchNodeCount:      1,
			OpenSearchNodeIps:        []string{"1.2.3.5", "1.2.3.6"},
		},
	)
	totalA := 0
	totalP := 0
	totalO := 0
	for _, resp := range startedServers {
		if resp.Host == "1.2.3.4" {
			totalA += 1
		}
		if resp.Host == "1.2.3.7" || resp.Host == "1.2.3.8" {
			totalP += 1
		}
		if resp.Host == "1.2.3.5" || resp.Host == "1.2.3.6" {
			totalO += 1
		}
	}
	assert.Equal(t, len(startedServers), 24)
	assert.Equal(t, len(failedServers), 0)
}

func TestGetDeploymentStatePostDeploy(t *testing.T) {
	ss := getBatchCheckServiceInstance()

	ss.httpRequestClient = SetupMockHttpRequestClient("", errors.New("error occurred"), false)
	ss.StopMockServerOnHostAndPort("1.2.3.4", "tcp", 1234)
	assert.NotNil(t, ss.log)
}

func TestStopMockServerOnHostAndPortPostDeploy(t *testing.T) {
	ss := getBatchCheckServiceInstance()
	ss.httpRequestClient = SetupMockHttpRequestClient(`{
		"status": "SUCCESS",
		"result": {
			"status": "OK",
			"services": [{"service_name": "deployment-service", "status": "OK", "version": "version-1"}],
			"error": ""
		}
	}`, nil, false)
	deployState, _ := ss.GetDeploymentState()
	assert.Equal(t, "post-deploy", deployState)
}

func TestStopMockServerOnHostAndPortPostDeployWhenServicesStopped(t *testing.T) {
	ss := getBatchCheckServiceInstance()
	ss.httpRequestClient = SetupMockHttpRequestClient(`{
		"status": "SUCCESS",
		"result": {
			"status": "OK",
			"services": [],
			"error": ""
		}
	}`, nil, false)
	deployState, _ := ss.GetDeploymentState()
	assert.Equal(t, "post-deploy", deployState)
}

func TestShouldStartMockServer(t *testing.T) {
	ss := getBatchCheckServiceInstance()

	ss.httpRequestClient = SetupMockHttpRequestClient(`{
		"status": "SUCCESS",
		"result": {
			"status": "OK",
			"services": [{"service_name": "deployment-service", "status": "OK", "version": "version-1"}],
			"error": ""
		}
	}`, nil, false)
	shouldStartMockServer, _ := ss.ShouldStartMockServer([]string{"abc"})
	assert.Equal(t, false, shouldStartMockServer)
}

func getResponseForIp(resp []models.BatchCheckResult, ip string, nodeType string) string {
	for _, res := range resp {
		if res.Ip == ip && res.NodeType == nodeType {
			b, _ := json.Marshal(res)
			return string(b)
		}
	}
	return ""
}

func getBatchCheckServiceInstance() *BatchCheckService {
	return NewBatchCheckService(trigger.NewCheckTrigger(SetupMockHardwareResourceCountCheck([]models.CheckTriggerResponse{}),
		SetupMockSshUserAccessCheck([]models.CheckTriggerResponse{}),
		SetupMockCertificateCheck(),
		SetupMockExternalOpenSearchCheck([]models.CheckTriggerResponse{}),
		SetupMockExternalPostgresCheck([]models.CheckTriggerResponse{}),
		SetupMockFirewallCheck(),
		SetupMockFqdnCheck(),
		SetupMockNfsBackupConfigCheck(),
		SetupMockOpenSearchS3BucketAccessCheck(),
		SetupMockS3BackupConfigCheck(),
		SetupMockSoftwareVersionCheck(),
		SetupMockSystemResourceCheck(),
		SetupMockSystemUserCheck(),
	), logger.NewTestLogger(), "1234")
}

func TestConstructResult(t *testing.T) {
	// Test case 1: Empty input map
	ipMap := make(map[string][]models.CheckTriggerResponse)
	result := constructResult(ipMap)
	assert.Empty(t, result, "Result should be empty for an empty input map")

	// Test case 2: Input map with one item
	ipMap = make(map[string][]models.CheckTriggerResponse)
	ipMap["key1"] = []models.CheckTriggerResponse{
		{
			Status: "status1",
			Result: models.ApiResult{
				Passed:  true,
				Message: "Test message 1",
				Check:   "check1",
				Checks: []models.Checks{
					{
						Title:         "Title 1",
						Passed:        true,
						SuccessMsg:    "Success 1",
						ErrorMsg:      "Error 1",
						ResolutionMsg: "Resolution 1",
					},
				},
			},
			Host:      "host1",
			NodeType:  "nodeType1",
			CheckType: "checkType1",
		},
	}
	result = constructResult(ipMap)
	assert.Len(t, result, 1, "Result should have one item")
	assert.Equal(t, "host1", result[0].Ip, "IP should be 'host1'")
	assert.Equal(t, "nodeType1", result[0].NodeType, "NodeType should be 'nodeType1'")
	assert.Len(t, result[0].Tests, 1, "Tests should have one item")
	assert.Equal(t, true, result[0].Tests[0].Passed, "Passed should be true")
	assert.Equal(t, "Test message 1", result[0].Tests[0].Message, "Message should be 'Test message 1'")

	// Test case 3: Input map with multiple items
	ipMap = make(map[string][]models.CheckTriggerResponse)
	ipMap["key1"] = []models.CheckTriggerResponse{
		{
			Status: "status1",
			Result: models.ApiResult{
				Passed:  true,
				Message: "Test message 1",
				Check:   "check1",
				Checks: []models.Checks{
					{
						Title:         "Title 1",
						Passed:        true,
						SuccessMsg:    "Success 1",
						ErrorMsg:      "Error 1",
						ResolutionMsg: "Resolution 1",
					},
				},
			},
			Host:      "host1",
			NodeType:  "nodeType1",
			CheckType: "checkType1",
		},
	}
	ipMap["key2"] = []models.CheckTriggerResponse{
		{
			Status: "status2",
			Result: models.ApiResult{
				Passed:  true,
				Message: "Test message 2",
				Check:   "check2",
				Checks: []models.Checks{
					{
						Title:         "Title 2",
						Passed:        true,
						SuccessMsg:    "Success 2",
						ErrorMsg:      "Error 2",
						ResolutionMsg: "Resolution 2",
					},
				},
			},
			Host:      "host2",
			NodeType:  "nodeType2",
			CheckType: "checkType2",
		},
	}
	result = constructResult(ipMap)
	assert.Len(t, result, 2, "Result should have two items")

	for _, v := range result {
		// Check the nodeType1
		if v.NodeType == "nodeType1" {
			assert.Equal(t, "host1", result[0].Ip, "IP should be 'host1'")
			assert.Len(t, result[0].Tests, 1, "Tests should have one item")
			assert.Equal(t, true, result[0].Tests[0].Passed, "Passed should be true")
			assert.Equal(t, "Test message 1", result[0].Tests[0].Message, "Message should be 'Test message 1'")
		}

		// Check the nodeType2
		if v.NodeType == "nodeType2" {
			assert.Equal(t, "host2", result[1].Ip, "IP should be 'host2'")
			assert.Equal(t, "nodeType2", result[1].NodeType, "NodeType should be 'nodeType2'")
			assert.Len(t, result[1].Tests, 1, "Tests should have one item")
			assert.Equal(t, true, result[1].Tests[0].Passed, "Passed should be true")
			assert.Equal(t, "Test message 2", result[1].Tests[0].Message, "Message should be 'Test message 2'")
		}

	}

	// Test case 4: Input map with an empty list
	ipMap = make(map[string][]models.CheckTriggerResponse)
	ipMap["key1"] = []models.CheckTriggerResponse{} // Empty list
	result = constructResult(ipMap)
	assert.Empty(t, result[0].Ip)
	assert.Empty(t, result[0].NodeType)
	assert.Empty(t, result[0].Tests)
	assert.Len(t, result, 1)
}
