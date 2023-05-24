package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/nfsmountservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupNFSMountHandler(nm nfsmountservice.INFSService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddNFSMountService(nm)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockNFSMountService() nfsmountservice.INFSService {
	return &nfsmountservice.MockNFSMountService{
		GetNFSMountDetailsFunc: func(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
			return &[]models.NFSMountResponse{
				{
					IP:       "localhost",
					NodeType: "automate",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "automate",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "chef-infra-server",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "chef-infra-server",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "postgresql",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "postgresql",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "postgresql",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "opensearch",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "opensearch",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
				{
					IP:       "localhost",
					NodeType: "opensearch",
					CheckList: []models.Checks{
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location found",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "NFS Mount",
							Passed:        true,
							SuccessMsg:    "NFS mount location is shared across given nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
					Error: nil,
				},
			}
		},
	}
}

func TestNFSMount(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName:     "Valid Body Request",
			ExpectedCode: 200,
			ExpectedBody: `{
				"status": "SUCCESS",
				"result": [
					{
						"ip": "localhost",
						"node_type": "automate",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "automate",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "chef-infra-server",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "chef-infra-server",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "postgresql",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "postgresql",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "postgresql",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "opensearch",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "opensearch",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					},
					{
						"ip": "localhost",
						"node_type": "opensearch",
						"checks": [
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location found",
								"error_msg": "",
								"resolution_msg": ""
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": ""
							}
						],
						"error": null
					}
				]
			}`,
			RequestBody: `{
				"automate_node_ips": [
					"localhost",
					"localhost"
				],
				"chef_infra_server_node_ips": [
					"localhost",
					"localhost"
				],
				"postgresql_node_ips": [
					"localhost",
					"localhost",
					"localhost"
				],
				"opensearch_node_ips": [
					"localhost",
					"localhost",
					"localhost"
				],
				"mount_location": "/mnt/automate_backups"
			}`,
		},
		{
			TestName:     "Invalid Body Request",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid Body Request"
				}
			}`,
			RequestBody: "Invalid Body",
		},
		{
			TestName:     "Not Given all the required IPs",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "AutomateNodeIPs, ChefInfraServerNodeIPs, PostgresqlNodeIPs or OpensearchNodeIPs cannot be empty"
				}
			}`,
			RequestBody: `{
				"automate_node_ips": []
			}`,
		},
		{
			TestName:     "Not Given the mount location",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Mount Location cannot be empty"
				}
			}`,
			RequestBody: `{
				"automate_node_ips": [
					"localhost",
					"localhost"
				],
				"chef_infra_server_node_ips": [
					"localhost",
					"localhost"
				],
				"postgresql_node_ips": [
					"localhost",
					"localhost",
					"localhost"
				],
				"opensearch_node_ips": [
					"localhost",
					"localhost",
					"localhost"
				]
			}`,
		},
	}

	NFSMountEndpoint := constants.NFS_MOUNT_API_PATH

	app, err := SetupNFSMountHandler(SetupMockNFSMountService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", NFSMountEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body) //semgrep
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
