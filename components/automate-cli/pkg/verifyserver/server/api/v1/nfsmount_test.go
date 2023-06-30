package v1_test

import (
	"io"
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

var (
	nfsErrorBodyParser = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Invalid Body Request\"}}"
	errorMountLocEmpty = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Mount Location cannot be empty\"}}"
	nfsNotFoundMsg     = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":404,\"message\":\"Failed to get NFS mount location\"}}"
	successMsg         = "{\"status\":\"SUCCESS\",\"result\":{\"address\":\"127.0.0.1\",\"mount_location\":\"/data\",\"nfs\":\"127.0.0.1:/\",\"storage_capacity\":\"9.5 GB\",\"available_free_space\":\"7.7 GB\"}}"
)

func SetupNFSMountHandler(nm nfsmountservice.NFSService) (*fiber.App, error) {
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

func SetupMockNFSMountService() nfsmountservice.NFSService {
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
		GetNFSMountLocFunc: func(reqBody models.NFSMountLocRequest) *models.NFSMountLocResponse {
			return &models.NFSMountLocResponse{
				Address:       "",
				Nfs:           "",
				MountLocation: "/data",
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
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
								"resolution_msg": "",
								"skipped":false
							},
							{
								"title": "NFS Mount",
								"passed": true,
								"success_msg": "NFS mount location is shared across given nodes",
								"error_msg": "",
								"resolution_msg": "",
								"skipped":false
							}
						],
						"error": null
					}
				]
			}`,
			RequestBody: `{
				"external_db_type":"",
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
			TestName:     "Not Given all the required IPs in On-Prem chef managed",
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
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}

func TestNFSMountLocationSuccess(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName:     "200:Success",
			ExpectedCode: 200,
			ExpectedBody: successMsg,
			RequestBody: `{
				"mount_location":"/data"
			}`,
		},
	}

	NFSMounLoctEndpoint := constants.NFS_MOUNT_LOC_API_PATH

	app, err := SetupNFSMountHandler(&nfsmountservice.MockNFSMountService{
		GetNFSMountLocFunc: func(reqBody models.NFSMountLocRequest) *models.NFSMountLocResponse {
			return &models.NFSMountLocResponse{
				Address:            "127.0.0.1",
				Nfs:                "127.0.0.1:/",
				MountLocation:      "/data",
				StorageCapacity:    "9.5 GB",
				AvailableFreeSpace: "7.7 GB",
			}
		},
	})
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			req := httptest.NewRequest("POST", NFSMounLoctEndpoint, strings.NewReader(test.RequestBody))
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.Contains(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}

func TestNFSMountLocationFailure(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName:     "400:Invalid Body Request",
			ExpectedCode: 400,
			ExpectedBody: nfsErrorBodyParser,
			RequestBody: `{
				"mount_location": "Invalid Body Request",
				""
			}`,
		},
		{
			TestName:     "400:Mount Location cannot be empty",
			ExpectedCode: 400,
			ExpectedBody: errorMountLocEmpty,
			RequestBody: `{
				"mount_location":""
			}`,
		},
		{
			TestName:     "404:Nfs not found",
			ExpectedCode: 404,
			ExpectedBody: nfsNotFoundMsg,
			RequestBody: `{
				"mount_location":"/data"
			}`,
		},
	}

	NFSMounLoctEndpoint := constants.NFS_MOUNT_LOC_API_PATH

	app, err := SetupNFSMountHandler(SetupMockNFSMountService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			req := httptest.NewRequest("POST", NFSMounLoctEndpoint, strings.NewReader(test.RequestBody))
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.Contains(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
