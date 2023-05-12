package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/hardwareresourcecount"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockHardwareResourceCountService() hardwareresourcecount.IHardwareResourceCountService {
	return &hardwareresourcecount.MockHardwareResourceCountService{
		GetHardwareResourceCountFunc: func(req models.HardwareResourceRequest) []models.HardwareResourceResponse {
			return []models.HardwareResourceResponse{
				{
					IP:       "172.154.0.1",
					NodeType: "Automate",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Automate Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.2",
					NodeType: "Automate",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Automate Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.3",
					NodeType: "Chef-infra-server",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Chef-infra-server Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.4",
					NodeType: "Chef-infra-server",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Chef-infra-server Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.5",
					NodeType: "Postgresql",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.6",
					NodeType: "Postgresql",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.7",
					NodeType: "Postgresql",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.8",
					NodeType: "Opensearch",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.9",
					NodeType: "Opensearch",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
				{
					IP:       "172.154.0.10",
					NodeType: "Opensearch",
					Checks: []models.Checks{
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is unique",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "IP address is of valid format",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Not shared with backend nodes",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
						{
							Title:         "IP address",
							Passed:        true,
							SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
							ErrorMsg:      "",
							ResolutionMsg: "",
						},
					},
				},
			}
		},
	}
}

func SetupHardwareResourceCountHandlers(hrc hardwareresourcecount.IHardwareResourceCountService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}
	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddHardwareResourceCountService(hrc)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup()
	return vs.App, nil
}

func TestHardwareResourceCountCheckAPI(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName: "200:success",
			RequestBody: `{
				"automate_node_count": 2,
				"automate_node_ips": [
				  "172.154.0.1",
				  "172.154.0.2"
				],
				"chef_infra_server_node_count": 2,
				"chef_infra_server_node_ips": [
				  "172.154.0.3",
				  "172.154.0.4"
				],
				"postgresql_node_count": 3,
				"postgresql_node_ips": [
				  "172.154.0.5",
				  "172.154.0.6",
				  "172.154.0.7"
				],
				"opensearch_node_count": 3,
				"opensearch_node_ips": [
				  "172.154.0.8",
				  "172.154.0.9",
				  "172.154.0.10"
				]
			  }`,
			ExpectedCode: 200,
			ExpectedBody: `{"status":"SUCCESS","result":[{"ip":"172.154.0.1","node_type":"Automate","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Automate Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.2","node_type":"Automate","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Automate Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.3","node_type":"Chef-infra-server","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Chef-infra-server Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.4","node_type":"Chef-infra-server","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Chef-infra-server Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.5","node_type":"Postgresql","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Postgresql Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.6","node_type":"Postgresql","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Postgresql Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.7","node_type":"Postgresql","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Postgresql Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.8","node_type":"Opensearch","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Opensearch Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.9","node_type":"Opensearch","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Opensearch Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]},{"ip":"172.154.0.10","node_type":"Opensearch","checks":[{"title":"IP address","passed":true,"success_msg":"IP address is unique","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"IP address is of valid format","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Not shared with backend nodes","error_msg":"","resolution_msg":""},{"title":"IP address","passed":true,"success_msg":"Opensearch Type has valid count as per Automate HA requirement","error_msg":"","resolution_msg":""}]}]}`,
		},
		{
			TestName:     "400:Failure Hardware Resource Count Check",
			RequestBody:  "Wrong body",
			ExpectedCode: 400,
			ExpectedBody: `{"status":"FAILED","result":null,"error":{"code":400,"message":"Invalid Body Request"}}`,
		},
		{
			TestName: "400:Failure Mismatch of Node Count and len of Node Ips",
			RequestBody: `{
				"automate_node_count": 2,
				"automate_node_ips": [
				  "172.154.0.1"
				],
				"chef_infra_server_node_count": 2,
				"chef_infra_server_node_ips": [
				  "172.154.0.3",
				  "172.154.0.4"
				],
				"postgresql_node_count": 3,
				"postgresql_node_ips": [
				  "172.154.0.5",
				  "172.154.0.6",
				  "172.154.0.7"
				],
				"opensearch_node_count": 3,
				"opensearch_node_ips": [
				  "172.154.0.8",
				  "172.154.0.9",
				  "172.154.0.10"
				]
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{"status":"FAILED","result":null,"error":{"code":400,"message":"Mismatch of Node Count and length of Node Ips."}}`,
		},
	}
	hardwareResourceCountCheckEndpoint := "/api/v1/checks/hardware-resource-count"
	// Setup the app as it is done in the main function
	app, err := SetupHardwareResourceCountHandlers(SetupMockHardwareResourceCountService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", hardwareResourceCountCheckEndpoint, bodyReader)
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
