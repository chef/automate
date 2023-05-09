package v1_test

import (
    "errors"
    "io/ioutil"
    "net/http/httptest"
    "testing"

    "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
    "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
    v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
    "github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
    "github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
    "github.com/chef/automate/lib/logger"
    "github.com/gofiber/fiber"
    "github.com/stretchr/testify/assert"
)

func SetupMockSoftwareVersionService(response models.SoftwareVersionDetails, err error) softwareversionservice.ISoftwareVersionService {
    return &softwareversionservice.MockSoftwareVersionService{
        GetSoftwareServicesFunc: func() (models.SoftwareVersionDetails, error) {
            return response, err
        },
    }
}

func SetupHandler(sv softwareversionservice.ISoftwareVersionService) *fiber.App {
    log, _ := logger.NewLogger("text", "debug")
    fconf := &fiber.Settings{
        ServerHeader: server.SERVICE,
        ErrorHandler: fiberutils.CustomErrorHandler,
    }
    app := fiber.New(fconf)
    handler := v1.NewHandler(log).AddSoftwareVersionService(sv)

    vs := &server.VerifyServer{
        Port:    server.DEFAULT_PORT,
        Log:     log,
        App:     app,
        Handler: handler,
    }
    vs.SetupRoutes()
    return vs.App
}

func TestSoftwareVersionAPI(t *testing.T) {
    tests := []struct {
        description   string
        expectedCode  int
        requestBody   models.SoftwareVersionDetails
        expectedBody  string
        expectedError error
    }{
        {
            description:  "200:success software version check route",
            expectedCode: 200,
            requestBody: models.SoftwareVersionDetails{
                Passed: true,
                Checks: []models.Checks{
                    {
                        Title:         "mkdir availability",
                        Passed:        true,
                        SuccessMsg:    "mkdir is available",
                        ErrorMsg:      "",
                        ResolutionMsg: "",
                    },
                },
            },
            expectedBody:  `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""}]}}`,
            expectedError: nil,
        },
        {
            description:   "500:Error while getting the file",
            expectedCode:  500,
            requestBody:   models.SoftwareVersionDetails{},
            expectedBody:  `{"status":"FAILED","result":null,"error":{"code":0,"message":"open /testfile: no such file or directory"}}`,
            expectedError: errors.New("open /testfile: no such file or directory"),
        },
    }
    softwareversioncheckEndpoint := "/api/v1/checks/software-versions/?node_type=opensearch"
    for _, test := range tests {

        t.Run(test.description, func(t *testing.T) {
            app := SetupHandler(SetupMockSoftwareVersionService(test.requestBody, test.expectedError))
            req := httptest.NewRequest("GET", softwareversioncheckEndpoint, nil)
            req.Header.Add("Content-Type", "application/json")
            res, err := app.Test(req, -1)
            assert.NoError(t, err)
            body, err := ioutil.ReadAll(res.Body)
            assert.NoError(t, err, test.description)
            assert.Contains(t, string(body), test.expectedBody)
            assert.Equal(t, res.StatusCode, test.expectedCode)
        })
    }
}