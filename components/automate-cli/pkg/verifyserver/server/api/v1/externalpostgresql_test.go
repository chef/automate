package v1_test

import (
	"errors"
	"io"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

var (
	ExternalPgsuccessMessage      = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"checks\":[{\"title\":\"Machine is able to connect with external managed PostgreSQL\",\"passed\":true,\"status\":\"\",\"success_msg\":\"Connection successfully tested\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}}"
	ExternalPgfailureMessage      = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"External Postgresql connection failed\",\"passed\":false,\"status\":\"\",\"success_msg\":\"\",\"error_msg\":\"Machine is unable to connect with external managed PostgreSQL\",\"resolution_msg\":\"Ensure that the PostgreSQL configuration provided is correct. Review security group or firewall settings as well on the infrastructure\"}]}}"
	ExternalPgInternalServerError = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Internal Server Error\"}}"
	ErrorBodyParser               = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"invalid character '}' looking for beginning of object key string\"}}"
	InvalidRequest                = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Request Parameters cannot be empty\"}}"
)

func SetupMockExternalPostgresqlService(responseBody *models.ExternalPgResponse, err error) externalpostgresqlservice.ExternalPostgresqlService {
	return &externalpostgresqlservice.MockExternalPostgresqlService{
		GetPgConnectionFunc: func(*models.ExternalPgRequest) (*models.ExternalPgResponse, error) {
			return responseBody, err
		},
	}
}

func SetupExternalPostgresqlHandlers(pg externalpostgresqlservice.ExternalPostgresqlService) (*fiber.App, error) {
	log, _ := logger.NewLogger("text", "debug")
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddExternalPostgresqlService(pg)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func TestExternalPostgresql(t *testing.T) {
	tests := []struct {
		description   string
		responseBody  *models.ExternalPgResponse
		expectedCode  int
		expectedBody  string
		expectedError error
		body          string
	}{
		{
			description: "200:success status route",
			responseBody: &models.ExternalPgResponse{
				Passed: true,
				Checks: []models.ExternalPgConnectionDetails{
					{
						Title:         constants.EXTERNAL_PG_SUCCESS_CONNECTION_TITLE,
						Passed:        true,
						SuccessMsg:    constants.EXTERNAL_PG_CONNECTION_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedCode: 200,
			expectedBody: ExternalPgsuccessMessage,
			body: `{
				"postgresql_instance_url": "A.B.C.D",
				"postgresql_instance_port": "7432",
				"postgresql_superuser_username": "postgres",
				"postgresql_superuser_password": "Progress123",
				"postgresql_dbuser_username": "postgres",
				"postgresql_dbuser_password": "Progress123",
				"postgresql_root_cert": "----- BEGIN CERTIFICATE ------"
			}`,
		},
		{
			description:  "400: body parser error",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: ErrorBodyParser,
			body: `{
						"postgresql_instance_url": "A.B.C.D",
						"postgresq_instance_port": "7432",
					}`,
		},
		{
			description:  "400: body parser error with special character",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: InvalidRequest,
			body: `{
				"postgresql&_instance_url": "A.B.C.D",
				"postgresql_instance_port": "7432",
				"postgresql_superuser_username": "postgres",
				"postgresql_superuser_password": "Progress123",
				"postgresql_dbuser_username": "postgres",
				"postgresql_dbuser_password": "Progress123",
				"postgresql_root_cert": ""
				}`,
		},
		{
			description:  "400: body parser error with invalid type",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: InvalidRequest,
			body: `{
				"postgresql&_instance_url": 1234,
				"postgresql_instance_port": "7432",
				"postgresql_superuser_username": "postgres",
				"postgresql_superuser_password": "Progress123",
				"postgresql_dbuser_username": "postgres",
				"postgresql_dbuser_password": "Progress123",
				"postgresql_root_cert": ""
				}`,
		},
		{
			description:  "400: Request validator with empty string",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: InvalidRequest,
			body: `{
				"postgresql_instance_url": "A.B.C.D",
				"postgresql_instance_port": "7432",
				"postgresql_superuser_username": "postgres",
				"postgresql_superuser_password": "Progress123",
				"postgresql_dbuser_username": "postgres",
				"postgresql_dbuser_password": "Progress123",
				"postgresql_root_cert": ""
				}`,
		},
		{
			description:   "500: Internal Server error",
			responseBody:  nil,
			expectedCode:  500,
			expectedBody:  ExternalPgInternalServerError,
			expectedError: errors.New("Internal Server Error"),
			body: `{
						"postgresql_instance_url": "A.B.C.D",
						"postgresql_instance_port": "7432",
						"postgresql_superuser_username": "postgres",
						"postgresql_superuser_password": "Progress123",
						"postgresql_dbuser_username": "postgres",
						"postgresql_dbuser_password": "Progress123",
						"postgresql_root_cert": "----- BEGIN CERTIFICATE ------"
					}`,
		},
	}
	ExternalPostgresqlEndpoint := "/api/v1/checks/external-postgresql"

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			app, _ := SetupExternalPostgresqlHandlers(SetupMockExternalPostgresqlService(test.responseBody, test.expectedError))
			req := httptest.NewRequest("POST", ExternalPostgresqlEndpoint, strings.NewReader(test.body))
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			t.Log(body, "body")
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
