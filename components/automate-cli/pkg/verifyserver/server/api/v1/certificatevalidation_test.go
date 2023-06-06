package v1_test

import (
	"io"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/certificatevalidation"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupCertificateHandlers(vc certificatevalidation.IValidateCertificateService) (*fiber.App, error) {
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
		AddCertificateValidation(vc)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup(false)
	return vs.App, nil
}

func SetUpMockCertificateValidationService() certificatevalidation.IValidateCertificateService {
	return &certificatevalidation.MockCertificateValidationService{
		CertificateValidationFunc: func(req models.CertificateCheckRequest) models.CertificateCheckResponse {
			return models.CertificateCheckResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_EXPIRY_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			}
		},
	}
}

func TestValidateCertificate(t *testing.T) {
	tests := []struct {
		TestName       string
		RequestBody    string
		ExpectedCode   int
		ExpectedResult string
	}{
		{
			TestName: "200:success",
			RequestBody: `{
				"root_certificate": "VALID CERTIFICATE",
				"private_key": "VALID CERTIFICATE",
				"node_certificate": "VALID CERTIFICATE",
				"admin_private_key": "VALID CERTIFICATE",
				"admin_certificate": "VALID CERTIFICATE"
			  }`,
			ExpectedCode: 200,
			ExpectedResult: `{
				"status": "SUCCESS",
				"result": {
					"passed": true,
					"checks": [
						{
							"title": "Certificates have valid expiry date",
							"passed": true,
							"success_msg": "All certificates expiry date is later than 365 days",
							"error_msg": "",
							"resolution_msg": ""
						},
						{
							"title": "Certificates are of X509 V3 format",
							"passed": true,
							"success_msg": "All certificates are of X509 V3 format",
							"error_msg": "",
							"resolution_msg": ""
						},
						{
							"title": "Private Keys are of PKCS8 format",
							"passed": true,
							"success_msg": "The private keys are of PKCS8 format",
							"error_msg": "",
							"resolution_msg": ""
						},
						{
							"title": "Certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
							"passed": true,
							"success_msg": "All certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
							"error_msg": "",
							"resolution_msg": ""
						}
					]
				}
			}`,
		},
		{
			TestName: "400: Failure Root Certificate is empty",
			RequestBody: `{
				"root_certificate": "",
				"private_key": "VALID CERTIFICATE",
				"node_certificate": "VALID CERTIFICATE",
				"admin_private_key": "VALID CERTIFICATE",
				"admin_certificate": "VALID CERTIFICATE"
			  }`,
			ExpectedCode: 400,
			ExpectedResult: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "root_certificate, private_key, node_certificate, admin_private_key and admin_certificate can't be empty, Please provide all the fields."
				}
			}`,
		},
		{
			TestName:     "400: Failure - Invalid Body",
			RequestBody:  "Wrong body",
			ExpectedCode: 400,
			ExpectedResult: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid Body Request"
				}
			}`,
		},
	}

	certificateValidationCheckEndpoint := constants.CERTIFICATE_CHECK_API_PATH
	// Setup the app as it is done in the main function
	app, err := SetupCertificateHandlers(SetUpMockCertificateValidationService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", certificateValidationCheckEndpoint, bodyReader)
			req.Header.Add(constants.CONTENT_TYPE, constants.TYPE_JSON)
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, test.ExpectedResult, string(body))
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
