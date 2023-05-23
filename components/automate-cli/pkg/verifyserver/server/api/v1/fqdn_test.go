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
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/fqdnservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockFqdnService() fqdnservice.IFqdnService {
	return &fqdnservice.MockFqdnService{
		CheckFqdnReachabilityFunc: func(req models.FqdnRequest, port string) models.FqdnResponse {
			return models.FqdnResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        true,
						SuccessMsg:    constants.FQDN_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.NODE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			}
		},
	}
}

func SetupFqdnHandlers(fq fqdnservice.IFqdnService) (*fiber.App, error) {
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
		AddFqdnService(fq)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup(false)
	return vs.App, nil
}

func TestCheckFqdn(t *testing.T) {
	tests := []struct {
		TestName     string
		RequestBody  string
		ExpectedCode int
		ExpectedBody string
	}{
		{
			TestName: "200:success",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "chef-infra-server",
				"root_cert": "-----BEGIN CERTIFICATE-----\nMIIE2TCCA8GgAwIBAgIJAK/tIWfh4wDxMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD\nVQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx\nFjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls\nb2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz\nMDUyMzA2NDY0NFoXDTMzMDUyMDA2NDY0NFowgZAxCzAJBgNVBAYTAklOMRIwEAYD\nVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv\nZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G\nCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggEiMA0GCSqGSIb3DQEBAQUA\nA4IBDwAwggEKAoIBAQDjU96JdUaSdNlRayVK9Fbs/Fu5cRaKe+WQZZDiHdIT1gtl\nITx2VYlPnQno6R3p5uCjKX1le8hJR9YrafaolIQrMnuHg6JkSB8DeahMutWufvGI\nOZu3USs1L0sd5FCq6Y5Yr0mNkQiNNNUhjFMZYvXDd9kps8olmOpR6Jwvc+UuP637\noh0zEc9OoKHicQ+SeoD7R8JbUmzy6okNWYJ5utLoVEunSkmb24NgNmkfJ5MAQcBe\nACKj905Vi8Z3Hpb5zXVxW84scXxN7ZxxRykE1Cc254iCNKrQxW+QkO1FxshOVUi5\n94E4aY+rk9FMnE9d7Hc3io3uhcTg8TbGiYwnJDwrAgMBAAGjggEyMIIBLjAdBgNV\nHQ4EFgQUXZ9TWHc3/ICalciFP2O/kUcqRX8wgcUGA1UdIwSBvTCBuoAUXZ9TWHc3\n/ICalciFP2O/kUcqRX+hgZakgZMwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlL\nYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3Mt\ndGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3\nDQEJARYRdGVzdEBwcm9ncmVzcy5jb22CCQCv7SFn4eMA8TAMBgNVHRMEBTADAQH/\nMAsGA1UdDwQEAwIC/DAUBgNVHREEDTALgglsb2NhbGhvc3QwFAYDVR0SBA0wC4IJ\nbG9jYWxob3N0MA0GCSqGSIb3DQEBCwUAA4IBAQBAYHmcpWQf8U5OfzuLWM6M2Xt6\nGxBjWAWilF4aZG/6f38fciRxHGQcXWZp1RrgHIGCjy6VpUTRyqr5ZneSZ0JxU5X8\nSe8KOg5dyu1BdzS7DdR9Ofs80EX85eoXW8KAHeWjLprN1r9NXQ7gRaFXXkFYYkXY\nDLt/b+QavfN/FZGTi/e5HlKONoNu6FxRdtuWE8d7PwjFm+Qm3nlX1qqvRyZP3gmK\nVm7y9nw2ib8nIGcH/2EKulEyDWxvHTTBjuQXytHU+oubDz3a1eFr5IkgLwusYn1P\n2QTvRmA7J/8Qj/wMN/W5ve4akoHzS1Zzfiphq5rSh+WrhUSWpL3bKzVrGEpT\n-----END CERTIFICATE-----\n",
				"is_after_deployment": false,
				"api_token": "WFlC7Q2sucYRg7IjCSKaDJV4kYE=",
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 200,
			ExpectedBody: `{
				"status": "SUCCESS",
				"result": {
					"passed": true,
					"checks": [
						{
							"title": "FQDN is reachable",
							"passed": true,
							"success_msg": "FQDN is reachable",
							"error_msg": "",
							"resolution_msg": ""
						},
						{
							"title": "Nodes are reachable",
							"passed": true,
							"success_msg": "All nodes are reachable",
							"error_msg": "",
							"resolution_msg": ""
						},
						{
							"title": "Certificate validity for FQDN",
							"passed": true,
							"success_msg": "FQDN has with valid certificates",
							"error_msg": "",
							"resolution_msg": ""
						}
					]
				}
			}`,
		},
		{
			TestName: "400: Failure - FQDN is empty",
			RequestBody: `{
				"fqdn": "",
				"node_type": "chef-infra-server",
				"root_cert": "-----BEGIN CERTIFICATE-----\nMIIE2TCCA8GgAwIBAgIJAK/tIWfh4wDxMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD\nVQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx\nFjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls\nb2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz\nMDUyMzA2NDY0NFoXDTMzMDUyMDA2NDY0NFowgZAxCzAJBgNVBAYTAklOMRIwEAYD\nVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv\nZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G\nCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggEiMA0GCSqGSIb3DQEBAQUA\nA4IBDwAwggEKAoIBAQDjU96JdUaSdNlRayVK9Fbs/Fu5cRaKe+WQZZDiHdIT1gtl\nITx2VYlPnQno6R3p5uCjKX1le8hJR9YrafaolIQrMnuHg6JkSB8DeahMutWufvGI\nOZu3USs1L0sd5FCq6Y5Yr0mNkQiNNNUhjFMZYvXDd9kps8olmOpR6Jwvc+UuP637\noh0zEc9OoKHicQ+SeoD7R8JbUmzy6okNWYJ5utLoVEunSkmb24NgNmkfJ5MAQcBe\nACKj905Vi8Z3Hpb5zXVxW84scXxN7ZxxRykE1Cc254iCNKrQxW+QkO1FxshOVUi5\n94E4aY+rk9FMnE9d7Hc3io3uhcTg8TbGiYwnJDwrAgMBAAGjggEyMIIBLjAdBgNV\nHQ4EFgQUXZ9TWHc3/ICalciFP2O/kUcqRX8wgcUGA1UdIwSBvTCBuoAUXZ9TWHc3\n/ICalciFP2O/kUcqRX+hgZakgZMwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlL\nYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3Mt\ndGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3\nDQEJARYRdGVzdEBwcm9ncmVzcy5jb22CCQCv7SFn4eMA8TAMBgNVHRMEBTADAQH/\nMAsGA1UdDwQEAwIC/DAUBgNVHREEDTALgglsb2NhbGhvc3QwFAYDVR0SBA0wC4IJ\nbG9jYWxob3N0MA0GCSqGSIb3DQEBCwUAA4IBAQBAYHmcpWQf8U5OfzuLWM6M2Xt6\nGxBjWAWilF4aZG/6f38fciRxHGQcXWZp1RrgHIGCjy6VpUTRyqr5ZneSZ0JxU5X8\nSe8KOg5dyu1BdzS7DdR9Ofs80EX85eoXW8KAHeWjLprN1r9NXQ7gRaFXXkFYYkXY\nDLt/b+QavfN/FZGTi/e5HlKONoNu6FxRdtuWE8d7PwjFm+Qm3nlX1qqvRyZP3gmK\nVm7y9nw2ib8nIGcH/2EKulEyDWxvHTTBjuQXytHU+oubDz3a1eFr5IkgLwusYn1P\n2QTvRmA7J/8Qj/wMN/W5ve4akoHzS1Zzfiphq5rSh+WrhUSWpL3bKzVrGEpT\n-----END CERTIFICATE-----\n",
				"is_after_deployment": false,
				"api_token": "WFlC7Q2sucYRg7IjCSKaDJV4kYE=",
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Fqdn, Root Cert and Nodes can't be empty, Please provide all the required fields."
				}
			}`,
		},
		{
			TestName: "400: Failure - API token is empty",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "chef-infra-server",
				"root_cert": "-----BEGIN CERTIFICATE-----\nMIIE2TCCA8GgAwIBAgIJAK/tIWfh4wDxMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD\nVQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx\nFjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls\nb2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz\nMDUyMzA2NDY0NFoXDTMzMDUyMDA2NDY0NFowgZAxCzAJBgNVBAYTAklOMRIwEAYD\nVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv\nZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G\nCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggEiMA0GCSqGSIb3DQEBAQUA\nA4IBDwAwggEKAoIBAQDjU96JdUaSdNlRayVK9Fbs/Fu5cRaKe+WQZZDiHdIT1gtl\nITx2VYlPnQno6R3p5uCjKX1le8hJR9YrafaolIQrMnuHg6JkSB8DeahMutWufvGI\nOZu3USs1L0sd5FCq6Y5Yr0mNkQiNNNUhjFMZYvXDd9kps8olmOpR6Jwvc+UuP637\noh0zEc9OoKHicQ+SeoD7R8JbUmzy6okNWYJ5utLoVEunSkmb24NgNmkfJ5MAQcBe\nACKj905Vi8Z3Hpb5zXVxW84scXxN7ZxxRykE1Cc254iCNKrQxW+QkO1FxshOVUi5\n94E4aY+rk9FMnE9d7Hc3io3uhcTg8TbGiYwnJDwrAgMBAAGjggEyMIIBLjAdBgNV\nHQ4EFgQUXZ9TWHc3/ICalciFP2O/kUcqRX8wgcUGA1UdIwSBvTCBuoAUXZ9TWHc3\n/ICalciFP2O/kUcqRX+hgZakgZMwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlL\nYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3Mt\ndGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3\nDQEJARYRdGVzdEBwcm9ncmVzcy5jb22CCQCv7SFn4eMA8TAMBgNVHRMEBTADAQH/\nMAsGA1UdDwQEAwIC/DAUBgNVHREEDTALgglsb2NhbGhvc3QwFAYDVR0SBA0wC4IJ\nbG9jYWxob3N0MA0GCSqGSIb3DQEBCwUAA4IBAQBAYHmcpWQf8U5OfzuLWM6M2Xt6\nGxBjWAWilF4aZG/6f38fciRxHGQcXWZp1RrgHIGCjy6VpUTRyqr5ZneSZ0JxU5X8\nSe8KOg5dyu1BdzS7DdR9Ofs80EX85eoXW8KAHeWjLprN1r9NXQ7gRaFXXkFYYkXY\nDLt/b+QavfN/FZGTi/e5HlKONoNu6FxRdtuWE8d7PwjFm+Qm3nlX1qqvRyZP3gmK\nVm7y9nw2ib8nIGcH/2EKulEyDWxvHTTBjuQXytHU+oubDz3a1eFr5IkgLwusYn1P\n2QTvRmA7J/8Qj/wMN/W5ve4akoHzS1Zzfiphq5rSh+WrhUSWpL3bKzVrGEpT\n-----END CERTIFICATE-----\n",
				"is_after_deployment": true,
				"api_token": "",
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Api Token can't be empty, it is needed for checking the Automate status."
				}
			}`,
		},
		{
			TestName: "400: Failure - Node Type is empty",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "",
				"root_cert": "-----BEGIN CERTIFICATE-----\nMIIE2TCCA8GgAwIBAgIJAK/tIWfh4wDxMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD\nVQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx\nFjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls\nb2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz\nMDUyMzA2NDY0NFoXDTMzMDUyMDA2NDY0NFowgZAxCzAJBgNVBAYTAklOMRIwEAYD\nVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv\nZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G\nCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggEiMA0GCSqGSIb3DQEBAQUA\nA4IBDwAwggEKAoIBAQDjU96JdUaSdNlRayVK9Fbs/Fu5cRaKe+WQZZDiHdIT1gtl\nITx2VYlPnQno6R3p5uCjKX1le8hJR9YrafaolIQrMnuHg6JkSB8DeahMutWufvGI\nOZu3USs1L0sd5FCq6Y5Yr0mNkQiNNNUhjFMZYvXDd9kps8olmOpR6Jwvc+UuP637\noh0zEc9OoKHicQ+SeoD7R8JbUmzy6okNWYJ5utLoVEunSkmb24NgNmkfJ5MAQcBe\nACKj905Vi8Z3Hpb5zXVxW84scXxN7ZxxRykE1Cc254iCNKrQxW+QkO1FxshOVUi5\n94E4aY+rk9FMnE9d7Hc3io3uhcTg8TbGiYwnJDwrAgMBAAGjggEyMIIBLjAdBgNV\nHQ4EFgQUXZ9TWHc3/ICalciFP2O/kUcqRX8wgcUGA1UdIwSBvTCBuoAUXZ9TWHc3\n/ICalciFP2O/kUcqRX+hgZakgZMwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlL\nYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3Mt\ndGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3\nDQEJARYRdGVzdEBwcm9ncmVzcy5jb22CCQCv7SFn4eMA8TAMBgNVHRMEBTADAQH/\nMAsGA1UdDwQEAwIC/DAUBgNVHREEDTALgglsb2NhbGhvc3QwFAYDVR0SBA0wC4IJ\nbG9jYWxob3N0MA0GCSqGSIb3DQEBCwUAA4IBAQBAYHmcpWQf8U5OfzuLWM6M2Xt6\nGxBjWAWilF4aZG/6f38fciRxHGQcXWZp1RrgHIGCjy6VpUTRyqr5ZneSZ0JxU5X8\nSe8KOg5dyu1BdzS7DdR9Ofs80EX85eoXW8KAHeWjLprN1r9NXQ7gRaFXXkFYYkXY\nDLt/b+QavfN/FZGTi/e5HlKONoNu6FxRdtuWE8d7PwjFm+Qm3nlX1qqvRyZP3gmK\nVm7y9nw2ib8nIGcH/2EKulEyDWxvHTTBjuQXytHU+oubDz3a1eFr5IkgLwusYn1P\n2QTvRmA7J/8Qj/wMN/W5ve4akoHzS1Zzfiphq5rSh+WrhUSWpL3bKzVrGEpT\n-----END CERTIFICATE-----\n",
				"is_after_deployment": true,
				"api_token": "WFlC7Q2sucYRg7IjCSKaDJV4kYE=",
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Node Type should be automate or chef-infra-server, Please provide Node Type."
				}
			}`,
		},
		{
			TestName:     "400: Failure - Invalid Body",
			RequestBody:  "Wrong body",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid Body Request"
				}
			}`,
		},
	}

	fqdnCheckEndpoint := constants.FQDN_API_PATH
	// Setup the app as it is done in the main function
	app, err := SetupFqdnHandlers(SetupMockFqdnService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", fqdnCheckEndpoint, bodyReader)
			req.Header.Add("Content-type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
