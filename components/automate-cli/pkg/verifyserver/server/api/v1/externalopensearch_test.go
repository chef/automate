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
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalopensearchservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupExternalOpensearchMountHandler(eos externalopensearchservice.IExternalOpensearchService) (*fiber.App, error) {
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
		AddExternalOpensearchService(eos)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockExternalOpensearchService() externalopensearchservice.IExternalOpensearchService {
	return &externalopensearchservice.MockExternalOpensearchService{
		GetExternalOpensearchDetailsFunc: func(reqBody models.ExternalOS, port int) models.ExternalOpensearchResponse {
			return models.ExternalOpensearchResponse{
				Passed: true,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         "Connection successfully tested",
						Passed:        true,
						Status:        constants.STATUS_PASS,
						SuccessMsg:    constants.EXTERNAL_OPENSEARCH_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
						DebugMsg:      "",
					},
				},
			}
		},
	}
}

func TestExternalOpensearch(t *testing.T) {
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
				"result": {
					"passed": true,
					"checks": [
						{
							"title": "Connection successfully tested",
							"passed": true,
							"status": "PASS",
							"success_msg": "Machine is able to connect with External Managed OpenSeach",
							"error_msg": "",
							"resolution_msg": "",
							"debug_msg": ""
						}
					]
				}
			}`,
			RequestBody: `{
				"opensearch_domain_name": "managed-services-os",
				"opensearch_domain_url": "search-managed-os-7drxn7jm4edbwzqaga.eu-west-3.es.amazonaws.com",
				"opensearch_username": "admin",
				"opensearch_user_password": "admin",
				"opensearch_root_cert": "-----BEGIN CERTIFICATE-----\nMIIDQTCCAimgAwIBAgITBmyfz5m/jAo54vB4ikPmljZbyjANBgkqhkiG9w0BAQsF\nADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6\nb24gUm9vdCBDQSAxMB4XDTE1MDUyNjAwMDAwMFoXDTM4MDExNzAwMDAwMFowOTEL\nMAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJv\nb3QgQ0EgMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALJ4gHHKeNXj\nca9HgFB0fW7Y14h29Jlo91ghYPl0hAEvrAIthtOgQ3pOsqTQNroBvo3bSMgHFzZM\n9O6II8c+6zf1tRn4SWiw3te5djgdYZ6k/oI2peVKVuRF4fn9tBb6dNqcmzU5L/qw\nIFAGbHrQgLKm+a/sRxmPUDgH3KKHOVj4utWp+UhnMJbulHheb4mjUcAwhmahRWa6\nVOujw5H5SNz/0egwLX0tdHA114gk957EWW67c4cX8jJGKLhD+rcdqsq08p8kDi1L\n93FcXmn/6pUCyziKrlA4b9v7LWIbxcceVOF34GfID5yHI9Y/QCB/IIDEgEw+OyQm\njgSubJrIqg0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMC\nAYYwHQYDVR0OBBYEFIQYzIU07LwMlJQuCFmcx7IQTgoIMA0GCSqGSIb3DQEBCwUA\nA4IBAQCY8jdaQZChGsV2USggNiMOruYou6r4lK5IpDB/G/wkjUu0yKGX9rbxenDI\nU5PMCCjjmCXPI6T53iHTfIUJrU6adTrCC2qJeHZERxhlbI1Bjjt/msv0tadQ1wUs\nN+gDS63pYaACbvXy8MWy7Vu33PqUXHeeE6V/Uq2V8viTO96LXFvKWlJbYK8U90vv\no/ufQJVtMVT8QtPHRh8jrdkPSHCa2XV4cdFyQzR1bldZwgJcJmApzyMZFo6IQ6XU\n5MsI+yMRQ+hDKXJioaldXgjUkK642M4UwtBV8ob2xJNDd2ZhwLnoQdeXeGADbkpy\nrqXRfboQnoZsG4q5WTP468SQvvG5\n-----END CERTIFICATE-----\n"
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
			TestName:     "Not Given all the required Parameters",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "OSDomainName, OSDomainURL, OSUsername, OSUserPassword or OSCert cannot be empty"
				}
			}`,
			RequestBody: `{
				"opensearch_domain_name": "managed-services-os",
				"opensearch_domain_url": "search-managed-os-7drxn7jm4edbwz3dukpshdqaga.eu-west-3.es.amazonaws.com",
				"opensearch_username": "admin",
				"opensearch_user_password": "",
				"opensearch_root_cert": "-----BEGIN CERTIFICATE-----\nMIIDQTCCAimgAwIBAgITBmyfz5m/jAo54vB4ikPmljZbyjANBgkqhkiG9w0BAQsF\nADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6\nb24gUm9vdCBDQSAxMB4XDTE1MDUyNjAwMDAwMFoXDTM4MDExNzAwMDAwMFowOTEL\nMAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJv\nb3QgQ0EgMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALJ4gHHKeNXj\nca9HgFB0fW7Y14h29Jlo91ghYPl0hAEvrAIthtOgQ3pOsqTQNroBvo3bSMgHFzZM\n9O6II8c+6zf1tRn4SWiw3te5djgdYZ6k/oI2peVKVuRF4fn9tBb6dNqcmzU5L/qw\nIFAGbHrQgLKm+a/sRxmPUDgH3KKHOVj4utWp+UhnMJbulHheb4mjUcAwhmahRWa6\nVOujw5H5SNz/0egwLX0tdHA114gk957EWW67c4cX8jJGKLhD+rcdqsq08p8kDi1L\n93FcXmn/6pUCyziKrlA4b9v7LWIbxcceVOF34GfID5yHI9Y/QCB/IIDEgEw+OyQm\njgSubJrIqg0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMC\nAYYwHQYDVR0OBBYEFIQYzIU07LwMlJQuCFmcx7IQTgoIMA0GCSqGSIb3DQEBCwUA\nA4IBAQCY8jdaQZChGsV2USggNiMOruYou6r4lK5IpDB/G/wkjUu0yKGX9rbxenDI\nU5PMCCjjmCXPI6T53iHTfIUJrU6adTrCC2qJeHZERxhlbI1Bjjt/msv0tadQ1wUs\nN+gDS63pYaACbvXy8MWy7Vu33PqUXHeeE6V/Uq2V8viTO96LXFvKWlJbYK8U90vv\no/ufQJVtMVT8QtPHRh8jrdkPSHCa2XV4cdFyQzR1bldZwgJcJmApzyMZFo6IQ6XU\n5MsI+yMRQ+hDKXJioaldXgjUkK642M4UwtBV8ob2xJNDd2ZhwLnoQdeXeGADbkpy\nrqXRfboQnoZsG4q5WTP468SQvvG5\n-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----\nMIIEXjCCA0agAwIBAgITB3MSSkvL1E7HtTvq8ZSELToPoTANBgkqhkiG9w0BAQsF\nADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6\nb24gUm9vdCBDQSAxMB4XDTIyMDgyMzIyMjUzMFoXDTMwMDgyMzIyMjUzMFowPDEL\nMAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEcMBoGA1UEAxMTQW1hem9uIFJT\nQSAyMDQ4IE0wMjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALtDGMZa\nqHneKei1by6+pUPPLljTB143Si6VpEWPc6mSkFhZb/6qrkZyoHlQLbDYnI2D7hD0\nsdzEqfnuAjIsuXQLG3A8TvX6V3oFNBFVe8NlLJHvBseKY88saLwufxkZVwk74g4n\nWlNMXzla9Y5F3wwRHwMVH443xGz6UtGSZSqQ94eFx5X7Tlqt8whi8qCaKdZ5rNak\n+r9nUThOeClqFd4oXych//Rc7Y0eX1KNWHYSI1Nk31mYgiK3JvH063g+K9tHA63Z\neTgKgndlh+WI+zv7i44HepRZjA1FYwYZ9Vv/9UkC5Yz8/yU65fgjaE+wVHM4e/Yy\nC2osrPWE7gJ+dXMCAwEAAaOCAVowggFWMBIGA1UdEwEB/wQIMAYBAf8CAQAwDgYD\nVR0PAQH/BAQDAgGGMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAdBgNV\nHQ4EFgQUwDFSzVpQw4J8dHHOy+mc+XrrguIwHwYDVR0jBBgwFoAUhBjMhTTsvAyU\nlC4IWZzHshBOCggwewYIKwYBBQUHAQEEbzBtMC8GCCsGAQUFBzABhiNodHRwOi8v\nb2NzcC5yb290Y2ExLmFtYXpvbnRydXN0LmNvbTA6BggrBgEFBQcwAoYuaHR0cDov\nL2NydC5yb290Y2ExLmFtYXpvbnRydXN0LmNvbS9yb290Y2ExLmNlcjA/BgNVHR8E\nODA2MDSgMqAwhi5odHRwOi8vY3JsLnJvb3RjYTEuYW1hem9udHJ1c3QuY29tL3Jv\nb3RjYTEuY3JsMBMGA1UdIAQMMAowCAYGZ4EMAQIBMA0GCSqGSIb3DQEBCwUAA4IB\nAQAtTi6Fs0Azfi+iwm7jrz+CSxHH+uHl7Law3MQSXVtR8RV53PtR6r/6gNpqlzdo\nZq4FKbADi1v9Bun8RY8D51uedRfjsbeodizeBB8nXmeyD33Ep7VATj4ozcd31YFV\nfgRhvTSxNrrTlNpWkUk0m3BMPv8sg381HhA6uEYokE5q9uws/3YkKqRiEz3TsaWm\nJqIRZhMbgAfp7O7FUwFIb7UIspogZSKxPIWJpxiPo3TcBambbVtQOcNRWz5qCQdD\nslI2yayq0n2TXoHyNCLEH8rpsJRVILFsg0jc7BaFrMnF462+ajSehgj12IidNeRN\n4zl+EoNaWdpnWndvSpAEkq2P\n-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----\nMIIF4zCCBMugAwIBAgIQAgdYBohcBa6gZ8J46ZrGOTANBgkqhkiG9w0BAQsFADA8\nMQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRwwGgYDVQQDExNBbWF6b24g\nUlNBIDIwNDggTTAyMB4XDTIzMDQwNDAwMDAwMFoXDTIzMDcxMDIzNTk1OVowJzEl\nMCMGA1UEAwwcKi5ldS13ZXN0LTMuZXMuYW1hem9uYXdzLmNvbTCCASIwDQYJKoZI\nhvcNAQEBBQADggEPADCCAQoCggEBAItfKDa/tuHppT8hKBel0gWz+hgfUKBB8Ktz\nk8LWWOVYRO8KTrRx+93tLmoZUng0TWmMDVu/7kiCftXk4tIgDM0Mh2Xgit08DCRz\nRZutvOLng0/bo+cJX+sz1SI5TRFOAHoZfvndkvIC0HuEHQbf0NfcR5qgswYG+//4\nJs7bS2dZmxMvb//XZuGr+LJLtbcPstHW91Efs17l62uRjFEiCqVoRcu/wNCJLrJN\nSzbTVliAs8VPcKyHc5PqUeggvT9qfQ6V6J7/mzY6IucmjxWjE/JUo5L0qz0eB8KR\nd78dFMqn29ot8wskvh+v9W2nYFCaq2C16n839+zbYzhySFT6IusCAwEAAaOCAvQw\nggLwMB8GA1UdIwQYMBaAFMAxUs1aUMOCfHRxzsvpnPl664LiMB0GA1UdDgQWBBRX\n77AKXv+y8lzFAAJRhoxvn4Av2jAnBgNVHREEIDAeghwqLmV1LXdlc3QtMy5lcy5h\nbWF6b25hd3MuY29tMA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcD\nAQYIKwYBBQUHAwIwOwYDVR0fBDQwMjAwoC6gLIYqaHR0cDovL2NybC5yMm0wMi5h\nbWF6b250cnVzdC5jb20vcjJtMDIuY3JsMBMGA1UdIAQMMAowCAYGZ4EMAQIBMHUG\nCCsGAQUFBwEBBGkwZzAtBggrBgEFBQcwAYYhaHR0cDovL29jc3AucjJtMDIuYW1h\nem9udHJ1c3QuY29tMDYGCCsGAQUFBzAChipodHRwOi8vY3J0LnIybTAyLmFtYXpv\nbnRydXN0LmNvbS9yMm0wMi5jZXIwDAYDVR0TAQH/BAIwADCCAX0GCisGAQQB1nkC\nBAIEggFtBIIBaQFnAHYA6D7Q2j71BjUy51covIlryQPTy9ERa+zraeF3fW0GvW4A\nAAGHTa9b1QAABAMARzBFAiBdJJnMEF2u4brz5n/npKmJpIUmZo1nq6Wkvqt5HOlT\newIhAKtH5FmwNm9SERPL4gjW00HG7h5rAeHMIVhLySHnNef7AHYAs3N3B+GEUPhj\nhtYFqdwRCUp5LbFnDAuH3PADDnk2pZoAAAGHTa9cQQAABAMARzBFAiA+78yUPvFk\nAIFd+dx5PLUuYKqzFwK0c1qbPdFS3zOrrgIhAILuL/cb1U2uIGVTMRK5RDVk2ocO\nMBLUyHSTdwpOUsjBAHUAejKMVNi3LbYg6jjgUh7phBZwMhOFTTvSK8E6V6NS61IA\nAAGHTa9b5gAABAMARjBEAiBRGaDlWve2iE8SQ1OTPGZE9T2uPjr0PT0NSejH+rZc\n1QIgCIe9ayrU/1+DvSIY0kV4oU2Rwt/iu2PsurvLY+WmDhkwDQYJKoZIhvcNAQEL\nBQADggEBABoXupjeMCM8WQAamj0SChBVMlxGu8xaPPR6fVkz5d1+zuTrrOMSyE7a\n9hTAjeHn4WRvXNRpZtEXBa31x2o3DoON3xpz+iTM+ACt5lpdixEJSkJQFcb7DfRC\nqS9fpTUExm85O/+wUtm3+enTnTbapJVMuAHa53ViAT2B65zzhGZ2AwPEgaBLI/Vb\nG6VtmK8Yv+bEPd2f1wKopy+sG+4fZGKMwhJJoblgNUmRSc+AEqi/bXYh8ry8xhww\nepP23yqKzXO4cZQ1tuLp6kXc9aRpPyKDvEy6c6eHTzkY3RLlqCcSx+nrzYRC525s\n3+CYlObA/CaUdzh4saR8Ftv2+v0xqmQ=\n-----END CERTIFICATE-----\n"
			  }`,
		},
	}

	ExternalOpensearchEndpoint := constants.EXTERNAL_OPENSEARCH_API_PATH

	app, err := SetupExternalOpensearchMountHandler(SetupMockExternalOpensearchService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", ExternalOpensearchEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body) //nosemgrep
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
