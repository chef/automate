package externalopensearchservice_test

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"
	"strconv"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalopensearchservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

const (
	LOCALHOST  = "localhost:3073"
	SERVER_CRT = `-----BEGIN CERTIFICATE-----
MIIGrzCCBJegAwIBAgIJAMgNHiWQ140JMA0GCSqGSIb3DQEBCwUAMGwxCzAJBgNV
BAYTAklOMRIwEAYDVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEW
MBQGA1UECgwNUHJvZ3Jlc3MtdGVzdDEMMAoGA1UECwwDT3JnMQ8wDQYDVQQDDAZS
b290Q0EwHhcNMjMwNTIyMTIwNDQzWhcNMjgwNTIwMTIwNDQzWjBsMQswCQYDVQQG
EwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUxFjAU
BgNVBAoMDVByb2dyZXNzLXRlc3QxDDAKBgNVBAsMA09yZzEPMA0GA1UEAwwGdWJ1
bnR1MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAqI944EjDaCFH2IBJ
qh3y/xnG3Lxn9SLst6THeX5+NmjWg/OsBYnIAUzH79RjkCAzdkq9GiK5C4QF3h0a
mq9boD3+RPPd00rZfRYI47eDo57QgMQ6fcpji2OHud0oXTPMoDTIBG9K7vV7K0ab
VS8F/t9aNd9SVAZXhBxrBTzvdtj3xXN9TZbVgvh+ATUflsqhSWERUoYAeUP5E1nG
d+l3A9SDPWezGC10GAM0U3cUh6a/FhwrAU8p/n96fsE6/vmaKEt/8i7wtwpZU2Z0
LmOnkiDQHBh2jh7oDnGbzWR4cQ/bnwq01vazZVEHSM/zZATQRwZxFFWUOokowP/8
w14xK2yxdbrR1xSTlr4W+AwPBKIZ9DBolWCA2HwcJvra6YKEyB8VRitvQS0YQSCj
rrOYzqJTcgfgLOSD5Be+xbcr7UpyqRliKZG1sypew8qUEGgLEpmTngreWn0Vv98A
NrWKiZjYbZrD58gEm2IzEgMJIiXgO7qiSbrWqQy7seSuZnG3It9V9iVCKeLUp1dY
8mBRIHH8BIUQ5AiJ9bATOaal9+LctcRG8JMrDmVlMLovcBY6nvlcDcE+/nNzepbo
uWjRETtxPb3PV82Qtz3aLze5U71LI8GE0+zuuTCeN0q5dz55f/nQjulUJl9MrJe7
hCun/yNlIxnNxCfEPAAywe+Rp9kCAwEAAaOCAVIwggFOMAkGA1UdEwQCMAAwEQYJ
YIZIAYb4QgEBBAQDAgZAMDMGCWCGSAGG+EIBDQQmFiRPcGVuU1NMIEdlbmVyYXRl
ZCBTZXJ2ZXIgQ2VydGlmaWNhdGUwHQYDVR0OBBYEFK5psaHGnEz3F7LmBiqW52Tu
3RYcMIGGBgNVHSMEfzB9oXCkbjBsMQswCQYDVQQGEwJJTjESMBAGA1UECAwJS2Fy
bmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUxFjAUBgNVBAoMDVByb2dyZXNzLXRl
c3QxDDAKBgNVBAsMA09yZzEPMA0GA1UEAwwGUm9vdENBggkAopCmcMizeIAwDgYD
VR0PAQH/BAQDAgWgMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAiBgNV
HREEGzAZhwSsFAoCggZ1YnVudHWCCWxvY2FsaG9zdDANBgkqhkiG9w0BAQsFAAOC
AgEAT0mLqpQHeMjiLg+meuk6izT/MiygAzr8IsM8K/wJu/lbvNF11SvFaenQK/OV
7+Bj98noMOhk4haIXkljqWYP9VY+yWtvHxRR3MJWISfNVjtaJIdMQeMGBE9E2ac4
4bwltDfuaDaYmdVZbvzg5tY9rgqbo1G7giJjqsjh0NUXDoImCeYNWJ2kcWaQgfwI
Yv2a+PWvX6oYGOmuQ5c4sgQJAZ0Z4mX9ez8Xva/LeL7mcrDlMFrlfp4kXd7nVpwX
ly3a0Ggtakc/cT5va2MIbyyaOf1quZXGFcS/yXI5Amhn2jj3xjYDafwH+azCT2h6
Z/A7cx1xts684cBQGluQsfQ9HuRwU9Hdlth1SkyFTaPY2p9yOaYp5vFM6bdKNzJc
gyizqFEv7cJ+TyeQwWMnro0gPLni3WM7nunp3fqFK251v7+7belFWVZhsfB8kTgt
pLjACQ+xfFDIanQpS+01pm7d4UQtXSopaPYr49lVp5KMQiRVw99Me/lFFckgLHDe
jUXeA04ND3FmYrhdanMuvYYL5jBIi9BjBeQpf2ZwSXey7+yeK4joxUIZlppjQGog
zGG2TLikVipjoyhYRlZFhzdTuICCJ2unKmNIMAZLorrMkf/g5B0hE1pxUk9giQew
StiaQ+XAUfnGHhcy1ubOOCO2LT0c/hKTlscCmYPsAJh1lk8=
-----END CERTIFICATE-----`
	SERVER_KEY = `-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----`
	CA_CERT = `-----BEGIN CERTIFICATE-----
MIIFVDCCAzwCCQCikKZwyLN4gDANBgkqhkiG9w0BAQsFADBsMQswCQYDVQQGEwJJ
TjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUxFjAUBgNV
BAoMDVByb2dyZXNzLXRlc3QxDDAKBgNVBAsMA09yZzEPMA0GA1UEAwwGUm9vdENB
MB4XDTIzMDUyMjEyMDEzNFoXDTI4MDUyMDEyMDEzNFowbDELMAkGA1UEBhMCSU4x
EjAQBgNVBAgMCUthcm5hdGFrYTESMBAGA1UEBwwJQmVuZ2FsdXJ1MRYwFAYDVQQK
DA1Qcm9ncmVzcy10ZXN0MQwwCgYDVQQLDANPcmcxDzANBgNVBAMMBlJvb3RDQTCC
AiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBANTskS0D2anCGLiQhL73aGj3
3HRxkYCxKKrv/yN7jldSDYFruXyGFuAmgDB4Lvk9pJz/V5b1Ke3hU1tmTsrrBdhE
c0H6JpM5OmPWpwpcW19oXCqmOWjhopEDV5DxzyXbDGY+b0w0M/BTigyAVdH+QRV7
PZPJsqoeHEPvIZ5uamkml6fg3krhvRKzshPdfMBb9n88O5t785YxUxcpAe0rQcoh
3jK95zJWd43mpgi0XGtCfhjx3BBgi0zBXAx674iQPi/2Ljy7/i1KboBdN3b0b4oZ
Gt+rVJgZSFhsk+Opm6DZYoi0G5TRE1mlSXH1P1ANniO6vbG4ylHWr6JJifMso2oU
PMeN0vDPPjLrdIRUdVxQ1kEkkG5u+PeWNgoa86bgc6suP6V/3SgMn6Lu+MchjOqi
sbmvblaQY0gcMuoovxcHjlGF7pyb1WrrsLpLgDVteAzku2QLz4JiyTmsb05kazJT
2oyH5le4bePUGY2fTbGzhHaFDaxKE7Bnj5OfSZZWx6Q3RGmYKuMOHkAcDxDiAKmL
+52oCa4TA1FYHME8JXpTVDmVdlwDGiZh5wT2v8QF8cxIHzL6Oh8C7waqNuxU8WSr
AYU/0YTU1XpcgmB3xUpPa4PSubWcCFFrqw2NnPb75KqG7QIujAgoasrbBR44EQHP
QDJHzNn+i+odTM4x3zpBAgMBAAEwDQYJKoZIhvcNAQELBQADggIBAIY7ucgzNtwR
TZR5x6afTWeRST0L/can076PSoBowq1vqDQw0Me9mCYj5pofS5e8nTcZc4REzS8S
2gMAgjYkhvXgaofIl62DKgnwvyZ6MJFI3/zb3SpKHEFoSUee0dT//amKvh33ilJU
74VtyTxaV0zj1Z5X1C6l+lGcO8PkoUWg+0y/3RDiZGg2B21IHi32cPHS0XHD4iW4
Tm+LcNmOiQzAPdOd+7kWpaRg9lX/9osGueIRy/Jy0t3RLmRiH4DefubI7Ya6vb1R
Ewniu1foX+97JJjQeDhBrD8a0vWEtpRAbp1oYBHVT39wJzMe8K4uUYT6G9f7FEWO
llwh/4CahbQKjjlGLuPXY0GYkkBxpIDE3IhwAXqBGLxWO/MmGuPDpgQK3vmY6htU
i/vipc8zHWFRaro1mL2sL+tZOk6zIJkA7YO2vux0DyDA+TTpyiooWfNLnDJ+TeBB
sP1Xn6H6U/++nPEP6ZHGu4CGRrJPkCbrc3AD8XMQrJqNt1MbpwqSJCT6dlW2tCrP
D+YLStm+V/xzmcSdd7h9zfZstJGXQmcAqoStfdBuRH1u3Wu97ory4+e0OsqkhNhS
gAvv7gUqzOym7L1rQqaJo3h2oBmE9OVmyGVyyrAhd5VwH+ve+cEyOwXUqSH4CTlT
cDO0rxyp16R84JLEj2vtqK/E5tp8Ew52
-----END CERTIFICATE-----`
	DIFF_SERVER_ROOT_CA = `-----BEGIN CERTIFICATE-----
MIIFojCCA4oCCQCrZnj0fDgwXTANBgkqhkiG9w0BAQsFADCBkjELMAkGA1UEBhMC
SU4xEjAQBgNVBAgMCUtBUk5BVEFLQTESMBAGA1UEBwwJQkVOR0FMVVJVMRYwFAYD
VQQKDA1Qcm9ncmVzcy10ZXN0MQswCQYDVQQLDAJJVDEUMBIGA1UEAwwLZXhhbXBs
ZS5jb20xIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIzMDUy
MzExNTQwMloXDTI4MDUyMDExNTQwMlowgZIxCzAJBgNVBAYTAklOMRIwEAYDVQQI
DAlLQVJOQVRBS0ExEjAQBgNVBAcMCUJFTkdBTFVSVTEWMBQGA1UECgwNUHJvZ3Jl
c3MtdGVzdDELMAkGA1UECwwCSVQxFDASBgNVBAMMC2V4YW1wbGUuY29tMSAwHgYJ
KoZIhvcNAQkBFhF0ZXN0QHByb2dyZXNzLmNvbTCCAiIwDQYJKoZIhvcNAQEBBQAD
ggIPADCCAgoCggIBANRXdcgEe7fXCmr3YRh/xM9r5Sr8FZtzE4NjRAxCbV1S+eTa
Z1DMidTImm0y70J02JSxuumpvxRcEtrjZzaoZGZTSm5Cy4YBwHv/vtbREaPI6uoO
+3iV2ykwmSrQHgvvkVb50zpAfHjc8P6aNcDgN0N8XWjLpx9E4P/K4FoE0xgfuMEj
jBvObpB/OxBtDrbqU2OIKiEVhbMITD2mIIkweTOODeejK1xVxJJurG+4Y6MTrwSJ
Wd4N1WQcyr5WjwLLPjWTV2I3Ng9Z6CvHaVL2/i442NK6pD6nGLQ8NuOoawcAAIgR
lkCm6E18uHoCoR8q71O7p2fqTiJF2RJVNBsWZbhp8upGUrs6vuTXZEj10OCwGvbY
riLbIDrKQbTUbf0mKmktKTc+9r0dO+U0E0k5J0vd8Y1yZoOSD+iLB+uASpN4v+BZ
hy9IQFgdtWFbuBbZV3oQ8q4ui8Xq5sNbgeVKoXPgJZ42kCubXffOq2u8jgGqtNHC
iEMdXOeaN6CofVhUEBYRLUwxwJEi8PrXCNK3Icl7JcGEUT0V0OMpPV9aoD7NFNtZ
lO/bXCmDSYGCM3W3vAcQKXtuV6L0lETt4WNap2MnifOAb401efFy9XBCytUQ7uQz
YXTVLaGJthidyTK7JghMo8BLz3Cyxath59frANqPLtnbCRValsN6tz3DQqdtAgMB
AAEwDQYJKoZIhvcNAQELBQADggIBAFAejo9UNuI4y+/OHLtWHz0ZJ0eeGLxCq/sh
Wv3EohH2tTgb6ls/CmNlxvhqJV6/rNE6n/eWdEa/8ONi0EqSD12OowogxkHDxV2m
mLcSYpEjRJovEZze13ryR4YguAhy5Cukc5GautdUKXJYgKZleZXGRLTkL60opkhp
73zKiQIhV04a4DDY0HxgSAqYN9do8H0bkn3XsRfjLTmorwoGGWvz0TVCv3QWWlgb
zOmPIZ5xKG3iniTWXQXGACTDUyIjO7DenZZAIJd3n6KR7HZ0cufzVSsRrnTnETJu
KG0HiRai9wsJyDusTwmd1gbrnBsHxBQp4ylq/ojufLqwaUwFpJz41A/2KkRdqz7g
Fi6NZQIIEfk8pp+0pEvlNy0a23aMYz8wXzTUkeqUpfmmt9DPC20bYjC9lnrTiSjb
NOLostbtejT6iggKSU0I17L+LN7tgpQgcCk0MojRWFBmlm34sHhbNxtuOwVdKh1f
b2qf3Jo1n6RB+DdU96tYxn6p7HF/0dCZ6S74GirKmZMAKbdap/d4mgTszHvVHcWt
ek2LMF/JQQKIXMNKJbo+F1cipClfBVBgTDOcLAoQ1x/SmR9O2RNHD01dq+GW537R
qUZKHRSxo/0WgcntahccsLc3wXA2pWyMXrhcggnyOnEp2XgnVzHPVof1PoG6XWf9
VZDL9oxn
-----END CERTIFICATE-----`
	WRONG_ROOT_CA = `-----BEGIN CERTIFICATE-----
abcd
-----END CERTIFICATE-----`
	TIMEOUT = 1
)

func TestPortReachableDetails(t *testing.T) {
	eos := externalopensearchservice.NewExternalOpensearchService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, eos)
}

func startHTTPSMockServerOnCustomPort(mockServer *httptest.Server, port string) error {
	cert, err := tls.X509KeyPair([]byte(SERVER_CRT), []byte(SERVER_KEY))
	if err != nil {
		return fmt.Errorf("Failed to load certificate and private key: %s", err)
	}
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(CA_CERT))

	// Create a TLS configuration with the certificate and certificate pool
	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		RootCAs:      caCertPool,
	}

	l, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%s", port))
	if err != nil {
		return err
	}
	mockServer.TLS = tlsConfig
	mockServer.Listener = l
	mockServer.StartTLS()
	return nil
}

func TestGetExternalOpensearchDetails(t *testing.T) {
	httpsport := 3073
	httpsMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		user, pass, ok := r.BasicAuth()
		if !ok || user != "admin" || pass != "admin" {
			w.Header().Set("WWW-Authenticate", `Basic realm="Authorization required"`)
			w.WriteHeader(http.StatusUnauthorized)
			fmt.Fprintln(w, "Unauthorized")
			return
		}
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	}))
	err := startHTTPSMockServerOnCustomPort(httpsMockServer, strconv.Itoa(httpsport))
	assert.NoError(t, err)
	defer httpsMockServer.Close()

	eos := externalopensearchservice.NewExternalOpensearchService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, eos)
	tests := []struct {
		TestName     string
		ReqBody      models.ExternalOSRequest
		ResponseBody models.ExternalOpensearchResponse
	}{
		{
			TestName: "Correct url, username, password and root_ca",
			ReqBody: models.ExternalOSRequest{
				OSDomainName:   "Test",
				OSDomainURL:    LOCALHOST,
				OSUsername:     "admin",
				OSUserPassword: "admin",
				OSCert:         CA_CERT,
			},
			ResponseBody: models.ExternalOpensearchResponse{
				Passed: true,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         constants.EXTERNAL_OPENSEARCH_SUCCESS_TITLE,
						Passed:        true,
						Status:        constants.STATUS_PASS,
						SuccessMsg:    constants.EXTERNAL_OPENSEARCH_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
						DebugMsg:      "",
					},
				},
			},
		},
		{
			TestName: "Correct url, username and password but wrong root_ca",
			ReqBody: models.ExternalOSRequest{
				OSDomainName:   "Test",
				OSDomainURL:    LOCALHOST,
				OSUsername:     "admin",
				OSUserPassword: "admin",
				OSCert:         WRONG_ROOT_CA,
			},
			ResponseBody: models.ExternalOpensearchResponse{
				Passed: false,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         constants.EXTERNAL_OPENSEARCH_FAILED_TITLE,
						Passed:        false,
						Status:        constants.STATUS_FAIL,
						SuccessMsg:    "",
						ErrorMsg:      constants.EXTERNAL_OPENSEARCH_ERROR_MSG,
						ResolutionMsg: constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG,
						DebugMsg:      "Failed to connect to OpenSearch",
					},
				},
			},
		},
		{
			TestName: "Correct url, username and password but different server root_ca",
			ReqBody: models.ExternalOSRequest{
				OSDomainName:   "Test",
				OSDomainURL:    LOCALHOST,
				OSUsername:     "admin",
				OSUserPassword: "admin",
				OSCert:         DIFF_SERVER_ROOT_CA,
			},
			ResponseBody: models.ExternalOpensearchResponse{
				Passed: false,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         constants.EXTERNAL_OPENSEARCH_FAILED_TITLE,
						Passed:        false,
						Status:        constants.STATUS_FAIL,
						SuccessMsg:    "",
						ErrorMsg:      constants.EXTERNAL_OPENSEARCH_ERROR_MSG,
						ResolutionMsg: constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG,
						DebugMsg:      "Failed to connect to OpenSearch",
					},
				},
			},
		},
		{
			TestName: "Correct root_ca and url but wrong username and password",
			ReqBody: models.ExternalOSRequest{
				OSDomainName:   "Test",
				OSDomainURL:    LOCALHOST,
				OSUsername:     "admin",
				OSUserPassword: "admin2",
				OSCert:         CA_CERT,
			},
			ResponseBody: models.ExternalOpensearchResponse{
				Passed: false,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         constants.EXTERNAL_OPENSEARCH_FAILED_TITLE,
						Passed:        false,
						Status:        constants.STATUS_FAIL,
						SuccessMsg:    "",
						ErrorMsg:      constants.EXTERNAL_OPENSEARCH_ERROR_MSG,
						ResolutionMsg: constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG,
						DebugMsg:      "external opensearch is not reachable",
					},
				},
			},
		},

		{
			TestName: "Wrong Format IP",
			ReqBody: models.ExternalOSRequest{
				OSDomainName:   "Test",
				OSDomainURL:    "\t", // Passing "\t" for failing the http.NewRequest() method. You can also pass "%%"
				OSUsername:     "admin",
				OSUserPassword: "admin",
				OSCert:         CA_CERT,
			},
			ResponseBody: models.ExternalOpensearchResponse{
				Passed: false,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         constants.EXTERNAL_OPENSEARCH_FAILED_TITLE,
						Passed:        false,
						Status:        constants.STATUS_FAIL,
						SuccessMsg:    "",
						ErrorMsg:      constants.EXTERNAL_OPENSEARCH_ERROR_MSG,
						ResolutionMsg: constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG,
						DebugMsg:      "Failed to create request",
					},
				},
			},
		},
	}
	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			resp := eos.GetExternalOpensearchDetails(e.ReqBody)
			assert.Equal(t, e.ResponseBody.Passed, resp.Passed)
			assert.NotNil(t, resp.Checks)
			for index, check := range resp.Checks {
				assert.Equal(t, e.ResponseBody.Checks[index].Title, check.Title)
				assert.Equal(t, e.ResponseBody.Checks[index].Passed, check.Passed)
				assert.Equal(t, e.ResponseBody.Checks[index].Status, check.Status)
				assert.Equal(t, e.ResponseBody.Checks[index].SuccessMsg, check.SuccessMsg)
				assert.Equal(t, e.ResponseBody.Checks[index].ErrorMsg, check.ErrorMsg)
				assert.Equal(t, e.ResponseBody.Checks[index].ResolutionMsg, check.ResolutionMsg)
				assert.Contains(t, check.DebugMsg, e.ResponseBody.Checks[index].DebugMsg)
			}
		})
	}
}
