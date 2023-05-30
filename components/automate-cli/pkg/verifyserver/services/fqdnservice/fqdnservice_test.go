package fqdnservice_test

import (
	"crypto/tls"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/fqdnservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

const (
	LOCALHOST  = "localhost"
	LOCALHOST2 = "localhost2"
)

var (
	SERVER_CRT = `-----BEGIN CERTIFICATE-----
MIIF2TCCBMGgAwIBAgIJAIhfhE2nWW2GMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD
VQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx
FjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls
b2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz
MDUyMzA3MTIwNVoXDTI0MDUyMjA3MTIwNVowgZAxCzAJBgNVBAYTAklOMRIwEAYD
VQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv
Z3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G
CSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggIiMA0GCSqGSIb3DQEBAQUA
A4ICDwAwggIKAoICAQDIEPUAQOpwMuAy+lxv4GqUdoH4B4LrfQQxaxcT1T9Y6UMK
lEu7IyDp2MJyzWLRDW5T1PkccCnOQt7GZqo2r3NjkgWylBFzfgOuNA61b0CChFpI
bZJoq5+no9L8ktCCDTufru7dRACzy35ahNitjVdaB4x76x2YyK25RsxPoinAXiKN
lAdticBnP+xHOTklHx0AUUKnZbaI5v4iN/76rdm6/kTamBs4mnWOKuvEKolR4Qry
9YvK/aBK9t1c7N6eHED8vnJV5XO+ZNpExNk1XgRnUgBXUnKCCxiRT3ykqfQHXtuS
B7uNG/Azyx/vl+N2w8y7rwMjdkBAvXX1FYuA+WrrVN5Dy1Mu6AOUphoVgkMPgwob
Nrj10GJbpXwRsRVXMTLHUh5QhUgUx9dnBqUWtBj5YVAs1ocKPwBpCZinDGeekk1M
Wjont44h/Ey7965+KAOC+WhV+uQphlJmsU/OCiZP7JZl+TUdiZXWjjYeCEkoPlLi
h4KQkW3XfJt9eYiMAe3mvLNXsrLdEFaBwsY+z9Oj9LDg8k5Lh2Mtjqh0Qy3WjSuy
Uai31lfhfrZlmDIBsOsSAOJsV6sTHw17fL96Tuw1PROWDZ95sv6Wu3mWnWM0hlTs
nCkwiN2WkePTqf06p5g+XKDdn8vDlwNtxkUpiAOlnhxNQbvtHeZpAG2fWE/vaQID
AQABo4IBMjCCAS4wHQYDVR0OBBYEFNazhNLDroMj++E97WFBqd32paZXMIHFBgNV
HSMEgb0wgbqAFF2fU1h3N/yAmpXIhT9jv5FHKkV/oYGWpIGTMIGQMQswCQYDVQQG
EwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUxFjAU
BgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAlsb2Nh
bGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tggkAr+0hZ+Hj
APEwDAYDVR0TBAUwAwEB/zALBgNVHQ8EBAMCAvwwFAYDVR0RBA0wC4IJbG9jYWxo
b3N0MBQGA1UdEgQNMAuCCWxvY2FsaG9zdDANBgkqhkiG9w0BAQsFAAOCAQEApqiR
BN4EnG8Zst8XUpWkzcV02IbBnxe47aF2+R6yqHy98tWZgmHDkL0PCMiOI0XLXSNT
Af7e7BKxqt5FTzKe/UyEuyHeJUojc4PdEji1D45h7rjjsKIuaRuONYwrBvFuYuFT
sXkiARa3d5Xun5Qj/Lt30ziH8qIVFZ7wGD2mZVppojR4ZyI4U6bRBow4q+7dK9gs
4yCfW+fjutTMnbkqYt8Vq8liX8k4w1RnamysvOAqlToOakavgh467Rf/FJsfksmi
vaeBHZyA3aOAD/s+TndmDRfKAPdxljTyRMx6/mdgotb/qIFxIA54yEdwG7DkWOuX
WZTeXqClyhiid0tayQ==
-----END CERTIFICATE-----`
	SERVER_KEY = `-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----`
	CA_CERT = `-----BEGIN CERTIFICATE-----
MIIE2TCCA8GgAwIBAgIJAK/tIWfh4wDxMA0GCSqGSIb3DQEBCwUAMIGQMQswCQYD
VQQGEwJJTjESMBAGA1UECAwJS2FybmF0YWthMRIwEAYDVQQHDAlCZW5nYWx1cnUx
FjAUBgNVBAoMDVByb2dyZXNzLXRlc3QxCzAJBgNVBAsMAklUMRIwEAYDVQQDDAls
b2NhbGhvc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4XDTIz
MDUyMzA2NDY0NFoXDTMzMDUyMDA2NDY0NFowgZAxCzAJBgNVBAYTAklOMRIwEAYD
VQQIDAlLYXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJv
Z3Jlc3MtdGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4G
CSqGSIb3DQEJARYRdGVzdEBwcm9ncmVzcy5jb20wggEiMA0GCSqGSIb3DQEBAQUA
A4IBDwAwggEKAoIBAQDjU96JdUaSdNlRayVK9Fbs/Fu5cRaKe+WQZZDiHdIT1gtl
ITx2VYlPnQno6R3p5uCjKX1le8hJR9YrafaolIQrMnuHg6JkSB8DeahMutWufvGI
OZu3USs1L0sd5FCq6Y5Yr0mNkQiNNNUhjFMZYvXDd9kps8olmOpR6Jwvc+UuP637
oh0zEc9OoKHicQ+SeoD7R8JbUmzy6okNWYJ5utLoVEunSkmb24NgNmkfJ5MAQcBe
ACKj905Vi8Z3Hpb5zXVxW84scXxN7ZxxRykE1Cc254iCNKrQxW+QkO1FxshOVUi5
94E4aY+rk9FMnE9d7Hc3io3uhcTg8TbGiYwnJDwrAgMBAAGjggEyMIIBLjAdBgNV
HQ4EFgQUXZ9TWHc3/ICalciFP2O/kUcqRX8wgcUGA1UdIwSBvTCBuoAUXZ9TWHc3
/ICalciFP2O/kUcqRX+hgZakgZMwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlL
YXJuYXRha2ExEjAQBgNVBAcMCUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3Mt
dGVzdDELMAkGA1UECwwCSVQxEjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3
DQEJARYRdGVzdEBwcm9ncmVzcy5jb22CCQCv7SFn4eMA8TAMBgNVHRMEBTADAQH/
MAsGA1UdDwQEAwIC/DAUBgNVHREEDTALgglsb2NhbGhvc3QwFAYDVR0SBA0wC4IJ
bG9jYWxob3N0MA0GCSqGSIb3DQEBCwUAA4IBAQBAYHmcpWQf8U5OfzuLWM6M2Xt6
GxBjWAWilF4aZG/6f38fciRxHGQcXWZp1RrgHIGCjy6VpUTRyqr5ZneSZ0JxU5X8
Se8KOg5dyu1BdzS7DdR9Ofs80EX85eoXW8KAHeWjLprN1r9NXQ7gRaFXXkFYYkXY
DLt/b+QavfN/FZGTi/e5HlKONoNu6FxRdtuWE8d7PwjFm+Qm3nlX1qqvRyZP3gmK
Vm7y9nw2ib8nIGcH/2EKulEyDWxvHTTBjuQXytHU+oubDz3a1eFr5IkgLwusYn1P
2QTvRmA7J/8Qj/wMN/W5ve4akoHzS1Zzfiphq5rSh+WrhUSWpL3bKzVrGEpT
-----END CERTIFICATE-----`
	INVALID_BLOCK_CA_CERT = `INVALID`
	NOT_SAN_CA_CERT       = `-----BEGIN CERTIFICATE-----
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
	INVALID_CA_CERT = `-----BEGIN CERTIFICATE-----
MIIBmTCCAQKgAwIBAgIJANsPlZp1G0g6MAoGCCqGSM49BAMCMIGzMQswCQYDVQQG
EwJVUzETMBEGA1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNj
eTELMAkGA1UECgwCSVQxCzAJBgNVBAsMAklUMRowGAYDVQQDDBF3d3cuZXhhbXBs
ZS5jb20wHhcNMjEwNDEyMTM1MTM5WhcNMjIwNDEyMTM1MTM5WjCBszELMAkGA1UE
BhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lz
Y3kxCzAJBgNVBAoMAklUMQswCQYDVQQLDAJJVDEaMBgGA1UEAwwRd3d3LmV4YW1w
bGUuY29tMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEzZM6tLoBdzjsCOEl8W1t
KxTIBW56+54vlRXO3lgJirWW+un1ViJZr3Qq5+/1CijU1Rj8Q+4rThpx9Y3j7N1i
5qNjMGEwHQYDVR0OBBYEFBZnu9HidO1EFT1DnjN8Uy+AXVuxMAoGCCqGSM49BAMA
A0gAMEUCIQCZQbUd8DmzwP1UcThdNyFZOLcN1YdPAnGKv1riM3n8jwIgP1IJKNo8
t0WcQFnqRRtXOVDCZuBE4jgFJgLP1p7p7qI=
-----END CERTIFICATE-----`
	TIMEOUT = 2
)

func TestFqdnService(t *testing.T) {
	fq := fqdnservice.NewFqdnService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, fq)
}

func startHTTPSMockServer(mockServer *httptest.Server, port string) error {
	cert, err := tls.X509KeyPair([]byte(SERVER_CRT), []byte(SERVER_KEY))
	if err != nil {
		return fmt.Errorf("Failed to load certificate and private key: %s", err)
	}

	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		MinVersion:   tls.VersionTLS12,
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

func TestCheckFqdnReachability(t *testing.T) {
	httpsSuccessPort := "5345"
	httpsFailurePort := "3345"
	httpsFailedServerPort := "4345"
	httpsSuccessMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/":
			w.Header().Set("x-server-ip", "172.154.0.2")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte("OK"))
		case "/_status":
			w.WriteHeader(http.StatusOK)
		case "/check_status":
			w.Header().Set("x-server-ip", "9baa99b32b43b7fc1aa1488bbc06348f")
			w.WriteHeader(http.StatusOK)
		default:
			http.NotFound(w, r)
		}
	}))

	err := startHTTPSMockServer(httpsSuccessMockServer, httpsSuccessPort)
	assert.NoError(t, err)
	defer httpsSuccessMockServer.Close()

	httpsFailureMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/":
			w.Header().Set("x-server-ip", "172.154.0.2")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte("OK"))
		case "/_status":
			w.WriteHeader(http.StatusOK)
		case "check_status":
			w.Header().Set("x-server-ip", "9baa99b32b43b7fc1aa1488bbc06348f")
			w.WriteHeader(http.StatusOK)
		default:
			http.NotFound(w, r)
		}
	}))

	err = startHTTPSMockServer(httpsFailureMockServer, httpsFailurePort)
	assert.NoError(t, err)
	defer httpsFailureMockServer.Close()

	httpsFailedResponseServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadGateway)
	}))

	err = startHTTPSMockServer(httpsFailedResponseServer, httpsFailedServerPort)
	assert.NoError(t, err)
	defer httpsFailedResponseServer.Close()

	fq := fqdnservice.NewFqdnService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, fq)

	tests := []struct {
		TestName     string
		ReqBody      models.FqdnRequest
		ResponseBody models.FqdnResponse
		Port         string
	}{
		{
			"Case is Before Deployment, FQDN is reachable, Nodes are reachable and Certicate is valid",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
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
			},
			httpsSuccessPort,
		},
		{
			"Case is Before Deployment, FQDN and Nodes are not reachable, Certicate is valid",
			models.FqdnRequest{
				Fqdn:              LOCALHOST2,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is Before Deployment, FQDN and Nodes are not reachable, Certificate block type is wrong",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          INVALID_BLOCK_CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is Before Deployment, FQDN and Nodes are not reachable, Certicate is not SAN based",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          NOT_SAN_CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is Before Deployment, FQDN and Nodes are not reachable, Certicate is invalid",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          INVALID_CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is After Deployment, FQDN is reachable, Chef Server Status is okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"10.1.1.11",
				},
			},
			models.FqdnResponse{
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
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is After Deployment, FQDN is not reachable, Chef Server Status is not okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST2,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is After Deployment, FQDN is reachable, Automate Status is okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.AUTOMATE,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"10.1.1.11",
				},
			},
			models.FqdnResponse{
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
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is After Deployment, FQDN is not reachable, Automate Status is not okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST2,
				NodeType:          constants.AUTOMATE,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsSuccessPort,
		},
		{
			"Case is After Deployment, FQDN is reachable, Automate Status is not okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.AUTOMATE,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
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
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsFailurePort,
		},
		{
			"Case is After Deployment, FQDN is reachable, Chef Server Status is not okay",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: true,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
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
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
				},
			},
			httpsFailurePort,
		},
		{
			"Case is Before Deployment, FQDN is giving bad gateway, Nodes are not reachable, Certicate is valid",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.FQDN_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.2]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			httpsFailedServerPort,
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			res := fq.CheckFqdnReachability(e.ReqBody, e.Port)
			assert.Equal(t, e.ResponseBody, res)
		})
	}
}
