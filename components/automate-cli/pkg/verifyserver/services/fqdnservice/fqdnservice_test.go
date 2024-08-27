package fqdnservice_test

import (
	"crypto/tls"
	"errors"
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
MIIEEzCCAvugAwIBAgIUVGM2gmMmqgqDojI78bipOIp6BF4wDQYJKoZIhvcNAQEL
BQAwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcM
CUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQx
EjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVz
cy5jb20wHhcNMjQwNzE2MDgyNzUyWhcNNDQwNzExMDgyNzUyWjCBkDELMAkGA1UE
BhMCSU4xEjAQBgNVBAgMCUthcm5hdGFrYTESMBAGA1UEBwwJQmVuZ2FsdXJ1MRYw
FAYDVQQKDA1Qcm9ncmVzcy10ZXN0MQswCQYDVQQLDAJJVDESMBAGA1UEAwwJbG9j
YWxob3N0MSAwHgYJKoZIhvcNAQkBFhF0ZXN0QHByb2dyZXNzLmNvbTCCASIwDQYJ
KoZIhvcNAQEBBQADggEPADCCAQoCggEBALGgKZl/to8a0eySlkVbD1vFcYk8KBKF
KPG1Ue91P51ESVPd9XeVG2+7jAqSRyox1VUxJI9wIbAo5XAnCD/FoKjQyRwdZOoo
yCR3nqyY6S3JyotP/RaIJk31KY7vkTxSpVOPyWZVgVzofx1mSZuA7DF3s1vPlgKr
1v/TGZkmNaOQo2ddMWeo6CP7UMSeHNqLyWIi2fquvHDUm6kJ1Dx1h1CzR2Ov+tmN
sYNG7Hx4QqmYigi5fnaQIj7XbO9n4iY65z1nMurN86QhGYaxAnPwKncTqomi77y6
vpSJRUOF+NrcJ4K/xNz8azoiRu99Lz5xMQUVPxloYwlaIbzTx62nt6kCAwEAAaNj
MGEwHwYDVR0RBBgwFoIJbG9jYWxob3N0gglsb2NhbGhvc3QwHQYDVR0OBBYEFK8o
KHvj30katQkAPXctoOY3JW77MB8GA1UdIwQYMBaAFFPdAk1eAI3cw7sk7kGV+5vX
/WH5MA0GCSqGSIb3DQEBCwUAA4IBAQCnNfXul1pVHNJ5e2tOaFKdqVnKBBF/nzl0
qCCozDCvD2I5N9JbBLzd5ksiW7w2aH35OrFGusSb0R2OqCTSKHCHsBVZOYp+0aIL
TYxKz0xd0c/XdYYyVpQ6tdTI9bK34LDlXmQm59cCMHeCOJIx2V+dkF/eRNxy7ytG
WTUwGKOQ7ilQx4S8HkqnzIfg6xthYChNf9XQxK2V6rvLn1XzQ2XmraDbMpLso6Ol
DTTor4xWqRugl7H4lG5UKn+Oz8CxgexHznOf3nrKI7JzAP3BYyLAoLSpTSyNNLlp
OFRL8gDWFAt6Mze5e3RdcG4H6fyk48iZ9YovndjABzoi/iiXooT3
-----END CERTIFICATE-----`
	SERVER_KEY = `-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCxoCmZf7aPGtHs
kpZFWw9bxXGJPCgShSjxtVHvdT+dRElT3fV3lRtvu4wKkkcqMdVVMSSPcCGwKOVw
Jwg/xaCo0MkcHWTqKMgkd56smOktycqLT/0WiCZN9SmO75E8UqVTj8lmVYFc6H8d
ZkmbgOwxd7Nbz5YCq9b/0xmZJjWjkKNnXTFnqOgj+1DEnhzai8liItn6rrxw1Jup
CdQ8dYdQs0djr/rZjbGDRux8eEKpmIoIuX52kCI+12zvZ+ImOuc9ZzLqzfOkIRmG
sQJz8Cp3E6qJou+8ur6UiUVDhfja3CeCv8Tc/Gs6IkbvfS8+cTEFFT8ZaGMJWiG8
08etp7epAgMBAAECggEATjGeLos44C2j3Tu4uGZ6T+GO9iWL1H9XQ3SZUctxSje9
+hQbk92Jmvnaj+rONkaCzgKYwCZSRjzyqBkeuggzji1/YweTnbt/RAJ+0SdtezE6
n3KCjVE5X5c+CMTw0DfnvO3u8MRScwaMsvINFE5AbBRwWkn7lUh1mOAjBDb8ZPiz
DaTKjnCWKjMNcOxjVenneJRv4JQDSoNnQx2wjI+2Ve61vkG8fZbn5F2wbQEe4Eo2
9r+nW9WfkKUEs/pDeKH3DgstfL0ua4Bnkb9rhpbd4Hy4ZIyKw5n+j7dKE9olHoS/
BK23xjJSKHv88/+tVXYH78NiofrS1J/GQKlNdekpqwKBgQDbB0PZL2fzzebYgeLB
C/q0rT0++qcTwM2VmtAqEWsH3tliHkAqRYMrTzT5st3O+mCXYCq8jjYew6CcrRDd
rjBuOo74iXB/1Jcmzt9CHBG+VfBanPZ6UR1NvEdGeH85AXueCy4RGIclkfHGJjV4
WN7rT9S3hy791GDvsAaFuLEgMwKBgQDPm8lt++Vk4OtFB9z/4qml0Re8WJCFt7N9
+55JbKcL5Rs/AMnXY95klHb9p9cDcAff1qLEDzwVsiTLCqocuZ6sYzqnuiQmxVTn
GNlrfxJ5P/1ZRkyfuiVnx15lbfur2ycoQqoNEcHb7bXw1Zc5s6Rw/C2c0KzSQHIZ
o9ocDXT8swKBgBp48VFhsm28OCd442Azw3Rs79cZ0nHHvtFTKc+71TTZUWrTfyvc
xPLKGvwHC1oF1wDUxbWOdqoXOOVW4Y/5iq738unFKsy4dCBjpoZGADsCBg/dosi7
3w/TLsYssCvCqx85+LJmRYdb8V3iplnKW/8S4gHgm+Rf8+tkWhV1cisTAoGBALbe
M3RkgC/2imXYwkrG3cYIQo8Nt+eA42LiQw/L2+x3VyOMzz2hY2BXAeT2dUc65ES/
a8mk3bkCnLQYZbU9r02LbjwVkwhvoxSYZ9LvMFeLDN8ZNB8xuQcLrtNLKHa9aGVn
KyCt9oOBhFRp33XdDcjT6F2L1d6xE5AQKuuw0s5PAoGBAMUc+02/UnIQQ+5CS/lo
GLYGp405Ig9zUrKqy4XTQ7v1jwvOYQtjdAKDb2FRfDgqrSMveUhmOcpEimjUr/hT
k/AjrTngLOxdhvqwcm/Hd1Hr9ciiDGoPURUeXyLpqTTDWl3RjY+qEBf5v378mK0E
35y+SseC0UdlfiptLTFFjSrf
-----END PRIVATE KEY-----`
	CA_CERT = `-----BEGIN CERTIFICATE-----
MIIEAzCCAuugAwIBAgIUZIyJCoa5dRUdWmDYuGngDC2Kue8wDQYJKoZIhvcNAQEL
BQAwgZAxCzAJBgNVBAYTAklOMRIwEAYDVQQIDAlLYXJuYXRha2ExEjAQBgNVBAcM
CUJlbmdhbHVydTEWMBQGA1UECgwNUHJvZ3Jlc3MtdGVzdDELMAkGA1UECwwCSVQx
EjAQBgNVBAMMCWxvY2FsaG9zdDEgMB4GCSqGSIb3DQEJARYRdGVzdEBwcm9ncmVz
cy5jb20wHhcNMjQwNzE2MDgyNTI5WhcNNDQwNzExMDgyNTI5WjCBkDELMAkGA1UE
BhMCSU4xEjAQBgNVBAgMCUthcm5hdGFrYTESMBAGA1UEBwwJQmVuZ2FsdXJ1MRYw
FAYDVQQKDA1Qcm9ncmVzcy10ZXN0MQswCQYDVQQLDAJJVDESMBAGA1UEAwwJbG9j
YWxob3N0MSAwHgYJKoZIhvcNAQkBFhF0ZXN0QHByb2dyZXNzLmNvbTCCASIwDQYJ
KoZIhvcNAQEBBQADggEPADCCAQoCggEBAMdTgFD+Ily9Qat/O1Vp70tchVaqXPvh
4BiZ0eI5Kahei+UiPb7357LLY5E94Ru0gIar3NC7Pmw9Y9mxUvgB7luKNmlpHemp
Y0I4agbc0AAz0pkp1rXWhtzLOvmsqT10+rCYni+tNpm1lHAcJSHNkH958THEsEQy
G5letfx9okyQJFhe+h0l25KsPA7MzNakzc9bd8CvhogNA8A5DVYBLiQTCczVLRm9
akp4GFE5Ed2kTqgt8/R2czFDMJPVGnl7vj3+wmvRpkYgT3oCUHddvdYUi9bDr3Ty
Y15j/XC6ZvU/sQ+1665gWKLUWwv7/gR0D1kIixAOhkRRNQUxOozpQXMCAwEAAaNT
MFEwHQYDVR0OBBYEFFPdAk1eAI3cw7sk7kGV+5vX/WH5MB8GA1UdIwQYMBaAFFPd
Ak1eAI3cw7sk7kGV+5vX/WH5MA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEL
BQADggEBAJgKcQRuSMuG8HnM3iy8urqFatyOoLNCtns3zbjqGZ+WLGMcoGYs1xXm
6WPLE/pca1TS/kJOPBta8XsZ+ytwWtmuMLzOmM3LhUr7c1/GtwaVGAwBYPc8Slry
gtA6/mwgODHdCYyhOhp9z42e6fPnk5TOce/h3YsCghg1ARXLm/dCz4TxwtSQGx1y
IgQoiLfayFmp4cNGwFex1b3dWQCdNUpaIdg7BpMNujVTmVqo2XZw6F8ubo5tb1Kp
udI0NqUP0i7AX4pLvnnu1BQ6ZsCuc6rFnkUHjqwzzLWc+js6dI0lCvEdsJpv3bEt
XDwMI4C/PDOL/LwhTmamQfGQKRbBhKI=
-----END CERTIFICATE-----`
	INVALID_BLOCK_CA_CERT = `INVALID`
	NON_CA_CERT           = `-----BEGIN CERTIFICATE-----
MIIDbDCCAlQCCQDDfzhxYrhWVDANBgkqhkiG9w0BAQsFADB4MQswCQYDVQQGEwJJ
TjERMA8GA1UECAwIS2FybmF0a2ExEjAQBgNVBAcMCUJlbmdhbHVydTERMA8GA1UE
CgwIUHJvZ3Jlc3MxDTALBgNVBAsMBHRlc3QxIDAeBgkqhkiG9w0BCQEWEXRlc3RA
cHJvZ3Jlc3MuY29tMB4XDTIzMDYwMjE0MDgyMVoXDTIzMDcwMjE0MDgyMVoweDEL
MAkGA1UEBhMCSU4xETAPBgNVBAgMCEthcm5hdGthMRIwEAYDVQQHDAlCZW5nYWx1
cnUxETAPBgNVBAoMCFByb2dyZXNzMQ0wCwYDVQQLDAR0ZXN0MSAwHgYJKoZIhvcN
AQkBFhF0ZXN0QHByb2dyZXNzLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
AQoCggEBAKtqz3RteZSLkC19zCHNKrw8BMNeTodGgSib25LPor3cJPpegSie+TdK
syRMagrjEOjjJ1Vr/YKuS4/5JJuEtddxH0GvVqPIkia/lUWOTHoylYgcnN+XzfkL
qxUYklBrZA3RQQ5/Gqhhnfn4jfvPOl82j4U/yD0bgz8vk44qohryP9+081+qB32W
dIQVgeS0U9L8cPWebV3fSFLO0BSXxTbGv2T1XBKyPBlMXAb0pLDVAiQvTqUm4jpu
OaMaQZplvrXM5cYb7fVQCfg9Ab7jpiZwz81NrOheN+2IICXVp11yptIuAF1pP4Of
SeN6xMZd7zwQN4ACB4WGJ32cjYuHDkcCAwEAATANBgkqhkiG9w0BAQsFAAOCAQEA
RmFFfBaTUKYqb1QmzwuyhgWN2qilSboAC2AOMQ5s9L1K6B8fkqsw2v5DvuuJN63N
KK0hEcVHNlBzruqAf9CfwMwKHLR6ow7666BcoQhokF5eiFL8yAZRpc3qHAq06DOr
eH3y6yWn5t4EKNT6GHfk0EgEi6XdpAmiYThpKFOO5yz2k/5QC1G2wv1VuaO7M40S
oQU6hzxXXnaCCeN+2yLfNz1fJRkmXCXd88FJIXy0M42FLXBYysgPWPdRL5vBmNli
Gc99pBaktsaH4rAwZZVVOkMRpl3DgL8Z1oNTGgv+iTFS4kXMLRKpmI58aELyHhnS
iIfhYvIBaIG7Urz+it5WPA==
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
		case "/check_status":
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
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.FQDN_TITLE,
						Passed:        true,
						SuccessMsg:    constants.FQDN_TITLE,
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
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "no such host",
						ResolutionMsg: constants.GENERIC_FQDN_CERT_RESOLUTION_MESSAGE,
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
			"Case is Before Deployment, FQDN is reachable, Nodes are not reachable, Certicate is valid",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.3",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.FQDN_TITLE,
						Passed:        true,
						SuccessMsg:    constants.FQDN_TITLE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.NODE_ERROR_MESSAGE, "[172.154.0.3]"),
						ResolutionMsg: constants.NODE_RESOLUTION_MESSAGE,
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
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
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
			"Case is Before Deployment, FQDN and Nodes are not reachable, Certicate is non CA",
			models.FqdnRequest{
				Fqdn:              LOCALHOST,
				NodeType:          constants.CHEF_INFRA_SERVER,
				RootCert:          NON_CA_CERT,
				IsAfterDeployment: false,
				Nodes: []string{
					"172.154.0.2",
				},
			},
			models.FqdnResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
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
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.CERTIFICATE_ERROR_MESSAGE,
						ResolutionMsg: constants.CERTIFICATE_RESOLUTION_MESSAGE,
					},
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
						SuccessMsg:    constants.FQDN_TITLE,
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
						ErrorMsg:      "no such host",
						ResolutionMsg: constants.GENERIC_FQDN_CERT_RESOLUTION_MESSAGE,
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
						SuccessMsg:    constants.FQDN_TITLE,
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
						ErrorMsg:      "no such host",
						ResolutionMsg: constants.GENERIC_FQDN_CERT_RESOLUTION_MESSAGE,
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
						SuccessMsg:    constants.FQDN_TITLE,
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
						SuccessMsg:    constants.FQDN_TITLE,
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
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.FQDN_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.FQDN_ERROR_MESSAGE,
						ResolutionMsg: constants.GENERIC_FQDN_CERT_RESOLUTION_MESSAGE,
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
			httpsFailedServerPort,
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			res := fq.CheckFqdnReachability(e.ReqBody, e.Port, time.Second*2)
			if e.ResponseBody.Passed {
				assert.Equal(t, e.ResponseBody, res)
			} else {
				for i := 0; i < len(e.ResponseBody.Checks); i++ {
					check := e.ResponseBody.Checks[i]
					if check.Passed {
						assert.Contains(t, res.Checks[i].SuccessMsg, check.SuccessMsg)
					} else {
						assert.Contains(t, res.Checks[i].ErrorMsg, check.ErrorMsg)
					}
				}
			}
		})
	}
}

func TestMakeConcurrentCalls(t *testing.T) {
	fq := fqdnservice.NewFqdnService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, fq)
	client := &http.Client{
		Transport: &http.Transport{},
		Timeout:   2 * time.Second,
	}
	url := fmt.Sprintf("https://%v:6574", LOCALHOST2)
	res := fq.MakeConcurrentCalls(url, client, make(map[string]int), time.Second*2)
	assert.Equal(t, errors.New("nodes are not reachable"), res)
}
