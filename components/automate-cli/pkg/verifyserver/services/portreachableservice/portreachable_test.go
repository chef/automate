package portreachableservice_test

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/portreachableservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	LOCALHOST             = "localhost"
	SERVER_NOT_RUNNING_IP = "192.168.43.23"
	SERVER_CRT            = `-----BEGIN CERTIFICATE-----
MIIGlDCCBHygAwIBAgIJAIyoNS4igNAdMA0GCSqGSIb3DQEBCwUAMGUxCzAJBgNV
BAYTAklOMQwwCgYDVQQIDANOU1cxEjAQBgNVBAcMCUJlbmdhbHVydTEVMBMGA1UE
CgwMR29MaW51eENsb3VkMQwwCgYDVQQLDANPcmcxDzANBgNVBAMMBlJvb3RDQTAe
Fw0yMzA1MTkwNzM5NDNaFw0yNDA1MTgwNzM5NDNaMGgxCzAJBgNVBAYTAklOMQww
CgYDVQQIDANOU1cxEjAQBgNVBAcMCUJlbmdhbHVydTEVMBMGA1UECgwMR29MaW51
eENsb3VkMQwwCgYDVQQLDANPcmcxEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJ
KoZIhvcNAQEBBQADggIPADCCAgoCggIBALSV2HUqcjuNYdasbw8sC0d27z7kuC/2
TyZu3IB9Swhnlfv08Bw7Qe8PWDjEz1DqHQky/3QcRlrY8CYBz3GLioG3iETOOnOE
Ex/vGxQ5KaDhaP0FmB8qdFYJBvzRxVX8mzxZ0rblLF7GpBN5+gofP5Hwwaw+xtVp
QKD+bpx+G7Vat0f2x0vG6qwkUQ52wRFysJRqR6mTn9ODRNBcLfnG3lAaqf5yUqzj
Cfe14v5FeFARnvLQls5x5dQGdT6eliisBRstd3qZHl0e6yfz6n+kkxkgHoH01lCQ
VqY3r8UQE8+AdtvsDk2LtwdGjuvlqDX3SSActf5yLmmQsD3tJ2dUEt+LjrWtbSzE
bO14zFGm2LpaXEaYLPJDs5BLw2FoOaerBocSmNqsq9SysX6f783QclEF5DvmQ1B3
xW78hwzbMrHF7HiHi9c8/tq1RxSptLDxzWWMsIaK3/wNjhC2S05LLD89j35qCVqZ
Z3RpcCnrpn5wMgjjMQh65ET62zL2gOGPTfzM2Pubxl5iP7Rh2zRpY5ATk1mdCaAv
3acYj1iIKYQla2qsroKxCsKZUEkkiVwDS/Ytjc+VkFnlAkSPLdxaH5f4Lb6WETqy
zArHApek65FgmzNcSnKFV4paCCSPI3PptejmpXHJuBgtBaFjmZtKnla7AV/6+AZt
7U9HK2RDvntbAgMBAAGjggFCMIIBPjAJBgNVHRMEAjAAMBEGCWCGSAGG+EIBAQQE
AwIGQDAzBglghkgBhvhCAQ0EJhYkT3BlblNTTCBHZW5lcmF0ZWQgU2VydmVyIENl
cnRpZmljYXRlMB0GA1UdDgQWBBSWBn1EfBuhxKjRYGsx4hrLquqxUDB/BgNVHSME
eDB2oWmkZzBlMQswCQYDVQQGEwJJTjEMMAoGA1UECAwDTlNXMRIwEAYDVQQHDAlC
ZW5nYWx1cnUxFTATBgNVBAoMDEdvTGludXhDbG91ZDEMMAoGA1UECwwDT3JnMQ8w
DQYDVQQDDAZSb290Q0GCCQCBf1/4VlNK+TAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0l
BBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMBoGA1UdEQQTMBGHBLS84NmCCWxvY2Fs
aG9zdDANBgkqhkiG9w0BAQsFAAOCAgEAgxk1Ln2ZYPT5r+D35jb7CDo1PQ2T5On7
ycHUCYbLNzzPLGM9/hI9aLrJYnmM2Nvi5csV7hgGznvaHR6x4z4iW6j2HCCBOug0
O0ohH+nOFKPxdAsPGsV+Os2LBcNBq/isVPR3EIqMW2WIejP5Ejs5PqZqtmABn8LM
Rcp6ruOeH4lp5Vb/nIXt6G+4636HcEWjSNDx8pbuEDzODYSrvyz6M9ZeCUPeNXsO
f+Pq8/Mfd3cJTEovmNzVJn987TvXRVp+F5X2pzAANwwP+a+HzgE4MrJuC/pc4PL7
P7l6ZHXht6fAwh0+nxKFn3orpeewZ6P/I4r8glTQISYF4OsfQyBvVt/rN3rwhTvc
4QLX95aVWEHw5pH3PC2FRyE0WTCYxt5KV4Lq2NYHdtqZwZaYh24Fl12z4pAzT9Mj
GFsRxbsToN0+ZxVhu/BDB+MsqKZB9lf/9zduqKDZrvw8+6kphtFkyKJF0sxaK3MT
3bPneMOU1iZMPs8CZ4Q4ukPjE2ruH67/5kTHindAFzBRsOLCOEoR7m+VdsjvKS39
6/+wNovmZdAnxMJtIYbKqO/WZYRL4OM9LIVjAmR1eJVkeqOn9ltnCnAocwYTd/kb
iTiNYukhVUodb75Q7GPR3ex0R9i87UEGC81yxG88ju0wGsMqUNvTeR5nEUjBn9JL
b/7Kj23UFr0=
-----END CERTIFICATE-----`
	SERVER_KEY = `-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----`
	CA_CERT = `-----BEGIN CERTIFICATE-----
MIIFRjCCAy4CCQCBf1/4VlNK+TANBgkqhkiG9w0BAQsFADBlMQswCQYDVQQGEwJJ
TjEMMAoGA1UECAwDTlNXMRIwEAYDVQQHDAlCZW5nYWx1cnUxFTATBgNVBAoMDEdv
TGludXhDbG91ZDEMMAoGA1UECwwDT3JnMQ8wDQYDVQQDDAZSb290Q0EwHhcNMjMw
NTE5MDczNjEzWhcNMjQwNTE4MDczNjEzWjBlMQswCQYDVQQGEwJJTjEMMAoGA1UE
CAwDTlNXMRIwEAYDVQQHDAlCZW5nYWx1cnUxFTATBgNVBAoMDEdvTGludXhDbG91
ZDEMMAoGA1UECwwDT3JnMQ8wDQYDVQQDDAZSb290Q0EwggIiMA0GCSqGSIb3DQEB
AQUAA4ICDwAwggIKAoICAQCqxnMkCdbUW7FNUtk4C9HcJSZvRs6El4OpxfJvnOV7
wigeY9QvebZG1bGjU7pYrqpDZuA4SpB4P03yQ58eGMjZFJmNJvxtHAOJ7xeoHnLB
lp/qvYME5acO1/ExeoBYugtSuFbp+PjDFSdHmLqBezvwrWhaKaw/Nt/fVlIYP1T4
F7omJDK9pU+AZh/wQq3fSAFCRiUhNKKgRuGlxdzppnG5SfRVjXSRrAfJUBtDfLP5
nCg972ZEpJBbuSFez/19uUAgTED5zNYW9Qn4M4YKMjuekOhakT41GEDppF4w4TwD
CI1zyvYH/6EczdnsFRgRoRhw+lthmzmgc9cFSaq0EOcgqbqAMODJb13OSbhKCJIn
WSKocy4HLG1Q+qmLkMXe/y8KBXB9jnpX+5TMO4kBorqu0MDG52m5Sjse0sBvsHMD
HYjz/gvj8xEv2q4fST24ypGyMWs/lSzlYU9xUQTgzeUwZ1HKDAeTsNMoyvPM7uaI
TzZRSU1nKmvN0254N0+AqdpFHJSwWuPPmKe0I29+sdZ2lTFGdeKJDfNR/03mjlYP
bJKiWzKdRt2s42z/JFA4j5aG2B4R0wD6K0Olr9BcSAGUCDVoIyrLQxcy2eHlAVXK
TCoEs29L4cGx9/pLQIQQ94kArrhSptriYC5xXjlIfkUtJ24IT4eC4MXNlM0zX8kB
BwIDAQABMA0GCSqGSIb3DQEBCwUAA4ICAQA4Rz1kz3sxVqUKdWwz0nzs8gJ9qSQR
lizdG52jC1YS+MIpOigeo9dsNsrHFJNAmk948TvthywP/ETVDDaVuM3ggCbN+wxS
ojLmqmV/bp/QRDCgtEeGIonaLBAG73ecaLi1HJmgpU83RhVUldgA0h3gJofzlRM7
RD+Fr2dObPLnkBPM/UGyQZtEfD+3hTPjIBN9aKKbzlxJUKc7acI5W1iAaxfZfWeI
MgglQa9hFiu1rxrQiuvSyAqu7je9T6y1prdc6nAWj5Hy1nf3r6Vc4SR9UJEM01uI
QSvgXGvmDIkt80XatLb/KlzUhzmRHrOrQdxWQw14e65szaPfq4CJQlnH+kmGvLLI
7raQxCwdJKVl/qy/2MLNFoaF2Z/9pMxnau2Ctvdv3krKmfcRHdsb1yrf6XEyoBMN
etScbM30xcA5NlhwgoZKi/3UKJuLqanDR+V+VUJqd+dNQkZWBE11O25xWssWe4B8
SxFvcp+BFuZ5zXfXyZMDhAjrO3q8Y6MECjTeXlx+WgW8/dGyaYxS2Zarwv7TCErY
ZH/kXdVksYGGJoG5LtSqsVqdzEuLMkwNP2g8Cj7cTNkgZlqy9IxLiLC7o25Urb8c
EKyM/tPAc5zaHPhTa9DkM73qXlz/0RT3Z5uxSpRyv166Bm/LIznSLdiT7HjcvBSe
NQWfC1YBMKULeA==
-----END CERTIFICATE-----`
	TIMEOUT = 2
)

func TestPortReachableDetails(t *testing.T) {
	pr := portreachableservice.NewPortReachableService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, pr)

	prDetails := pr.GetPortReachableDetails(models.PortReachableRequest{})
	assert.Equal(t, models.Checks{
		Title:         "Check for reachability of service at destination port",
		Passed:        true,
		SuccessMsg:    "The  service running at :0 is reachable",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, prDetails)
}

func startTCPMockServerOnCustomPort(mockServer *httptest.Server, port string) error {
	l, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%s", port))
	if err != nil {
		return err
	}
	mockServer.Listener = l
	mockServer.Start()
	return nil
}

func startUDPMockServerOnCustomPort(t *testing.T, port string) (*net.UDPConn, error) {
	serverAddr, err := net.ResolveUDPAddr("udp", "localhost:"+port)
	if err != nil {
		return nil, fmt.Errorf("Failed to resolve server address: %s", err)
	}

	conn, err := net.ListenUDP("udp", serverAddr)
	if err != nil {
		return nil, fmt.Errorf("Failed to start UDP server: %s", err)
	}

	go func() {
		buffer := make([]byte, 1024)
		n, addr, err := conn.ReadFromUDP(buffer)
		if err != nil {
			t.Errorf("Failed to read from UDP: %v", err)
			return
		}

		message := string(buffer[:n])
		response := "Hello, " + message
		_, err = conn.WriteToUDP([]byte(response), addr)
		if err != nil {
			t.Errorf("Failed to write to UDP: %v", err)
			return
		}
	}()

	return conn, nil
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

func TestGetPortReachableDetails(t *testing.T) {
	tcpTestPort := 1234
	udpTestPort := 1235
	httpsTestPort := 1236

	tcpMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	}))
	err := startTCPMockServerOnCustomPort(tcpMockServer, strconv.Itoa(tcpTestPort))
	assert.NoError(t, err)
	defer tcpMockServer.Close()

	conn, err := startUDPMockServerOnCustomPort(t, strconv.Itoa(udpTestPort))
	assert.NoError(t, err)
	defer conn.Close()

	httpsMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	}))
	err = startHTTPSMockServerOnCustomPort(httpsMockServer, strconv.Itoa(httpsTestPort))
	assert.NoError(t, err)
	defer httpsMockServer.Close()

	pr := portreachableservice.NewPortReachableService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, pr)
	tests := []struct {
		TestName     string
		ReqBody      models.PortReachableRequest
		ResponseBody models.Checks
	}{
		{
			"TCP Server running there",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            tcpTestPort,
				DestinationNodeServiceProtocol: constants.TCP,
				RootCA:                         "",
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("The %s service running at %s:%d is reachable", constants.TCP, LOCALHOST, tcpTestPort),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			"TCP Server not running there",
			models.PortReachableRequest{
				DestinationNodeIp:              SERVER_NOT_RUNNING_IP,
				DestinationNodePort:            tcpTestPort,
				DestinationNodeServiceProtocol: constants.TCP,
				RootCA:                         "",
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("The %s service running at %s:%d is not reachable", constants.TCP, SERVER_NOT_RUNNING_IP, tcpTestPort),
				ResolutionMsg: fmt.Sprintf("Check your firewall settings to provide access to %d port at %s", tcpTestPort, SERVER_NOT_RUNNING_IP),
			},
		},
		{
			"UDP Server running there",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            udpTestPort,
				DestinationNodeServiceProtocol: constants.UDP,
				RootCA:                         "",
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("The %s service running at %s:%d is reachable", constants.UDP, LOCALHOST, udpTestPort),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			"UDP Server not running there",
			models.PortReachableRequest{
				DestinationNodeIp:              SERVER_NOT_RUNNING_IP,
				DestinationNodePort:            udpTestPort,
				DestinationNodeServiceProtocol: constants.UDP,
				RootCA:                         "",
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("The %s service running at %s:%d is not reachable", constants.UDP, SERVER_NOT_RUNNING_IP, udpTestPort),
				ResolutionMsg: fmt.Sprintf("Check your firewall settings to provide access to %d port at %s", udpTestPort, SERVER_NOT_RUNNING_IP),
			},
		},
		{
			"HTTPS Server running there",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            httpsTestPort,
				DestinationNodeServiceProtocol: constants.HTTPS,
				RootCA:                         CA_CERT,
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("The %s service running at %s:%d is reachable", constants.HTTPS, LOCALHOST, httpsTestPort),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			"HTTPS Server not running there",
			models.PortReachableRequest{
				DestinationNodeIp:              SERVER_NOT_RUNNING_IP,
				DestinationNodePort:            httpsTestPort,
				DestinationNodeServiceProtocol: constants.HTTPS,
				RootCA:                         CA_CERT,
			},
			models.Checks{
				Title:         "Check for reachability of service at destination port",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("The %s service running at %s:%d is not reachable", constants.HTTPS, SERVER_NOT_RUNNING_IP, httpsTestPort),
				ResolutionMsg: fmt.Sprintf("Check your firewall settings to provide access to %d port at %s", httpsTestPort, SERVER_NOT_RUNNING_IP),
			},
		},
	}
	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			resp := pr.GetPortReachableDetails(e.ReqBody)
			assert.Equal(t, resp, e.ResponseBody)
		})
	}
}
