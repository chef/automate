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
	LOCALHOST                 = portreachableservice.LOCALHOST
	SERVER_NOT_RUNNING_IP     = portreachableservice.SERVER_NOT_RUNNING_IP
	INVALID_FORMAT_RUNNING_IP = portreachableservice.INVALID_FORMAT_RUNNING_IP
	SERVER_CRT                = portreachableservice.SERVER_CRT
	SERVER_KEY                = portreachableservice.SERVER_KEY
	TIMEOUT                   = portreachableservice.TIMEOUT
	CA_CERT                   = portreachableservice.CA_CERT
	DIFF_SERVER_ROOT_CA       = portreachableservice.DIFF_SERVER_ROOT_CA
	WRONG_ROOT_CA             = portreachableservice.WRONG_ROOT_CA
)

func TestPortReachableDetails(t *testing.T) {
	pr := portreachableservice.NewPortReachableService(logger.NewTestLogger(), time.Duration(TIMEOUT))
	assert.NotNil(t, pr)
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
	tcpTestPort := 3069
	udpTestPort := 3070
	httpsTestPort := 3071
	httpsTestPort2 := 3072

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

	httpsMockServerWithDiffStatusCode := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadGateway)
		w.Write([]byte("OK"))
	}))
	err = startHTTPSMockServerOnCustomPort(httpsMockServerWithDiffStatusCode, strconv.Itoa(httpsTestPort2))
	assert.NoError(t, err)
	defer httpsMockServerWithDiffStatusCode.Close()

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
				Title:         constants.PORT_REACHABLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(constants.PORT_REACHABLE_SUCCESS_MSG, constants.TCP, LOCALHOST, tcpTestPort),
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
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.TCP, SERVER_NOT_RUNNING_IP, tcpTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, tcpTestPort, SERVER_NOT_RUNNING_IP),
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
				Title:         constants.PORT_REACHABLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(constants.PORT_REACHABLE_SUCCESS_MSG, constants.UDP, LOCALHOST, udpTestPort),
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
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.UDP, SERVER_NOT_RUNNING_IP, udpTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, udpTestPort, SERVER_NOT_RUNNING_IP),
			},
		},
		{
			"Wrong Format IP for UDP server",
			models.PortReachableRequest{
				DestinationNodeIp:              INVALID_FORMAT_RUNNING_IP,
				DestinationNodePort:            udpTestPort,
				DestinationNodeServiceProtocol: constants.UDP,
				RootCA:                         "",
			},
			models.Checks{
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.UDP, INVALID_FORMAT_RUNNING_IP, udpTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, udpTestPort, INVALID_FORMAT_RUNNING_IP),
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
				Title:         constants.PORT_REACHABLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(constants.PORT_REACHABLE_SUCCESS_MSG, constants.HTTPS, LOCALHOST, httpsTestPort),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			"HTTPS Server running there but giving different server rootCA",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            httpsTestPort,
				DestinationNodeServiceProtocol: constants.HTTPS,
				RootCA:                         DIFF_SERVER_ROOT_CA,
			},
			models.Checks{
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.HTTPS, LOCALHOST, httpsTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, httpsTestPort, LOCALHOST),
			},
		},
		{
			"HTTPS Server running there but giving wrong rootCA",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            httpsTestPort,
				DestinationNodeServiceProtocol: constants.HTTPS,
				RootCA:                         WRONG_ROOT_CA,
			},
			models.Checks{
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.HTTPS, LOCALHOST, httpsTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, httpsTestPort, LOCALHOST),
			},
		},
		{
			"HTTPS Server running there but giving wrong response status code",
			models.PortReachableRequest{
				DestinationNodeIp:              LOCALHOST,
				DestinationNodePort:            httpsTestPort2,
				DestinationNodeServiceProtocol: constants.HTTPS,
				RootCA:                         CA_CERT,
			},
			models.Checks{
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.HTTPS, LOCALHOST, httpsTestPort2),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, httpsTestPort2, LOCALHOST),
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
				Title:         constants.PORT_REACHABLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, constants.HTTPS, SERVER_NOT_RUNNING_IP, httpsTestPort),
				ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, httpsTestPort, SERVER_NOT_RUNNING_IP),
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
