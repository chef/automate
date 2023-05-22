package portreachableservice

import (
	"crypto/tls"
	"crypto/x509"
	"errors"
	"fmt"
	"net"
	"net/http"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IPortReachableService interface {
	GetPortReachableDetails(models.PortReachableRequest) models.Checks
}

type PortReachableService struct {
	log     logger.Logger
	timeout time.Duration
}

func NewPortReachableService(log logger.Logger, timeout time.Duration) *PortReachableService {
	return &PortReachableService{
		log:     log,
		timeout: timeout,
	}
}

func (pr *PortReachableService) tcpClient(protocolType, host, port string) error {
	pr.log.Debug("URL: ", net.JoinHostPort(host, port))
	timeout := time.Second * pr.timeout
	// In TCP connection, if we are getting the connection it's sufficient to check the reachibility
	conn, err := net.DialTimeout(protocolType, net.JoinHostPort(host, port), timeout)
	if err != nil {
		return fmt.Errorf("dial failed: %w", err)
	}
	pr.log.Debug("Got the Connection Object")
	conn.Close()
	return nil
}

func (pr *PortReachableService) udpClient(protocolType, host, port string) error {
	pr.log.Debug("URL: ", net.JoinHostPort(host, port))
	udpServer, err := net.ResolveUDPAddr(protocolType, host+":"+port)
	if err != nil {
		return fmt.Errorf("ResolveUDPAddr failed: %w", err)
	}

	// NOTE: For UDP connection, even if you pass any IP where UDP server is not running still it will give you connection object.
	// Hence even after getting the conn object we need to write something and read something from UDP server.
	conn, err := net.DialUDP(protocolType, nil, udpServer)
	if err != nil {
		return fmt.Errorf("dial failed: %w", err)
	}
	pr.log.Debug("Got the Connection Object")
	defer conn.Close()

	// need to setDeadline for UDP client. Otherwise UDP client will stuck forever while waiting for something to read
	err = conn.SetDeadline(time.Now().Add(pr.timeout * time.Second))
	if err != nil {
		return fmt.Errorf("setdeadline error: %w", err)
	}

	// Preparing Data to send to UDP Server. Otherwise UDP server will wait forver for reading this data
	inputString := fmt.Sprintf("Hi %s Server, How are you doing?", protocolType)
	_, err = conn.Write([]byte(inputString))
	if err != nil {
		return fmt.Errorf("write error: %w", err)
	}

	// Reading the server response
	buffer := make([]byte, 2048)
	n, err := conn.Read(buffer)
	if err != nil {
		return fmt.Errorf("read error: %w", err)
	}
	pr.log.Debug("Server Response(if any): ", string(buffer[:n]))
	return nil
}

func (pr *PortReachableService) httpsClient(ip, cert string, port int) error {
	caCertPool := x509.NewCertPool()
	ok := caCertPool.AppendCertsFromPEM([]byte(cert))
	if !ok {
		return errors.New("certificate error")
	}
	client := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{
				RootCAs: caCertPool,
			},
		},
		Timeout: time.Second * pr.timeout,
	}
	url := fmt.Sprintf("https://%s:%d", ip, port)
	pr.log.Debug("URL: ", url)
	resp, err := client.Get(url)
	if err != nil {
		return fmt.Errorf("call error: %w", err)
	}
	pr.log.Debug("Response Status Code: ", resp.StatusCode)
	if resp.StatusCode != 200 {
		return fmt.Errorf("got response code: %d", resp.StatusCode)
	}
	return nil
}

func (pr *PortReachableService) GetPortReachableDetails(reqBody models.PortReachableRequest) models.Checks {
	var err error
	if reqBody.DestinationNodeServiceProtocol == constants.HTTPS {
		err = pr.httpsClient(reqBody.DestinationNodeIp, reqBody.RootCA, reqBody.DestinationNodePort)
	} else if reqBody.DestinationNodeServiceProtocol == constants.TCP {
		err = pr.tcpClient(reqBody.DestinationNodeServiceProtocol, reqBody.DestinationNodeIp, fmt.Sprint(reqBody.DestinationNodePort))
	} else if reqBody.DestinationNodeServiceProtocol == constants.UDP {
		err = pr.udpClient(reqBody.DestinationNodeServiceProtocol, reqBody.DestinationNodeIp, fmt.Sprint(reqBody.DestinationNodePort))
	}
	if err != nil {
		pr.log.Error(err.Error())
		return models.Checks{
			Title:         constants.PORT_REACHABLE,
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      fmt.Sprintf(constants.PORT_REACHABLE_ERROR_MSG, reqBody.DestinationNodeServiceProtocol, reqBody.DestinationNodeIp, reqBody.DestinationNodePort),
			ResolutionMsg: fmt.Sprintf(constants.PORT_REACHABLE_RESOLUTION_MSG, reqBody.DestinationNodePort, reqBody.DestinationNodeIp),
		}
	}

	return models.Checks{
		Title:         constants.PORT_REACHABLE,
		Passed:        true,
		SuccessMsg:    fmt.Sprintf(constants.PORT_REACHABLE_SUCCESS_MSG, reqBody.DestinationNodeServiceProtocol, reqBody.DestinationNodeIp, reqBody.DestinationNodePort),
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
}
