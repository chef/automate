package startmockserverservice

import (
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

// MockServerService provides functionality to start mock servers.
type MockServerService struct {
	MockServers []*models.Server
	logger      logger.Logger
}

type IStartMockServersService interface {
	StartMockServer(cfg *models.StartMockServerRequestBody) error
	GetMockServers() []*models.Server
}

func New(logger logger.Logger) IStartMockServersService {
	return &MockServerService{
		logger: logger,
	}
}

// func to get mock server list
func (s *MockServerService) GetMockServers() []*models.Server {
	return s.MockServers
}

// StartMockServer starts a mock server of the given type and port.
func (servers *MockServerService) StartMockServer(cfg *models.StartMockServerRequestBody) error {
	var myServer *models.Server
	var err error
	switch cfg.Protocol {
	case constants.TCP:
		myServer, err = servers.StartTCPServer(cfg.Port)
	case constants.UDP:
		myServer, err = servers.StartUDPServer(cfg.Port)
	case constants.HTTPS:
		myServer, err = servers.StartHTTPSServer(cfg.Port, cfg.Cert, cfg.Key)
	default:
		err = errors.New("unsupported protocol")
	}
	if err != nil {
		return err
	}
	servers.MockServers = append(servers.MockServers, myServer)
	return nil
}

func (s *MockServerService) StartTCPServer(port int) (*models.Server, error) {
	// create a TCP listener on the specified port and
	// save the listener instance in the handler struct
	listener, err := net.Listen(constants.TCP, fmt.Sprintf("localhost:%d", port))

	if err != nil {

		s.logger.Error("Error listening to the port:", err.Error())
		return nil, errors.New(err.Error())
	}

	s.logger.Infof("TCP server started on port %d\n", port)

	go func() {
		for {
			conn, err := listener.Accept()
			if err != nil {
				s.logger.Errorln("Error accepting connection:", err)
				return
			}
			s.HandleTCPRequest(conn)
		}
	}()

	return &models.Server{
		Port:        port,
		ListenerTCP: listener,
		ListenerUDP: nil,
		Protocol:    constants.TCP,
	}, nil
}

func (s *MockServerService) HandleTCPRequest(conn net.Conn) {
	defer conn.Close()

	// Following is kept for future use incase the server needs to send back the incoming data

	// buf := make([]byte, 1024)
	// _, err := conn.Read(buf)
	// if err != nil {
	// 	s.logger.Infoln("Error reading data from connection:", err)
	// 	return
	// }
	// responseStr := fmt.Sprintf("Your message is: %v.", string(buf[:]))

	conn.Write([]byte("ok"))

}

func (s *MockServerService) StartUDPServer(port int) (*models.Server, error) {
	addr, err := net.ResolveUDPAddr(constants.UDP, fmt.Sprintf(":%d", port))
	if err != nil {
		return nil, err
	}

	conn, err := net.ListenUDP(constants.UDP, addr)
	if err != nil {
		return nil, errors.New(err.Error())
	}

	s.logger.Infof("UDP server started on port %d\n", port)

	go func() {
		for {
			buf := make([]byte, 1024)
			n, addr, err := conn.ReadFromUDP(buf)
			if err != nil {
				s.logger.Errorln("Error receiving message:", err)
				return
			}
			s.HandleUDPRequest(conn, addr, buf[:n])
		}
	}()

	return &models.Server{
		Port:        port,
		ListenerTCP: nil,
		ListenerUDP: conn,
		Protocol:    constants.UDP,
	}, nil
}

func (s *MockServerService) HandleUDPRequest(conn *net.UDPConn, addr *net.UDPAddr, buf []byte) {
	// Following is kept for future use incase the server needs to send back the incoming data
	// s.logger.Info("UDP request received from %v", addr)
	// responseStr := []byte(fmt.Sprintf("Your message is: %v.", string(buf[:])))
	conn.WriteToUDP([]byte("ok"), addr)
}

func (s *MockServerService) StartHTTPSServer(port int, cert string, key string) (*models.Server, error) {

	// Load the TLS certificate and private key
	tlsCert, err := tls.X509KeyPair([]byte(cert), []byte(key))
	if err != nil {
		s.logger.Errorf("Certificate error: %v\n", err)
		return nil, err
	}

	// Create the TLS configuration for the server
	config := &tls.Config{
		Certificates: []tls.Certificate{tlsCert},
		MinVersion:   tls.VersionTLS13,
	}

	// Create the HTTPS server
	server := &http.Server{
		Addr:      fmt.Sprintf(":%d", port),
		TLSConfig: config,
	}

	// Handle response
	http.HandleFunc("/", func(rw http.ResponseWriter, r *http.Request) {
		rw.Write([]byte("ok\n"))
	})

	// Start the HTTPS server
	go func() {
		err = server.ListenAndServeTLS("", "")
		if err != nil && err != http.ErrServerClosed {
			s.logger.Errorln("Error starting HTTPS server: ", err)
			return
		}
		s.logger.Infof("HTTPS server started on port %d\n", port)
	}()

	return &models.Server{
		Port:         port,
		ListenerTCP:  nil,
		ListenerUDP:  nil,
		ListenerHTTP: server,
		Protocol:     constants.HTTPS,
	}, nil
}
