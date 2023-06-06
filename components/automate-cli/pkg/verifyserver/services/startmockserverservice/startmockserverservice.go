package startmockserverservice

import (
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"time"

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
	SetMockServers(servers []*models.Server)
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

func (s *MockServerService) SetMockServers(servers []*models.Server) {
	s.MockServers = servers
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
	listener, err := net.Listen(constants.TCP, fmt.Sprintf(":%d", port))

	if err != nil {

		s.logger.Error("Error listening to the port:", err.Error())
		return nil, errors.New(err.Error())
	}

	s.logger.Infof("TCP server started on port %d\n", port)

	//Create a channel to listen for close signal
	signalChan := make(chan bool, 1)

	go func() {
		for {
			conn, err := listener.Accept()
			if err != nil {
				select {
				case <-signalChan:
					s.logger.Infoln("Stopping TCP server....")
					return
				default:
					s.logger.Errorln("Error accepting connection:", err)
					return
				}
			}
			s.HandleTCPRequest(conn)
		}
	}()

	return &models.Server{
		Port:        port,
		ListenerTCP: listener,
		ListenerUDP: nil,
		Protocol:    constants.TCP,
		SignalChan:  signalChan,
	}, nil
}

func (s *MockServerService) HandleTCPRequest(conn net.Conn) {
	defer conn.Close()

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
	//Create a channel to listen for close signal
	signalChan := make(chan bool, 1)

	go func() {
		for {
			buf := make([]byte, 1024)
			n, addr, err := conn.ReadFromUDP(buf)
			if err != nil {
				select {
				case <-signalChan:
					s.logger.Infoln("Stopping UDP server....")
					return
				default:
					s.logger.Errorln("Error receiving message:", err)
					return
				}
			}
			s.HandleUDPRequest(conn, addr, buf[:n])
		}
	}()

	return &models.Server{
		Port:        port,
		ListenerTCP: nil,
		ListenerUDP: conn,
		Protocol:    constants.UDP,
		SignalChan:  signalChan,
	}, nil
}

func (s *MockServerService) HandleUDPRequest(conn *net.UDPConn, addr *net.UDPAddr, buf []byte) {
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
		MinVersion:   tls.VersionTLS12,
	}
	m := http.NewServeMux()

	// Create the HTTPS server
	server := &http.Server{
		Addr:      fmt.Sprintf(":%d", port),
		Handler:   m,
		TLSConfig: config,
	}

	// Handle response
	m.HandleFunc("/", func(rw http.ResponseWriter, r *http.Request) {
		privateIP := GetPrivateIP()

		rw.Header().Set("x-server-ip", privateIP)
		response := models.HTTPSServerResponse{
			Status: "ok",
		}

		jsonData, _ := json.Marshal(response)

		rw.Header().Set("Content-Type", "application/json")

		rw.Write(jsonData)
	})

	serverErr := make(chan error)
	// Start the HTTPS server
	go func(serverStarted chan error) {
		if err = server.ListenAndServeTLS("", ""); err != nil && err != http.ErrServerClosed {
			s.logger.Errorln("Error starting HTTPS server: ", err)
			serverStarted <- err
		} else {
			s.logger.Infof("HTTPS server stopped gracefully")
		}
	}(serverErr)

	timeout := time.After(100 * time.Millisecond)

	// Waiting for 100 milli second in case of any error before returning
	select {
	case err := <-serverErr:
		return nil, err
	case <-timeout:
		s.logger.Infof("HTTPS server started succesfully on port %d", port)
		return &models.Server{
			Port:         port,
			ListenerTCP:  nil,
			ListenerUDP:  nil,
			ListenerHTTP: server,
			Protocol:     constants.HTTPS,
		}, nil
	}
}

// Function to get the private IP address of the server
func GetPrivateIP() string {
	// Get all network interfaces
	interfaces, err := net.Interfaces()
	if err != nil {
		return "Unknown"
	}

	// Iterate through the interfaces and find the first non-loopback, non-virtual interface with an IP address
	for _, iface := range interfaces {
		if isPhysicalInterface(iface) {
			ip := getInterfaceIP(iface)
			if ip != "" {
				return ip
			}
		}
	}
	return "Unknown"
}

func isPhysicalInterface(iface net.Interface) bool {
	return iface.Flags&net.FlagLoopback == 0 && iface.Flags&net.FlagUp != 0
}

func getInterfaceIP(iface net.Interface) string {
	addrs, err := iface.Addrs()
	if err != nil {
		return ""
	}

	for _, addr := range addrs {
		ip, ok := getIPAddress(addr)
		if ok {
			return ip
		}
	}
	return ""
}

func getIPAddress(addr net.Addr) (string, bool) {
	if ipnet, ok := addr.(*net.IPNet); ok && !ipnet.IP.IsLoopback() && ipnet.IP.To4() != nil {
		return ipnet.IP.String(), true
	}

	if ipaddr, ok := addr.(*net.IPAddr); ok && !ipaddr.IP.IsLoopback() && ipaddr.IP.To4() != nil {
		return ipaddr.IP.String(), true
	}
	return "", false
}
