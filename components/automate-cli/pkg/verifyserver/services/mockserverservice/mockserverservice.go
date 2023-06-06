package mockserverservice

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"sync"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

// MockServerService provides functionality to start mock servers.
type MockServersServiceImp struct {
	mu          sync.Mutex
	mockServers []*models.Server
	logger      logger.Logger
}

type MockServersService interface {
	Start(cfg *models.StartMockServerRequestBody) error
	Stop(cfg *models.StopMockServerRequestBody) error
}

func NewMockServersServiceImp(logger logger.Logger) *MockServersServiceImp {
	return &MockServersServiceImp{
		logger: logger,
	}
}

// Start starts a mock server of the given type and port.
func (s *MockServersServiceImp) Start(cfg *models.StartMockServerRequestBody) error {
	var err error
	switch cfg.Protocol {
	case constants.TCP:
		err = s.StartTCPServer(cfg.Port)
	case constants.UDP:
		err = s.StartUDPServer(cfg.Port)
	case constants.HTTP:
		err = s.StartHTTPServer(cfg.Port)
	case constants.HTTPS:
		err = s.StartHTTPSServer(cfg.Port, cfg.Cert, cfg.Key)
	default:
		err = errors.New("unsupported protocol")
	}
	if err != nil {
		return err
	}
	return nil
}

// Stop stops a mock server of the given type on given port.
func (s *MockServersServiceImp) Stop(cfg *models.StopMockServerRequestBody) error {
	var err error
	switch cfg.Protocol {
	case constants.TCP:
		err = s.StopTCPServer(cfg.Port, cfg.Protocol)
	case constants.UDP:
		err = s.StopUDPServer(cfg.Port, cfg.Protocol)
	case constants.HTTP:
		err = s.StopHTTPServer(cfg.Port, cfg.Protocol)
	case constants.HTTPS:
		err = s.StopHTTPSServer(cfg.Port, cfg.Protocol)
	default:
		err = errors.New("unsupported protocol")
	}
	if err != nil {
		return err
	}
	return nil
}

// IsMockServerRunning returns true if on given port and protocol any server is running.
func (s *MockServersServiceImp) IsMockServerRunningOnGivenPort(port int) bool {
	servers := s.mockServers

	for _, server := range servers {
		if server.Port == port {
			s.logger.Debug("Mock server is already running on port: ", port)
			return true
		}
	}
	return false
}

func (s *MockServersServiceImp) IsMockServerRunningOnGivenPortAndProctocol(port int, protocol string) bool {
	servers := s.mockServers

	for _, server := range servers {
		if server.Port == port && server.Protocol == protocol {
			return true
		}
	}
	s.logger.Debug("No Mock server is running on port: ", port)
	return false
}

func (s *MockServersServiceImp) StartTCPServer(port int) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.IsMockServerRunningOnGivenPort(port) {
		return errors.New("port unavailable")
	}

	// create a TCP listener on the specified port and
	// save the listener instance in the handler struct
	listener, err := net.Listen(constants.TCP, fmt.Sprintf(":%d", port))

	if err != nil {
		s.logger.Error("Error listening to the port:", err.Error())
		return errors.New(err.Error())
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
			defer conn.Close()
			conn.Write([]byte("ok"))
		}
	}()

	server := &models.Server{
		Port:        port,
		ListenerTCP: listener,
		ListenerUDP: nil,
		Protocol:    constants.TCP,
		SignalChan:  signalChan,
	}
	s.mockServers = append(s.mockServers, server)

	return nil
}

func (s *MockServersServiceImp) StartUDPServer(port int) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.IsMockServerRunningOnGivenPort(port) {
		return errors.New("port unavailable")
	}

	addr, err := net.ResolveUDPAddr(constants.UDP, fmt.Sprintf(":%d", port))
	if err != nil {
		return err
	}

	conn, err := net.ListenUDP(constants.UDP, addr)
	if err != nil {
		return errors.New(err.Error())
	}

	s.logger.Infof("UDP server started on port %d\n", port)
	//Create a channel to listen for close signal
	signalChan := make(chan bool, 1)

	go func() {
		for {
			buf := make([]byte, 1024)
			_, addr, err := conn.ReadFromUDP(buf)
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
			conn.WriteToUDP([]byte("ok"), addr)
		}
	}()

	server := &models.Server{
		Port:        port,
		ListenerTCP: nil,
		ListenerUDP: conn,
		Protocol:    constants.UDP,
		SignalChan:  signalChan,
	}
	s.mockServers = append(s.mockServers, server)
	return nil
}

func (s *MockServersServiceImp) StartHTTPServer(port int) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.IsMockServerRunningOnGivenPort(port) {
		return errors.New("port unavailable")
	}

	m := http.NewServeMux()

	// Create the HTTPS server
	server := &http.Server{
		Addr:    fmt.Sprintf(":%d", port),
		Handler: m,
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
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			s.logger.Errorln("Error starting HTTP server: ", err)
			serverStarted <- err
		} else {
			s.logger.Infof("HTTP server stopped gracefully")
		}
	}(serverErr)

	timeout := time.After(100 * time.Millisecond)

	// Waiting for 100 milli second in case of any error before returning
	select {
	case err := <-serverErr:
		return err
	case <-timeout:
		s.logger.Infof("HTTP server started succesfully on port %d", port)
		server := &models.Server{
			Port:         port,
			ListenerTCP:  nil,
			ListenerUDP:  nil,
			ListenerHTTP: server,
			Protocol:     constants.HTTP,
		}
		s.mockServers = append(s.mockServers, server)
		return nil
	}
}

func (s *MockServersServiceImp) StartHTTPSServer(port int, cert string, key string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.IsMockServerRunningOnGivenPort(port) {
		return errors.New("port unavailable")
	}

	// Load the TLS certificate and private key
	tlsCert, err := tls.X509KeyPair([]byte(cert), []byte(key))
	if err != nil {
		s.logger.Errorf("Certificate error: %v\n", err)
		return err
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
		return err
	case <-timeout:
		s.logger.Infof("HTTPS server started succesfully on port %d", port)
		server := &models.Server{
			Port:         port,
			ListenerTCP:  nil,
			ListenerUDP:  nil,
			ListenerHTTP: server,
			Protocol:     constants.HTTPS,
		}
		s.mockServers = append(s.mockServers, server)
		return nil
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

func (s *MockServersServiceImp) StopTCPServer(port int, protocol string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.IsMockServerRunningOnGivenPortAndProctocol(port, protocol) {
		return errors.New("no mock server found")
	}

	server, updatedMockServers := s.getMockServer(port, protocol, s.mockServers)

	s.logger.Info("Stop TCP server request recieved")
	server.SignalChan <- true
	if err := server.ListenerTCP.Close(); err != nil {
		s.logger.Error("Error while stopping TCP server: ", err.Error())
		return err
	}

	close(server.SignalChan)
	s.mockServers = updatedMockServers
	return nil
}

func (s *MockServersServiceImp) StopUDPServer(port int, protocol string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.IsMockServerRunningOnGivenPortAndProctocol(port, protocol) {
		return errors.New("no mock server found")
	}

	server, updatedMockServers := s.getMockServer(port, protocol, s.mockServers)

	s.logger.Info("Stop UDP server request recieved")
	server.SignalChan <- true
	if err := server.ListenerUDP.Close(); err != nil {
		s.logger.Error("Error while stopping UDP server: ", err.Error())
		return err
	}

	close(server.SignalChan)
	s.mockServers = updatedMockServers
	return nil
}

func (s *MockServersServiceImp) StopHTTPServer(port int, protocol string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.IsMockServerRunningOnGivenPortAndProctocol(port, protocol) {
		return errors.New("no mock server found")
	}

	server, updatedMockServers := s.getMockServer(port, protocol, s.mockServers)

	s.logger.Info("Stop HTTP server request recieved")
	if err := server.ListenerHTTP.Shutdown(context.Background()); err != nil {
		s.logger.Error("Error while stopping HTTP server: ", err.Error())
		return err
	}

	s.mockServers = updatedMockServers
	return nil
}

func (s *MockServersServiceImp) StopHTTPSServer(port int, protocol string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.IsMockServerRunningOnGivenPortAndProctocol(port, protocol) {
		return errors.New("no mock server found")
	}

	server, updatedMockServers := s.getMockServer(port, protocol, s.mockServers)

	s.logger.Info("Stop HTTPS server request recieved")
	if err := server.ListenerHTTP.Shutdown(context.Background()); err != nil {
		s.logger.Error("Error while stopping HTTPS server: ", err.Error())
		return err
	}

	s.mockServers = updatedMockServers
	return nil
}

func (s *MockServersServiceImp) getMockServer(port int, protocol string, mockServers []*models.Server) (*models.Server, []*models.Server) {
	var server *models.Server
	var updatedMockServers []*models.Server

	for _, mockServer := range mockServers {
		if mockServer.Port == port && mockServer.Protocol == protocol {
			server = mockServer
		} else {
			updatedMockServers = append(updatedMockServers, mockServer)
		}
	}

	return server, updatedMockServers
}
