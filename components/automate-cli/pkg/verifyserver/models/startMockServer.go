package models

import (
	"net"
	"net/http"
)

// StartMockServerRequestBody contains the configuration for starting a mock server.
type StartMockServerRequestBody struct {
	Port     int
	Protocol string
	Cert     string
	Key      string
}

type Server struct {
	Port         int
	ListenerTCP  net.Listener
	ListenerUDP  net.PacketConn
	ListenerHTTP *http.Server
	SignalChan   chan bool
	Protocol     string
}

type HTTPSServerResponse struct {
	Private_IP string `json:"private_ip"`
	Status     string `json:"status"`
}
