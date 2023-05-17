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
	Protocol     string
}
