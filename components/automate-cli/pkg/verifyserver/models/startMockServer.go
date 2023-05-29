package models

import (
	"context"
	"net"
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
	ListenerHTTP HTTPsListener
	SignalChan   chan bool
	Protocol     string
}

type HTTPSServerResponse struct {
	Status string `json:"status"`
}

type HTTPsListener interface {
	Shutdown(ctx context.Context) error
}
