package tcp

import (
	"net"
)

type MockTCPListener struct {
	AcceptFunc func() (net.Conn, error)
	CloseFunc  func() error
	AddrFunc   func() net.Addr
}

func (l *MockTCPListener) Accept() (net.Conn, error) {
	return l.AcceptFunc()
}

func (l *MockTCPListener) Close() error {
	return l.CloseFunc()
}

func (l *MockTCPListener) Addr() net.Addr {
	return l.AddrFunc()
}
