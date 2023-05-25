package udp

import (
	"net"
	"time"
)

type MockUDPListener struct {
	ReadFromFunc         func(p []byte) (n int, addr net.Addr, err error)
	WriteToFunc          func(p []byte, addr net.Addr) (n int, err error)
	LocalAddrFunc        func() net.Addr
	SetDeadlineFunc      func(t time.Time) error
	SetReadDeadlineFunc  func(t time.Time) error
	SetWriteDeadlineFunc func(t time.Time) error
	CloseFunc            func() error
}

func (l *MockUDPListener) Close() error {
	return l.CloseFunc()
}

func (l *MockUDPListener) ReadFrom(p []byte) (n int, addr net.Addr, err error) {
	return l.ReadFromFunc(p)
}

func (l *MockUDPListener) WriteTo(p []byte, addr net.Addr) (n int, err error) {
	return l.WriteToFunc(p, addr)
}

func (l *MockUDPListener) LocalAddr() net.Addr {
	return l.LocalAddrFunc()
}

func (l *MockUDPListener) SetDeadline(t time.Time) error {
	return l.SetDeadlineFunc(t)
}

func (l *MockUDPListener) SetReadDeadline(t time.Time) error {
	return l.SetReadDeadlineFunc(t)
}

func (l *MockUDPListener) SetWriteDeadline(t time.Time) error {
	return l.SetWriteDeadlineFunc(t)
}
