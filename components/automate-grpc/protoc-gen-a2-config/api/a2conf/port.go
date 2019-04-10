package a2conf

import "errors"

type PortBind interface {
	BindPort(name string, value uint16) error
	ListPorts() []PortInfo
	GetPort(name string, value uint16) (uint16, error)
}

// PortInfo describes a bindable port
type PortInfo struct {
	Name     string
	Default  uint16
	Protocol string
}

var ErrPortNotFound = errors.New("port not found")

func IsErrPortNotFound(err error) bool {
	return err == ErrPortNotFound
}
