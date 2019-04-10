package backup

import (
	"fmt"
)

// ESSidecarConnInfo represents how to connect to the sidecar
type ESSidecarConnInfo struct {
	Host string
	Port int32
}

// Address returns an address that can be passed to grpc
func (e ESSidecarConnInfo) Address() string {
	return fmt.Sprintf("%s:%d", e.Host, e.Port)
}
