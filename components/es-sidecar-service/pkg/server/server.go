package server

import (
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
)

// EsSidecarServer is the interface to this component.
type EsSidecarServer struct {
	es            *elastic.Elastic
	backupsConfig *elastic.BackupsConfig
}

// NewEsSidecarServer creates a new EsSidecarServer instance.
func NewEsSidecarServer(es *elastic.Elastic, bc *elastic.BackupsConfig) *EsSidecarServer {
	return &EsSidecarServer{
		es:            es,
		backupsConfig: bc,
	}
}
