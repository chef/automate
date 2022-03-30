package client

import (
	"context"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

// InMemory manifest client reads the manifest from bytes in memory
type InMemory struct {
	data []byte
}

// NewInMemoryClient returns a InMemory manifest client
func NewInMemoryClient(data []byte) *InMemory {
	return &InMemory{
		data: data,
	}
}

// GetCurrentManifest parses the manifest in memory
func (d *InMemory) GetCurrentManifest(_ context.Context, _ string) (*manifest.A2, error) {
	m, err := parser.ManifestFromBytes(d.data)
	if err != nil {
		return nil, err
	}

	if m.HartOverrides == nil {
		m.HartOverrides = []habpkg.Hart{}
	}

	return m, nil
}

// GetManifest parses the manifest in memory
func (d *InMemory) GetManifest(_ context.Context, release string) (*manifest.A2, error) {
	m, err := parser.ManifestFromBytes(d.data)
	if err != nil {
		return nil, err
	}

	if m.Build != release {
		return nil, err
	}

	if m.HartOverrides == nil {
		m.HartOverrides = []habpkg.Hart{}
	}

	return m, nil
}
