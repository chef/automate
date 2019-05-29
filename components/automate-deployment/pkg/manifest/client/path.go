package client

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

// A Path client retrieves and parses a Manifest from files in a
// directory or a file
type Path struct {
	path string
}

// NewPathClient returns a Directory client
func NewPathClient(path string) *Path {
	return &Path{
		path: path,
	}
}

// GetCurrentManifest returns an A2 manifest.
func (d *Path) GetCurrentManifest(_ context.Context, channel string) (*manifest.A2, error) {
	stat, err := os.Stat(d.path)
	if err != nil {
		return nil, manifest.NewErrNoSuchManifest(err)
	}
	path := d.path

	if stat.IsDir() {
		path = fmt.Sprintf("%s/%s.json", d.path, channel)
	}
	return d.manifestFromPath(path)
}

// GetCurrentManifest returns an A2 manifest.
func (d *Path) GetManifest(_ context.Context, release string) (*manifest.A2, error) {
	stat, err := os.Stat(d.path)
	if err != nil {
		return nil, manifest.NewErrNoSuchManifest(err)
	}
	path := d.path

	if stat.IsDir() {
		path = fmt.Sprintf("%s/%s.json", d.path, release)
	}

	return d.manifestFromPath(path)
}

func (d *Path) manifestFromPath(path string) (*manifest.A2, error) {
	body, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, manifest.NewErrNoSuchManifest(err)
	}

	m, err := parser.ManifestFromBytes(body)
	if err != nil {
		return nil, err
	}

	if m.HartOverrides == nil {
		m.HartOverrides = []habpkg.Hart{}
	}

	return m, nil
}
