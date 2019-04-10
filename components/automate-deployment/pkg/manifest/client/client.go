package client

import (
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

// NewDefaultClient is a convenience constructor that
// returns a ReleaseManifestProvider.
func NewDefaultClient(manifestPath string) manifest.ReleaseManifestProvider {
	if manifestPath != "" {
		return NewPathClient(manifestPath)
	}
	return NewHTTPClient()
}
