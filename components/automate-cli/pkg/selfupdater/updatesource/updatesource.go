package updatesource

import (
	"context"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

// ErrUpdateNotAvailable is returned when no versions are available
var ErrUpdateNotAvailable = status.New(status.UpdateExecError, "A chef-automate CLI is not available")

// UpdateSource describes a place where we can get a version and executable
type UpdateSource interface {
	// DesiredVersion returns the version the update source thinks we should be
	// running
	DesiredVersion(ctx context.Context) (string, error)
	// FetchLatest returns the binary for the latest executable along with the
	// version. Because of time, the version returned in DesiredVersion can end
	// up being different than what FetchLatest says the version is.
	FetchLatest(ctx context.Context) ([]byte, string, error)
}
