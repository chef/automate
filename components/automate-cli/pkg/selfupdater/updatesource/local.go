package updatesource

import (
	"context"

	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/executablecache"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

type local struct {
	executableCache executablecache.ExecutableCache
}

// Local returns an UpdateSource that only knows how to check for available
// updates that are locally cached using the ExecutableCache. This UpdateSource
// is used for making sure we start off by running the latest executable we have
func Local(executableCache executablecache.ExecutableCache) UpdateSource {
	return &local{
		executableCache: executableCache,
	}
}

func (l *local) DesiredVersion(context.Context) (string, error) {
	_, v := l.executableCache.Latest()

	if v == "" {
		return "", ErrUpdateNotAvailable
	}

	return v, nil
}

func (*local) FetchLatest(context.Context) ([]byte, string, error) {
	// If this function gets called, it is a bug. There is no need to
	// The executable will already be cached on disk and can just be wrapped
	return nil, "", status.New(status.UpdateExecError, "The executable is already cached locally")
}
