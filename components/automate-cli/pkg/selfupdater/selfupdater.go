package selfupdater

import (
	"context"
	"os"
	"os/exec"
	"syscall"

	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/executablecache"
	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/updatesource"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/version"
)

// NextExecutable represents the next command to execute when self upgrading.
// If one is not available, Available() will return false
type NextExecutable struct {
	Version string
	Cmd     *exec.Cmd
}

// ReExec runs the next executable and exits. This function panics if a
// a next executable is not available. It is the caller's responsibility
// to verify that with the Available() function
func (nextExe *NextExecutable) ReExec(extraEnv []string) error {
	if !nextExe.Available() {
		panic("There is nothing to reexec")
	}

	nextExe.Cmd.Env = os.Environ()
	if extraEnv != nil && len(extraEnv) > 0 {
		nextExe.Cmd.Env = append(nextExe.Cmd.Env, extraEnv...)
	}

	if err := nextExe.Cmd.Run(); err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			if waitStatus, ok := exitError.Sys().(syscall.WaitStatus); ok {
				os.Exit(waitStatus.ExitStatus())
			} else {
				os.Exit(status.UnknownError)
			}
		} else {
			return status.Wrap(err, status.UpdateExecError, "Execution of auto-updated chef-automate binary failed")
		}
	}
	os.Exit(0)
	return nil
}

// Available returns true if a next executable can be run
func (nextExe *NextExecutable) Available() bool {
	return nextExe != nil && nextExe.Cmd != nil
}

// SelfUpdater gets the updated executables
type SelfUpdater interface {
	// NextExecutable returns the next executable to exec if one is available.
	// One not being available is not considered an error
	NextExecutable(ctx context.Context) (NextExecutable, error)
}

type selfUpdater struct {
	executableCache executablecache.ExecutableCache
	updateSource    updatesource.UpdateSource
	myVersion       string
}

// NewSelfUpdater returns a new self updater. It will fetch updates from the
// update source and store them in the given executable cache
func NewSelfUpdater(updateSource updatesource.UpdateSource, executableCache executablecache.ExecutableCache) SelfUpdater {
	return &selfUpdater{
		updateSource:    updateSource,
		executableCache: executableCache,
		myVersion:       version.BuildTime,
	}
}

func (s *selfUpdater) NextExecutable(ctx context.Context) (NextExecutable, error) {
	shouldUpdate, cliVersion, err := s.shouldUpdate(ctx)

	if err != nil {
		return NextExecutable{}, err
	}

	if !shouldUpdate {
		// An update not being available returns a NextExecutable that
		// is not available
		return NextExecutable{}, nil
	}

	isCached, err := s.executableCache.Exists(cliVersion)

	if err != nil {
		return NextExecutable{}, err
	}

	if isCached {
		return NextExecutable{
			Version: cliVersion,
			Cmd:     s.executableCache.AsCmd(cliVersion),
		}, nil
	}

	return s.fetchUpdateAndCache(ctx)
}

func (s *selfUpdater) shouldUpdate(ctx context.Context) (bool, string, error) {
	v, err := s.updateSource.DesiredVersion(ctx)
	if err != nil {
		return false, "", err
	}
	return v != s.myVersion, v, nil
}

func (s *selfUpdater) fetchUpdateAndCache(ctx context.Context) (NextExecutable, error) {
	data, v, err := s.updateSource.FetchLatest(ctx)
	if err != nil {
		return NextExecutable{}, err
	}

	cmd, err := s.executableCache.Store(v, data)
	if err != nil {
		return NextExecutable{}, err
	}

	return NextExecutable{
		Version: v,
		Cmd:     cmd,
	}, nil
}
