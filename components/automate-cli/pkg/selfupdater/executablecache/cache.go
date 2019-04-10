package executablecache

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/sirupsen/logrus"
)

// ErrNoCacheDir is returned if a suitable cache directory was not found
var ErrNoCacheDir = status.New(status.FileAccessError, `Executable cache directory not available.
Make sure binaries can be executed from $HOME/.chef-automate/executable-cache
or set CHEF_AUTOMATE_HOME to a suitable directory.`)

// ExecutableCache represents something that can cache chef-automate executables
type ExecutableCache interface {
	// Store stores the data given as an executable. It can be retrieved
	// using version as the key.
	Store(version string, data []byte) (*exec.Cmd, error)
	// Exists returns true if the given executable exists
	Exists(version string) (bool, error)
	// AsCmd returns the command for the executable version. The caller
	// is responsible for checking if it exists before making this call
	AsCmd(version string) *exec.Cmd
	// Latest returns the latest version available
	Latest() (*exec.Cmd, string)
}

type executableCache struct {
	executableDirectory string
}

// Opt are functional options for executableCache
type Opt func(*executableCache)

// WithCacheDir sets the cache directory for the executable cache
// This function is used by the tests. It is up to the caller to
// make sure that the directory exists before calling Store
func WithCacheDir(cacheDir string) Opt {
	return func(ec *executableCache) {
		ec.executableDirectory = cacheDir
	}
}

// New creates a new ExecutableCache
func New(opts ...Opt) ExecutableCache {
	ec := &executableCache{}

	for _, o := range opts {
		o(ec)
	}

	if ec.executableDirectory == "" {
		p, err := makeAHome()
		if err != nil {
			logrus.WithError(err).Debug("Couldn't make directory to hold cached executables")
		} else {
			ec.executableDirectory = p
		}
	}
	return ec
}

func (cache *executableCache) Store(version string, data []byte) (*exec.Cmd, error) {
	if cache.executableDirectory == "" {
		return nil, ErrNoCacheDir
	}

	tmpPath := cache.tmpExecutablePath(version)
	execPath := cache.executablePath(version)

	err := ioutil.WriteFile(tmpPath, data, 0755)

	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "Could not write CLI executable")
	}

	err = os.Rename(tmpPath, execPath)
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "Could not rename CLI executable")
	}

	return cache.AsCmd(version), nil
}

func (cache *executableCache) Exists(version string) (bool, error) {
	if cache.executableDirectory == "" {
		return false, nil
	}

	_, err := os.Stat(cache.executablePath(version))
	if err != nil {
		if os.IsNotExist(err) {
			return false, nil
		}
		return false, status.Annotate(err, status.FileAccessError)
	}
	return true, nil
}

func (cache *executableCache) AsCmd(version string) *exec.Cmd {
	if cache.executableDirectory == "" {
		return nil
	}

	cmd := exec.Command(cache.executablePath(version))
	cmd.Args = os.Args
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd
}

func (cache *executableCache) Latest() (*exec.Cmd, string) {
	if cache.executableDirectory == "" {
		return nil, ""
	}

	matches, err := filepath.Glob(filepath.Join(cache.executableDirectory, "*"))
	if err != nil {
		logrus.WithError(err).Debug("Glob shouldn't have returned an error")
	}

	if len(matches) > 0 {
		sort.Sort(sort.Reverse(sort.StringSlice(matches)))
		v := path.Base(matches[0])
		return cache.AsCmd(v), v
	}
	return nil, ""
}

func (cache *executableCache) executablePath(version string) string {
	return path.Join(cache.executableDirectory, version)
}

func (cache *executableCache) tmpExecutablePath(version string) string {
	return path.Join(cache.executableDirectory, fmt.Sprintf(".tmp.%s", version))
}

func makeAHome() (string, error) {
	var p string
	if v, ok := os.LookupEnv("CHEF_AUTOMATE_HOME"); ok {
		p = path.Join(v, "executable-cache")
	} else if v, ok := os.LookupEnv("HOME"); ok {
		p = path.Join(v, ".chef-automate", "executable-cache")
	}

	if p == "" {
		return "", ErrNoCacheDir
	}

	err := os.MkdirAll(p, 0755)
	return p, err
}
