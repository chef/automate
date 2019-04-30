package target

import (
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/platform/command"
)

var stdHabOptions = []command.Opt{
	// Don't emit ANSI color escape codes
	command.Envvar("HAB_NOCOLORING", "true"),
	// Don't use progress bars in output
	command.Envvar("HAB_NONINTERACTIVE", "true"),
	// Don't prompt for license acceptance
	command.Envvar("HAB_LICENSE", "accept-no-persist"),
}

// A HabCmd runs the `hab` command-line tool with a standard set of
// options.
type HabCmd interface {
	// InstallPackage installs an Installable habitat package
	// (a hartifact or a package from the Depot)
	InstallPackage(habpkg.Installable, string) (string, error)
	// IsInstalled returns true if the specified package is
	// installed and false otherwise.  An error is returned when
	// the underlying habitat commands have failed.
	IsInstalled(habpkg.VersionedPackage) (bool, error)
	// BinlinkPackage binlinks a binary in the given Habitat
	// package. An error is returned if the underlying hab command
	// failed.
	BinlinkPackage(habpkg.VersionedPackage, string) (string, error)

	// LoadService loads the given habpkg.VersionedPackage as a service
	// with the provided options.
	LoadService(habpkg.VersionedPackage, ...LoadOption) (string, error)
	// UnloadService unloads a given habpkg.VersionedPackage
	UnloadService(habpkg.VersionedPackage) (string, error)
	// StartService starts an already-loaded service identified by
	// the given habpkg.VersionedPackage.
	StartService(habpkg.VersionedPackage) (string, error)
	// StopService stops an already-loaded service identified by
	// the given habpkg.VersionedPackage.
	StopService(habpkg.VersionedPackage) (string, error)

	// Terminate supervisor, does not block
	SupTerm() error
}

type LoadOption func([]string) []string

// Binds is a LoadOption that applies the passed bind to the service's
// load command line arguments.
func Binds(binds []string) LoadOption {
	return func(args []string) []string {
		if binds != nil {
			for _, b := range binds {
				args = append(args, "--bind", b)
			}
		}
		return args
	}
}

// BindMode is a LoadOption that applies the passed binding mode to
// the service's load command line arguments.
func BindMode(mode string) LoadOption {
	return func(args []string) []string {
		if mode != "" {
			return append(args, "--binding-mode", mode)
		}
		return args
	}
}

type habCmd struct {
	offlineMode bool
	executor    command.Executor
}

// NewHabCmd returns an habCmd that uses the given
// command.Executor. If offlineMode is true then any InstallPackage()
// calls will use Habitat's OFFLINE_INSTALL feature.
func NewHabCmd(c command.Executor, offlineMode bool) HabCmd {
	return &habCmd{
		executor:    c,
		offlineMode: offlineMode,
	}
}

// Install installs the given Installable. If the install fails an
// error is returned.
//
// TODO(ssd) 2018-07-16: Can we rip channel out of here?  I don't
// think anything really uses channel anymore.
func (c *habCmd) InstallPackage(pkg habpkg.Installable, channel string) (string, error) {
	args := []string{"pkg", "install", pkg.InstallIdent()}
	opts := standardHabOptions()

	if c.offlineMode {
		args = append(args, "--offline")
		opts = append(opts, command.Envvar("HAB_FEAT_OFFLINE_INSTALL", "true"))
	}

	if channel != "" {
		args = append(args, "--channel", channel)
	}

	return c.executor.CombinedOutput("hab", append(opts, command.Args(args...))...)
}

// IsInstalled checks if a package is already installed
func (c *habCmd) IsInstalled(pkg habpkg.VersionedPackage) (bool, error) {
	err := c.executor.Run("hab",
		append(standardHabOptions(),
			command.Args("pkg", "path", habpkg.Ident(pkg)))...)
	if err != nil {
		return false, nil
	}

	return true, nil
}

// BinlinkPackage binlinks an executable from a Habitat package
func (c *habCmd) BinlinkPackage(pkg habpkg.VersionedPackage, exe string) (string, error) {
	args := command.Args("pkg", "binlink", "--force", habpkg.Ident(pkg), exe)
	return c.executor.CombinedOutput("hab", append(standardHabOptions(), args)...)
}

func (c *habCmd) LoadService(svc habpkg.VersionedPackage, opts ...LoadOption) (string, error) {
	args := []string{"svc", "load", "--force", habpkg.Ident(svc), "--strategy", "none"}
	for _, o := range opts {
		args = o(args)
	}
	return c.executor.CombinedOutput("hab", append(standardHabOptions(), command.Args(args...))...)
}

func (c *habCmd) UnloadService(svc habpkg.VersionedPackage) (string, error) {
	args := command.Args("svc", "unload", habpkg.ShortIdent(svc))
	return c.executor.CombinedOutput("hab", append(standardHabOptions(), args)...)
}

func (c *habCmd) StartService(svc habpkg.VersionedPackage) (string, error) {
	args := command.Args("svc", "start", habpkg.ShortIdent(svc))
	return c.executor.CombinedOutput("hab", append(standardHabOptions(), args)...)
}

func (c *habCmd) StopService(svc habpkg.VersionedPackage) (string, error) {
	args := command.Args("svc", "stop", habpkg.ShortIdent(svc))
	return c.executor.CombinedOutput("hab", append(standardHabOptions(), args)...)
}

func (c *habCmd) SupTerm() error {
	args := command.Args("sup", "term")
	_, err := c.executor.Start("hab", append(standardHabOptions(), args)...)
	return err
}

func standardHabOptions() []command.Opt {
	cmdOpts := make([]command.Opt, len(stdHabOptions))
	copy(cmdOpts, stdHabOptions)
	return cmdOpts
}
