package converge

import "github.com/chef/automate/components/automate-deployment/pkg/habpkg"

// SupervisorState represents the desired state of the Habitat
// supervisor, including its integration with systemd.
type SupervisorState interface {
	IsSkip() bool
	SupPkg() habpkg.HabPkg
	BinPkg() habpkg.HabPkg
	LauncherPkg() habpkg.HabPkg
	ProxyConfig() string
}

type enforcedSupervisorState struct {
	// hab-sup package
	supPkg habpkg.HabPkg
	// hab package (ugh, this is literally the hab habpkg.HabPkg)
	binPkg      habpkg.HabPkg
	launcherPkg habpkg.HabPkg
	proxyConfig string
}

func (s enforcedSupervisorState) IsSkip() bool {
	return false
}

func (s enforcedSupervisorState) SupPkg() habpkg.HabPkg {
	return s.supPkg
}

func (s enforcedSupervisorState) BinPkg() habpkg.HabPkg {
	return s.binPkg
}

func (s enforcedSupervisorState) LauncherPkg() habpkg.HabPkg {
	return s.launcherPkg
}

func (s enforcedSupervisorState) ProxyConfig() string {
	return s.proxyConfig
}

type skippedSupervisorState struct{}

func (s skippedSupervisorState) IsSkip() bool {
	return true
}

func (s skippedSupervisorState) SupPkg() habpkg.HabPkg {
	return habpkg.HabPkg{}
}

func (s skippedSupervisorState) BinPkg() habpkg.HabPkg {
	return habpkg.HabPkg{}
}

func (s skippedSupervisorState) LauncherPkg() habpkg.HabPkg {
	return habpkg.HabPkg{}
}

func (s skippedSupervisorState) ProxyConfig() string {
	return ""
}

func NewSupervisorState(supPkg habpkg.HabPkg, binPkg habpkg.HabPkg,
	launcherPkg habpkg.HabPkg, proxyConfig string) SupervisorState {
	return enforcedSupervisorState{
		supPkg:      supPkg,
		binPkg:      binPkg,
		launcherPkg: launcherPkg,
		proxyConfig: proxyConfig,
	}
}

func NewSkipSupervisorState() SupervisorState {
	return skippedSupervisorState{}
}
