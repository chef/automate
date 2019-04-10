package converge

import "github.com/chef/automate/components/automate-deployment/pkg/habpkg"

// ServiceConvergeState is the desired state for an individual service
type ServiceConvergeState interface {
	isServiceConvergeState()
}

type skip struct {
}

func (*skip) isServiceConvergeState() {}

type installed struct {
	pkg habpkg.Installable
}

func (*installed) isServiceConvergeState() {}

type running struct {
	installed *installed
	userToml  string
	bindMode  string
	binds     []string
}

func (*running) isServiceConvergeState() {}

type RunningOption func(r *running)

func UserTOML(t string) RunningOption {
	return func(r *running) {
		r.userToml = t
	}
}

func BindMode(m string) RunningOption {
	return func(r *running) {
		r.bindMode = m
	}
}

func Binds(b []string) RunningOption {
	return func(r *running) {
		r.binds = b
	}
}

// Skip is a ConvergeState that means we should do nothing to put the service in any particular state
func Skip() ServiceConvergeState {
	return &skip{}
}

// Installed is a ConvergeState that means we should install hab package. The VersionConstraint specified
// describes options for installation, such as to do it from a hart
func Installed(pkg habpkg.Installable) ServiceConvergeState {
	return &installed{
		pkg: pkg,
	}
}

// Running is a ConvergeState that means the service should be
// installed, configured and supervised by hab
func Running(pkg habpkg.Installable, opts ...RunningOption) ServiceConvergeState {
	r := &running{
		installed: &installed{
			pkg: pkg,
		},
	}

	for _, o := range opts {
		o(r)
	}
	return r
}

func ServicePackage(convergeState ServiceConvergeState) habpkg.Installable {
	if s, ok := convergeState.(*running); ok {
		return s.installed.pkg
	} else if s, ok := convergeState.(*installed); ok {
		return s.pkg
	}
	return nil
}
