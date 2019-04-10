package manifest

import (
	"errors"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// ErrNotFoundInManifest indicates a package was not found in the manifest for
// the given name
var ErrNotFoundInManifest = errors.New("Could not find package in manifest")

// A ReleaseManifest specifies the expected contents of the build.
type ReleaseManifest interface {
	HartForServiceName(name string) (bool, habpkg.Hart)
	PackageForServiceName(name string) (bool, habpkg.HabPkg)
	ListPackages() []habpkg.HabPkg
	Version() string
	SHA() string
}

// An A2 manifest specifies the expected contents of a build
type A2 struct {
	Build         string          `json:"build"`
	BuildSHA      string          `json:"build_sha"`
	Packages      []habpkg.HabPkg `json:"packages"`
	HartOverrides []habpkg.Hart   `json:"hart_overrides"`
}

// PackageForServiceName returns a Package for the named service
func (m *A2) PackageForServiceName(name string) (found bool, habPkg habpkg.HabPkg) {
	for _, pkg := range m.Packages {
		if pkg.Name() == name {
			return true, pkg
		}
	}
	return false, habpkg.HabPkg{}
}

// HartForServiceName returns a hart for the named service to use as an override
func (m *A2) HartForServiceName(name string) (bool, habpkg.Hart) {
	for _, hart := range m.HartOverrides {
		if hart.Name() == name {
			return true, hart
		}
	}
	return false, habpkg.Hart{}
}

func (m *A2) ListPackages() []habpkg.HabPkg {
	pkgs := make([]habpkg.HabPkg, 0, len(m.Packages)+len(m.HartOverrides))
	pkgs = append(pkgs, m.Packages...)
	for _, h := range m.HartOverrides {
		pkgs = append(pkgs, habpkg.NewFQ(h.Origin(), h.Name(), h.Version(), h.Release()))
	}
	return pkgs
}

// Version returns the manifest version
func (m *A2) Version() string {
	return m.Build
}

// SHA returns the manifest SHA
func (m *A2) SHA() string {
	return m.BuildSHA
}

// InstallableFromManifest returns a hart if one was provided for the given service,
// falling back to the package provided in the manifest
// If the service is not found, both will be nil
func InstallableFromManifest(m ReleaseManifest, name string) habpkg.Installable {
	found, hart := m.HartForServiceName(name)
	if found {
		return &hart
	}

	found, habPkg := m.PackageForServiceName(name)
	if found {
		return &habPkg
	}

	return nil
}

// VersionedPackageFromManifest returns a VersionedPackage from the
// manifest for the given service. If a hartifact exists, the
// hartifact is preferred. Returns nil if not found.
func VersionedPackageFromManifest(m ReleaseManifest, name string) habpkg.VersionedPackage {
	return InstallableFromManifest(m, name)
}
