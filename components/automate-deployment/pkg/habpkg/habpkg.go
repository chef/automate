package habpkg

import (
	"fmt"
	"strings"

	"github.com/pkg/errors"
)

// HabPkg represents a Habitat package that can be installed from
// either the depot or a file on disk using the hab command line tool.
type HabPkg struct {
	origin  string
	name    string
	version string
	release string
}

// VersionedPackage is an interface to help bridge the gap between
// HabPkg and Hart.
type VersionedPackage interface {
	Origin() string
	Name() string
	VersionedArtifact
}

// VersionedArtifact is an interface for artifacts that use
// habitat-style VERSION/RELEASE version string
type VersionedArtifact interface {
	Version() string
	Release() string
}

// An Installable represents something that can be installed with `hab
// pkg install`. Currently this includes HabPkgs and Harts.
type Installable interface {
	VersionedPackage
	InstallIdent() string
}

// GreaterOrEqual returns true if VersionedArtifact fully qualified
// version is greater or equal to b.  It assumes Version() and
// Release() return lexically sortable strings.
func GreaterOrEqual(a VersionedArtifact, b VersionedArtifact) bool {
	if a.Version() > b.Version() {
		return true
	}

	if b.Version() > a.Version() {
		return false
	}

	// a.Version() and b.Version() are equal, check release
	if a.Release() >= b.Release() {
		return true
	}

	return false
}

// VersionString (ugh, bad name) returns a string representing the
// version information available in the VersionedArtifact
func VersionString(v VersionedArtifact) string {
	if v.Release() != "" {
		return fmt.Sprintf("%s/%s", v.Version(), v.Release())
	}

	return v.Version()
}

// New constructs a HabPkg
func New(origin, name string) HabPkg {
	return HabPkg{
		origin: origin,
		name:   name,
	}
}

// NewFQ constructs a fully qualified HabPkg
func NewFQ(origin, name, version, release string) HabPkg {
	return HabPkg{
		origin:  origin,
		name:    name,
		version: version,
		release: release,
	}
}

// NewWithVersion constructs a HabPkg with a version (but no release)
func NewWithVersion(origin, name, version string) HabPkg {
	return HabPkg{
		origin:  origin,
		name:    name,
		version: version,
	}
}

// FromString takes a string in the form of ORIGIN/NAME and
// resturns a HabPkg struct
func FromString(ident string) (HabPkg, error) {
	parts := strings.Split(ident, "/")
	if len(parts) < 2 || len(parts) > 4 {
		return HabPkg{}, errors.Errorf("invalid package identifier '%s'", ident)
	}

	p := HabPkg{}

	for i, part := range parts {
		switch i {
		case 0:
			p.origin = part
		case 1:
			p.name = part
		case 2:
			p.version = part
		case 3:
			p.release = part
		}
	}

	return p, nil
}

// FromStrings takes a newline separated list of package identifiers
// and returns a list of HabPkgs.
func FromStrings(data string) ([]HabPkg, error) {
	paths := strings.Split(data, "\n")
	return FromList(paths)
}

func FromList(paths []string) ([]HabPkg, error) {
	packages := make([]HabPkg, 0, len(paths))
	for _, path := range paths {
		path = strings.Trim(path, "\t\n ")
		if len(path) == 0 {
			continue
		}

		sp, err := FromString(path)
		if err != nil {
			return packages, err
		}
		packages = append(packages, sp)
	}

	return packages, nil
}

func ShortIdent(p VersionedPackage) string {
	return fmt.Sprintf("%s/%s", p.Origin(), p.Name())
}

// Ident returns the path form of a package name as ORIGIN/NAME or
// ORIGIN/NAME/VERSION or ORIGIN/NAME/VERSION/RELEASE
func Ident(p VersionedPackage) string {
	base := fmt.Sprintf("%s/%s", p.Origin(), p.Name())

	if p.Version() != "" {
		base = fmt.Sprintf("%s/%s", base, p.Version())

		if p.Release() != "" {
			base = fmt.Sprintf("%s/%s", base, p.Release())
		}
	}

	return base
}

// IsFullyQualified returns true if both version and release are specified
func IsFullyQualified(p VersionedPackage) bool {
	return p.Version() != "" && p.Release() != ""
}

// Origin returns the origin of the package
func (p *HabPkg) Origin() string {
	return p.origin
}

// Name returns the name of the package
func (p *HabPkg) Name() string {
	return p.name
}

// Version returns the version of the package
func (p *HabPkg) Version() string {
	return p.version
}

// Release returns the release of the package
func (p *HabPkg) Release() string {
	return p.release
}

func (p *HabPkg) InstallIdent() string {
	return Ident(p)
}

// MarshalText turns a HabPkg into text
func (p *HabPkg) MarshalText() ([]byte, error) {
	return []byte(Ident(p)), nil
}

// UnmarshalText can take text and parse a HabPkg
func (p *HabPkg) UnmarshalText(text []byte) error {
	newPkg, err := FromString(string(text))
	if err != nil {
		return err
	}
	*p = newPkg
	return nil
}

func (p *HabPkg) String() string {
	return p.InstallIdent()
}
