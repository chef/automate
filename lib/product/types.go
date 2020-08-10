package product

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
)

type CollectionType string

const (
	// BaseType is a collection that may not be directly
	// deployed by the user. It provides a way to group
	// services which may be depended upon by other products.
	BaseType CollectionType = "base"

	// ProductType is a collection that can be deployed by the user.
	ProductType CollectionType = "product"
)

func (c CollectionType) validate() error {
	switch c {
	case BaseType, ProductType:
		return nil
	default:
		return errors.Errorf("%s is not a valid collection type", c)
	}
}

// PackageName represents a package or service by origin/name
type PackageName struct {
	// Origin is the habitat origin this package belongs to
	Origin string
	// Name is the habitat name of the package
	Name string
}

func (p *PackageName) UnmarshalText(text []byte) error {
	parts := strings.Split(string(text), "/")
	if len(parts) == 2 {
		p.Origin = parts[0]
		p.Name = parts[1]
	} else {
		return errors.Errorf("%s is not a valid package. Must be origin/name", parts)
	}
	return nil
}

func (p *PackageName) MarshalText() ([]byte, error) {
	return []byte(fmt.Sprintf("%s/%s", p.Origin, p.Name)), nil
}

func (p *PackageName) validate() error {
	if p.Origin == "" || p.Name == "" {
		return errors.New("Invalid package")
	}
	return nil
}

func (p PackageName) String() string {
	return fmt.Sprintf("%s/%s", p.Origin, p.Name)
}

// DeletedPackage represents a deleted package or service and the last version
// and release.
type DeletedPackage struct {
	// Origin is the habitat origin this package belongs to
	Origin string
	// Name is the habitat name of the package
	Name string
	// Version is the habitat package version
	Version string
	// Release is the habitat build release
	Release string
}

func (p *DeletedPackage) UnmarshalText(text []byte) error {
	parts := strings.Split(string(text), "/")
	if len(parts) == 4 {
		p.Origin = parts[0]
		p.Name = parts[1]
		p.Version = parts[2]
		p.Release = parts[3]
	} else {
		return errors.Errorf("%s is not a valid deleted package. Must be origin/name/version/release", parts)
	}
	return nil
}

func (p *DeletedPackage) MarshalText() ([]byte, error) {
	return []byte(fmt.Sprintf("%s/%s/%s/%s", p.Origin, p.Name, p.Version, p.Release)), nil
}

func (p *DeletedPackage) validate() error {
	if p.Origin == "" || p.Name == "" || p.Version == "" || p.Release == "" {
		return errors.New("Invalid deleted package")
	}
	return nil
}

func (p DeletedPackage) String() string {
	return fmt.Sprintf("%s/%s/%s/%s", p.Origin, p.Name, p.Version, p.Release)
}

// Collection is a group of services and packages that provide some
// desired functionality. For example, postgres requires 3 services
// to fully work in Automate: postgresql, a sidecar, and a tcp
// gateway.
// Collections can be of type base, meaning they are deployed as
// part of other collections. The other type is product, which allows
// them to be deployed by users (automate, chef-server, workflow, etc).
type Collection struct {
	Name string `json:"name"`

	// Aliases is a list of alternative names that can be used for the
	// collection. As an example, we might want to allow both chef-server
	// and chef-infra-server
	Aliases []string `json:"aliases"`

	// Type is one of base or product. Products can be deployed, where as
	// base collections can be included in other collections as deps
	Type CollectionType `json:"type"`

	// Services is the list of packages that will be deployed as services
	// for the collection.
	Services []PackageName `json:"services"`

	// Packages is a list of packages that are needed for this collection.
	// They will likely be providing binlinks for the user
	Packages []PackageName `json:"packages"`

	// Dependencies is a list of collections this collection depends on.
	Dependencies []string `json:"dependencies"`

	// Hidden allows adding collections that we don't want to present to the
	// user
	Hidden bool `json:"hidden"`
}

func (c *Collection) validate(allowedPackageSet map[PackageName]bool) error {
	if err := c.Type.validate(); err != nil {
		return err
	}
	for _, p := range c.Services {
		if err := p.validate(); err != nil {
			return err
		}
		if !allowedPackageSet[p] {
			return errors.Errorf("Expected %s in packages list", p)
		}
	}

	for _, p := range c.Packages {
		if err := p.validate(); err != nil {
			return err
		}
		if !allowedPackageSet[p] {
			return errors.Errorf("Expected %s in packages list", p)
		}
	}
	return nil
}

type BootstrapType string

const (
	BootstrapTypeFile   BootstrapType = "file"
	BootstrapTypeSecret BootstrapType = "secret"
)

type BootstrapSpec struct {
	Type       BootstrapType `json:"type"`
	Path       string        `json:"path,omitempty"`
	Optional   bool          `json:"optional"`
	SecretSpec string        `json:"secret_spec,omitempty"`
}

func (b *BootstrapSpec) validate() error {
	switch b.Type {
	case BootstrapTypeFile:
		if b.Path == "" {
			return errors.Errorf("Must provide a path file type %s", BootstrapTypeFile)
		}
		if filepath.IsAbs(b.Path) {
			return errors.Errorf("path must be a relative path inside the services /hab/svc/svc-name directory")
		}
		// TODO: validate that the path stays in the service directory
	case BootstrapTypeSecret:
		if b.SecretSpec == "" {
			return errors.Errorf("Must provide a secret name for type %s", b.Type)
		}
	default:
		return errors.Errorf("%q is not a valid bootstrap type", b.Type)
	}

	return nil
}

// PackageMetadata is a set of metadata that components may optionally provide.
type PackageMetadata struct {
	Name PackageName `json:"name"`

	// DataService is set to true if this service is a data service.
	// For example, postgres, elasticsearch, s3
	DataService bool `json:"data_service"`

	// A list of binaries to be binlinked when the package is deployed
	Binlinks []string `json:"binlinks"`

	// UsesPlatformScaffolding is set to true if the service uses the platform
	// scaffolding for config
	UsesPlatformScaffolding bool `json:"uses_platform_scaffolding"`

	Bootstrap []BootstrapSpec `json:"bootstrap"`
}

func (p *PackageMetadata) validate() error {
	return nil
}

// Package represents a package/service along with its metadata
type Package struct {
	Name PackageName `json:"name"`
	// Metadata is optional metadata a package may provide. This
	// field is nullable

	Metadata *PackageMetadata `json:"metadata"`
}

// Metadata is the top level metadata the describes the automate
// collections and its packages.
type Metadata struct {
	Packages        []*Package       `json:"packages"`
	DeletedPackages []DeletedPackage `json:"deleted_packages"`
	Collections     []*Collection    `json:"collections"`
}
