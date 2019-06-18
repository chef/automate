package product

import (
	"fmt"
	"strings"

	"github.com/pkg/errors"
)

type CollectionType string

const (
	// BaseType is a service collection that may not be directly
	// deployed by the user. It provides a way to group
	// services which may be depended upon by other products.
	BaseType CollectionType = "base"
	// ProductType is a service collection that can be deployed
	// by a user.
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

type PackageName struct {
	Origin string
	Name   string
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

type Collection struct {
	Name         string         `json:"name"`
	Aliases      []string       `json:"aliases"`
	Type         CollectionType `json:"type"`
	Services     []PackageName  `json:"services"`
	Packages     []PackageName  `json:"packages"`
	Dependencies []string       `json:"dependencies"`
	Hidden       bool           `json:"hidden"`
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

type PackageMetadata struct {
	Name PackageName `json:"name"`
	// DataService is set to true if this service is a a data service.
	// For example, postgres, elasticsearch, s3
	DataService bool `json:"data_service"`

	Binlinks []string `json:"binlinks"`
}

func (p *PackageMetadata) validate() error {
	return nil
}

type Package struct {
	Name     PackageName      `json:"name"`
	Metadata *PackageMetadata `json:"metadata"`
}

type Metadata struct {
	Packages    []*Package    `json:"packages"`
	Collections []*Collection `json:"collections"`
}
