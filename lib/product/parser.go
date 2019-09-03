package product

import (
	"encoding/json"
	"os"
	"path"
	"path/filepath"

	"github.com/pkg/errors"
)

func parsePackageMetadata(packagePath string) (*PackageMetadata, error) {
	f, err := os.Open(packagePath)
	if err != nil {
		return nil, errors.Wrapf(err, "Couldn't load %s", packagePath)
	}
	defer f.Close() // nolint: errcheck
	decoder := json.NewDecoder(f)
	decoder.DisallowUnknownFields()

	metadata := PackageMetadata{}
	if err := decoder.Decode(&metadata); err != nil {
		return nil, errors.Wrapf(err, "Failed to read metadata for %s", packagePath)
	}

	if err := metadata.validate(); err != nil {
		return nil, errors.Wrapf(err, "Failed to validate metadata for %s", packagePath)
	}
	return &metadata, nil
}

type productsmeta struct {
	// Packages is a list of the root level packages/services that will
	// make up a release of automate.
	Packages []PackageName `json:"packages"`

	// DeletedPackages are old automate services or packages that are no
	// longer included in the repository.
	DeletedPackages []DeletedPackage `json:"deleted_packages"`

	// Collections is a list of collections that automate is broken down
	// into. Some of these are deployable by the user.
	Collections []*Collection `json:"collections"`
}

func (b *productsmeta) validate() error {
	if len(b.Collections) <= 0 {
		return errors.New("No collections found")
	}

	if len(b.Packages) <= 0 {
		return errors.New("No packages found")
	}

	allowedPackageSet := make(map[PackageName]bool)
	for _, p := range b.Packages {
		if err := p.validate(); err != nil {
			return err
		}
		allowedPackageSet[p] = true
	}

	collectionMap := make(map[string]*Collection)
	for _, c := range b.Collections {
		if err := c.validate(allowedPackageSet); err != nil {
			return err
		}
		if _, found := collectionMap[c.Name]; found {
			return errors.Errorf("collection name/alias %q was repeated", c.Name)

		}
		collectionMap[c.Name] = c
		for _, alias := range c.Aliases {
			if _, found := collectionMap[alias]; found {
				return errors.Errorf("collection name/alias %q was repeated", c.Name)

			}
			collectionMap[alias] = c
		}
	}

	for _, c := range b.Collections {
		for _, dep := range c.Dependencies {
			depCollection, found := collectionMap[dep]
			if !found {
				return errors.Errorf("%q is listed as a dependency for %q but was not found", dep, c.Name)
			}
			if c.Type == BaseType && depCollection.Type == ProductType {
				return errors.Errorf("base collection %q may not depend on product collection (%q)",
					c.Name, depCollection.Name)
			}
		}
	}

	return nil
}

func parseProductsMeta(filePath string) (*productsmeta, error) {
	f, err := os.Open(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open %s", filePath)
	}
	defer f.Close() // nolint: errcheck

	decoder := json.NewDecoder(f)
	decoder.DisallowUnknownFields()

	val := productsmeta{}
	if err := decoder.Decode(&val); err != nil {
		return nil, errors.Wrapf(err, "Failed to parse %s", filePath)
	}

	if err := val.validate(); err != nil {
		return nil, err
	}

	return &val, nil
}

// Parse parses the root level product.meta along with the package.meta files that
// may be provided by each component.
func Parse(repoRootPath string) (*Metadata, error) {
	componentsPath := path.Join(repoRootPath, "components")
	packageMetadataMap := make(map[PackageName]*PackageMetadata)
	packagesWithMetadata, err := filepath.Glob(path.Join(componentsPath, "*", "package.meta"))
	if err != nil {
		return nil, errors.Wrap(err, "Failed to glob packages")
	}

	if len(packagesWithMetadata) <= 0 {
		return nil, errors.New("Found no package metadata")
	}

	for _, packagePath := range packagesWithMetadata {
		metadata, err := parsePackageMetadata(packagePath)
		if err != nil {
			return nil, err
		}
		packageMetadataMap[metadata.Name] = metadata
	}

	metadata, err := parseProductsMeta(path.Join(repoRootPath, "products.meta"))
	if err != nil {
		return nil, err
	}

	packages := make([]*Package, len(metadata.Packages))
	for i := range metadata.Packages {
		packages[i] = &Package{
			Name:     metadata.Packages[i],
			Metadata: packageMetadataMap[metadata.Packages[i]],
		}
	}

	return &Metadata{
		Packages:        packages,
		DeletedPackages: metadata.DeletedPackages,
		Collections:     metadata.Collections,
	}, nil
}
