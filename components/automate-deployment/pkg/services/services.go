// Copyright © 2017 Chef Software

package services

import (
	"encoding/json"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/assets"
	"github.com/chef/automate/components/automate-deployment/pkg/bind"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services/internal/generated"
	"github.com/chef/automate/lib/product"
)

//go:generate go run ../../tools/services-pkg-gen/main.go ../../../../ internal/generated/gen.go

// AllBinds contains all the bindings that the deployment service is aware of
var AllBinds bind.Binds

var serviceList []habpkg.HabPkg
var productList []string
var packageMetadataMap map[string]*product.Package
var collectionMap map[string]*product.Collection

// well-known collections
const (
	AutomateCollectionName   = "automate"
	BuilderCollectionName    = "builder"
	ChefServerCollectionName = "chef-server"
	WorkflowCollectionName   = "workflow"
	MonitoringCollectionName = "monitoring"
)

func AllServices() ([]habpkg.HabPkg, error) {
	return serviceList, nil
}

func MetadataForPackage(pkgName string) *product.PackageMetadata {
	if packageMetadataMap[pkgName] != nil {
		return packageMetadataMap[pkgName].Metadata
	}
	return nil
}

func ContainsCollection(needle string, haystack []string) bool {
	desiredCollection := collectionMap[needle]
	if desiredCollection == nil {
		return false
	}

	visited := map[string]bool{}

	for _, collectionName := range haystack {
		collection := collectionMap[collectionName]
		if collection != nil {
			if collection == desiredCollection {
				return true
			}
			deps := getRequiredCollections(collectionName, visited)
			for _, d := range deps {
				if d.Type != product.ProductType && desiredCollection == d {
					return true
				}
			}
		}
	}
	return false
}

func NormalizeNames(products []string) {
	for i, p := range products {
		collection := collectionMap[p]
		if collection == nil {
			continue
		}
		if collection.Name != p {
			products[i] = collection.Name
		}
	}
}

func ListProducts() []string {
	return productList
}

func ValidateProductDeployment(products []string) error {
	visited := map[string]bool{}
	requiredCollectionSet := make(map[string]*product.Collection)
	desiredCollectionSet := make(map[string]*product.Collection)
	for _, p := range products {
		collection := collectionMap[p]
		if collection == nil || collection.Type != product.ProductType {
			return errors.Errorf("Unknown product %q. Must be one of (%s)", p, strings.Join(ListProducts(), ", "))
		}
		desiredCollectionSet[collection.Name] = collection
		deps := getRequiredCollections(p, visited)
		for _, d := range deps {
			requiredCollectionSet[d.Name] = d
		}
	}

	for requiredCollectionName, requiredCollection := range requiredCollectionSet {
		if desiredCollectionSet[requiredCollectionName] == nil &&
			requiredCollection.Type == product.ProductType {
			return errors.Errorf("You must deploy %q to deploy %s", requiredCollectionName, strings.Join(products, ", "))
		}
	}

	return nil
}

func ServicesInCollection(collection string) ([]habpkg.HabPkg, error) {
	return ServicesInCollections([]string{collection})
}

func ServicesInCollections(collections []string) ([]habpkg.HabPkg, error) {
	requiredServices := map[string]bool{}
	visited := map[string]bool{}
	for _, collection := range collections {
		if collectionMap[collection] == nil {
			return nil, errors.Errorf("unknown collection %q", collection)
		}
		deps := getRequiredServices(collection, visited)
		for _, d := range deps {
			requiredServices[d.Name] = true
		}
	}
	return sortServices(requiredServices), nil
}

func SupplementaryPackagesInCollection(collection string) ([]habpkg.HabPkg, error) {
	if collectionMap[collection] == nil {
		return nil, errors.Errorf("unknown collection %q", collection)
	}

	visited := map[string]bool{}
	deps := getRequiredCollections(collection, visited)
	pkgs := []habpkg.HabPkg{}
	for _, c := range deps {
		for _, p := range c.Packages {
			pkgs = append(pkgs, habpkg.New(p.Origin, p.Name))
		}
	}

	return pkgs, nil
}

// IsDataService is used during A1 -> A2 upgrades to start postgres/elasticsearch
// separately from the domain services.
func IsDataService(pkgName string) bool {
	if packageMetadataMap[pkgName].Metadata != nil {
		return packageMetadataMap[pkgName].Metadata.DataService
	}
	return false
}

func BinlinksForPackage(pkgName string) []string {
	if packageMetadataMap[pkgName] == nil {
		return nil
	}
	if packageMetadataMap[pkgName].Metadata != nil {
		return packageMetadataMap[pkgName].Metadata.Binlinks
	}
	return nil
}

// loadServiceBinds parses the bindings for the services
func loadServiceBinds() bind.Binds {
	b, err := bind.ParseData([]byte(assets.BindData))
	if err != nil {
		panic(errors.Wrap(err, "assets.BindData is not parsable"))
	}
	return b
}

func getRequiredServices(collectionName string, visitedCollections map[string]bool) []product.PackageName {
	if visitedCollections[collectionName] {
		return []product.PackageName{}
	}

	visitedCollections[collectionName] = true
	requiredServices := []product.PackageName{}
	collection := collectionMap[collectionName]
	for _, c := range collection.Dependencies {
		services := getRequiredServices(c, visitedCollections)
		requiredServices = append(requiredServices, services...)
	}

	requiredServices = append(requiredServices, collection.Services...)
	return requiredServices

}

func sortServices(requiredServices map[string]bool) []habpkg.HabPkg {
	sortedRequiredServices := make([]habpkg.HabPkg, 0, len(requiredServices))
	for _, s := range serviceList {
		if requiredServices[s.Name()] {
			sortedRequiredServices = append(sortedRequiredServices, s)
		}
	}
	return sortedRequiredServices
}

func getRequiredCollections(collectionName string, visitedCollections map[string]bool) []*product.Collection {
	collection := collectionMap[collectionName]

	if visitedCollections[collection.Name] {
		return []*product.Collection{}
	}

	visitedCollections[collection.Name] = true
	requiredCollections := []*product.Collection{}
	for _, c := range collection.Dependencies {
		collections := getRequiredCollections(c, visitedCollections)
		requiredCollections = append(requiredCollections, collections...)
	}

	requiredCollections = append(requiredCollections, collection)
	return requiredCollections

}

func init() {
	AllBinds = loadServiceBinds()

	packageMetadataMap = make(map[string]*product.Package)
	collectionMap = make(map[string]*product.Collection)

	metadata := product.Metadata{}
	err := json.Unmarshal([]byte(generated.ProductMetadataJSON), &metadata)
	if err != nil {
		panic(err)
	}

	serviceSet := make(map[product.PackageName]bool)
	for _, c := range metadata.Collections {
		if c.Type == product.ProductType && !c.Hidden {
			productList = append(productList, c.Name)
		}

		collectionMap[c.Name] = c
		for _, alias := range c.Aliases {
			collectionMap[alias] = c
		}

		for _, s := range c.Services {
			serviceSet[s] = true
		}
	}

	for _, p := range metadata.Packages {
		packageMetadataMap[p.Name.Name] = p
		if serviceSet[p.Name] {
			serviceList = append(serviceList, habpkg.New(p.Name.Origin, p.Name.Name))
		}
	}
}
