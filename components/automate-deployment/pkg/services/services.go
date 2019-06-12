// Copyright © 2017 Chef Software

package services

import (
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/assets"
	"github.com/chef/automate/components/automate-deployment/pkg/bind"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services/parser"
)

var servicesByCollection map[string][]habpkg.HabPkg

var supplementaryPackagesByCollection map[string][]habpkg.HabPkg

var serviceCollections map[string]parser.ServiceCollection

// AllBinds contains all the bindings that the deployment service is aware of
var AllBinds bind.Binds

// automateFull is the full ServiceCollection object with all the data on
// packages and services we install/manage
var automateFull parser.ServiceCollection

// AllPackages returns a list of all packages (service, binary, or whatever) that
// were specified in the manifest
func AllPackages() []habpkg.HabPkg {
	pkgs := []habpkg.HabPkg{}
	mark := make(map[string]bool)
	for _, serviceCollection := range serviceCollections {
		for _, section := range [][]string{
			serviceCollection.AllServices,
			serviceCollection.ExtraPkgs,
			serviceCollection.Pins,
			serviceCollection.DataServices} {
			for _, pkgStr := range section {
				if !mark[pkgStr] {
					mark[pkgStr] = true
					pkg, err := habpkg.FromString(pkgStr)
					if err != nil {
						panic(errors.Wrapf(err, "failed to parse hab pkg name '%s' in services list", pkgStr))
					}
					pkgs = append(pkgs, pkg)
				}
			}
		}
	}
	return pkgs
}

func AllServices() ([]habpkg.HabPkg, error) {
	names := []string{}
	for name := range servicesByCollection {
		names = append(names, name)
	}
	return ServicesInCollections(names)
}

func ListCollections() []string {
	collections := make([]string, len(serviceCollections))
	i := 0
	for k := range serviceCollections {
		collections[i] = k
		i++
	}
	return collections
}

func ServicesInCollections(collections []string) ([]habpkg.HabPkg, error) {
	combined := []habpkg.HabPkg{}
	for _, collectionName := range collections {
		serviceIDs, err := ServicesInCollection(collectionName)
		if err != nil {
			return combined, err
		}
		combined = append(combined, serviceIDs...)
	}
	return combined, nil
}

func ServicesInCollection(collection string) ([]habpkg.HabPkg, error) {
	list, ok := servicesByCollection[collection]
	if !ok {
		return nil, errors.Errorf("services collection '%s' not found", collection)
	}
	return list, nil
}

func SupplementaryPackagesInCollection(collection string) ([]habpkg.HabPkg, error) {
	list, ok := supplementaryPackagesByCollection[collection]
	if !ok {
		return []habpkg.HabPkg{}, errors.Errorf("services collection '%s' not found", collection)
	}
	return list, nil
}

// IsDataService is used during A1 -> A2 upgrades to start postgres/elasticsearch
// separately from the domain services.
func IsDataService(candidate string) bool {
	serviceIDs, err := habpkg.FromList(automateFull.DataServices)

	if err != nil {
		panic(err.Error())
	}

	for _, iter := range serviceIDs {
		if iter.Name() == candidate {
			return true
		}
	}
	return false
}

var binlinkMap map[string][]string

func BinlinksForService(serviceName string) []string {
	return binlinkMap[serviceName]
}

func loadServiceCollections() map[string]parser.ServiceCollection {
	bytes := assets.MustAsset("data/services.json")
	collections, err := parser.ServiceCollectionsFromJSON(bytes)
	if err != nil {
		panic(err.Error())
	}

	collectionMap := make(map[string]parser.ServiceCollection)

	for _, collection := range collections {
		// IsDataService panics on invalid data; force the panic to happen in init if
		// the data is bad.
		for _, s := range collection.AllServices {
			pkg, err := habpkg.FromString(s)
			if err != nil {
				panic(errors.Wrapf(err, "failed to parse hab pkg name '%s' in services list", s))
			}
			_ = IsDataService(pkg.Name())
		}
		collectionMap[collection.Name] = collection
	}

	return collectionMap
}

func loadAutomateServiceCollection() parser.ServiceCollection {
	return serviceCollections["automate-full"]
}

func loadServiceIDs() map[string][]habpkg.HabPkg {
	serviceMap := make(map[string][]habpkg.HabPkg)
	for name, collection := range serviceCollections {
		serviceIDs, err := habpkg.FromList(collection.AllServices)
		if err != nil {
			panic(err.Error())
		}
		serviceMap[name] = removeDeploymentService(name, serviceIDs)
	}
	return serviceMap
}

func removeDeploymentService(collectionName string, serviceIDs []habpkg.HabPkg) []habpkg.HabPkg {
	// remove deployment-services from Services slice
	for i := range serviceIDs {
		if serviceIDs[i].Name() == "deployment-service" {
			serviceIDs = append(serviceIDs[:i], serviceIDs[i+1:]...)
			return serviceIDs
		}
	}
	return serviceIDs
}

func loadSupplementaryPackages() map[string][]habpkg.HabPkg {
	pkgMap := make(map[string][]habpkg.HabPkg)
	for name, collection := range serviceCollections {
		rawPackageList := collection.ExtraPkgs
		pkgIDs, err := habpkg.FromList(rawPackageList)
		if err != nil {
			panic(err.Error())
		}
		pkgMap[name] = pkgIDs
	}
	return pkgMap
}

// loadServiceBinds parses the bindings for the services
func loadServiceBinds() bind.Binds {
	data := assets.MustAsset("data/binds.txt")
	b, err := parser.ParseServiceBinds(data)
	if err != nil {
		panic(errors.Wrap(err, "binds.txt is not parsable"))
	}
	return b
}

func loadBinlinkMap() map[string][]string {
	m := make(map[string][]string)
	for _, collection := range serviceCollections {
		for pkgName, binlinks := range collection.Binlinks {
			m[pkgName] = binlinks
		}
	}
	return m
}

func init() {
	serviceCollections = loadServiceCollections()
	automateFull = loadAutomateServiceCollection()
	AllBinds = loadServiceBinds()
	servicesByCollection = loadServiceIDs()
	supplementaryPackagesByCollection = loadSupplementaryPackages()
	binlinkMap = loadBinlinkMap()
}
