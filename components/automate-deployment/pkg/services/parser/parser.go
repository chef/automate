package parser

import (
	"encoding/json"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/bind"
)

type ServiceCollection struct {
	Name         string     `json:"collection"`
	DataServices []string   `json:"data_services"`
	AllServices  []string   `json:"all_services"`
	Pins         []string   `json:"pinned_packages"`
	ExtraPkgs    []string   `json:"extra_packages"`
	Binlinks     BinlinkMap `json:"binlinks"`
}

type BinlinkMap map[string][]string

func ServiceCollectionsFromJSON(serviceData []byte) ([]ServiceCollection, error) {
	collections := []ServiceCollection{}
	err := json.Unmarshal(serviceData, &collections)

	if err != nil {
		return []ServiceCollection{}, errors.Wrap(err, "invalid JSON in services file")
	}
	return collections, nil
}

func ServiceCollectionFromJSON(serviceData []byte, collectionName string) (ServiceCollection, error) {
	collections, err := ServiceCollectionsFromJSON(serviceData)
	if err != nil {
		return ServiceCollection{}, err
	}

	for _, c := range collections {
		if c.Name == collectionName {
			return c, nil
		}
	}
	return ServiceCollection{}, errors.Errorf("service collection '%s' not found in services JSON data", collectionName)
}

func ServiceNamesFromJSON(serviceData []byte, collectionName string) ([]string, error) {
	c, err := ServiceCollectionFromJSON(serviceData, collectionName)
	if err != nil {
		return []string{}, err
	}
	return c.AllServices, nil
}

func ParseServiceBinds(data []byte) (bind.Binds, error) {
	b, err := bind.ParseData(data)
	if err != nil {
		return bind.Binds{}, errors.Wrap(err, "service binds data is not parsable")
	}
	return b, nil
}
