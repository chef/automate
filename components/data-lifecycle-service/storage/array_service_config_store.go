package storage

import "fmt"

// ArrayServiceConfigStore provides service config information from an in memory
// array. These services must all implement the DataLifeCycleManageable interface
type ArrayServiceConfigStore struct {
	services map[string]ServiceConfig
}

// NewArrayServiceConfigStore returns a ConfigStore that returns service configuration information
// from the provided services map
func NewArrayServiceConfigStore(services map[string]ServiceConfig) *ArrayServiceConfigStore {
	return &ArrayServiceConfigStore{
		services: services,
	}
}

// GetServiceConfig returns the service configuration for the requested service. If the configuration
// is not found, an error is returned.
func (store *ArrayServiceConfigStore) GetServiceConfig(serviceName string) (*ServiceConfig, error) {
	serviceConfig, present := store.services[serviceName]
	if !present {
		return nil, fmt.Errorf("Config for service '%s' not found", serviceName)
	}

	serviceConfigCopy := serviceConfig
	return &serviceConfigCopy, nil
}

// ListServices returns a list of configured services by name
func (store *ArrayServiceConfigStore) ListServices() []string {
	keys := []string{}
	for k := range store.services {
		keys = append(keys, k)
	}
	return keys
}
