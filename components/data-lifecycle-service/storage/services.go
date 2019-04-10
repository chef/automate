package storage

// ServiceConfig describes how to connect to a service
type ServiceConfig struct {
	Address string
	Secure  bool
}

// ServiceConfigStore provides an interface to get information
// required for the data lifecycle service to manage the data
// lifecycle of a service. This data is not runtime configurable.
type ServiceConfigStore interface {
	// GetServiceConfig returns the service configuration for the requested service. If the configuration
	// is not found, an error is returned.
	GetServiceConfig(serviceName string) (*ServiceConfig, error)

	// ListServices returns a list of configured services by name
	ListServices() []string
}
