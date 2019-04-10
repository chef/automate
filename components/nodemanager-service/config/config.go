package config

import "github.com/chef/automate/lib/tls/certs"

var SERVICE_NAME = "nodemanager"

// Service is a base config options struct for all services
type Service struct {
	Name       string
	HostBind   string
	Port       int
	LogLevel   string
	ServerBind string
	certs.TLSConfig
}

// Nodemanager service specific config options
type Nodemanager struct {
	Postgres
	Service
	Manager
	Secrets
	EventConfig
}

// Secrets service specific config options
type Secrets struct {
	Endpoint string
	HostBind string
	Port     int
}

// Manager service specific config options
type Manager struct {
	Endpoint                   string
	ManualPollIntervalMinutes  int
	AwsEc2PollIntervalMinutes  int
	AzureVMPollIntervalMinutes int
}

// Postgres specific options
type Postgres struct {
	ConnectionString string
	MigrationsPath   string
	Database         string
}

type EventConfig struct {
	Endpoint string
}
