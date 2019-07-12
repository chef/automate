package config

import (
	"github.com/chef/automate/lib/tls/certs"
)

// Service is a base config options struct for all services
type Service struct {
	Endpoint   string
	Name       string
	HostBind   string
	Port       int
	LogLevel   string
	ServerBind string
	certs.TLSConfig
	ConfigFilePath string
}

// Compliance service specific config options
type Compliance struct {
	Postgres
	ElasticSearch
	InspecAgent
	Service
	Profiles
	Delivery
	Manager
	Secrets
	Authz
	LatestRebuilder
	ElasticSearchSidecar
	DataRetention
	EventConfig
	Notifications
}

type Notifications struct {
	Target string
}

type LatestRebuilder struct {
	Clean                        bool
	StartFromDateInUtc           string
	DaysBeyondMostRecentScanDate int
	QuickMode                    bool
}

// Secrets service specific config options
type Secrets struct {
	Endpoint string
	HostBind string
	Port     int
}

// Authz service specific config options
type Authz struct {
	Endpoint string
	HostBind string
	Port     int
}

// Manager service specific config options
type Manager struct {
	Host                       string
	Port                       int
	Endpoint                   string
	ManualPollIntervalMinutes  int
	AwsEc2PollIntervalMinutes  int
	AzureVMPollIntervalMinutes int
}

// InspecAgent service specific config options
type InspecAgent struct {
	JobBufferSize       int
	JobWorkers          int
	SocketPath          string
	BackendCache        string
	AuthnTarget         string
	AutomateFQDN        string
	TmpDir              string
	RemoteInspecVersion string
}

// Postgres specific options
type Postgres struct {
	ConnectionString string
	Database         string
	MigrationsPath   string
}

// ElasticSearch specific options
type ElasticSearch struct {
	Url         string
	Token       string
	AuthVersion string
	AllowOrigin string
}

// Profiles specific options
type Profiles struct {
	MarketPath   string
	ProfilesPath string
}

// Delivery specific options to authenticate to Automate
type Delivery struct {
	Enterprise string
	User       string
	Token      string
	DCToken    string
}

// ElasticSearchSidecar specific options
type ElasticSearchSidecar struct {
	ESSidecarAddress string
}

// DataRetention describes data retention policies
type DataRetention struct {
	ComplianceReportDays int32
}

type EventConfig struct {
	Endpoint string
}
