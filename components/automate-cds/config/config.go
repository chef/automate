package config

import "github.com/chef/automate/lib/tls/certs"

// AutomateCdsConfig automate CDS config
type AutomateCdsConfig struct {
	GRPC              string `mapstructure:"grpc"`
	LogFormat         string `mapstructure:"log-format"`
	LogLevel          string `mapstructure:"log-level"`
	certs.TLSConfig   `mapstructure:"tls"`
	ComplianceService `mapstructure:"compliance-service"`
}

// ComplianceService connection information
type ComplianceService struct {
	Address string `mapstructure:"address"`
}
