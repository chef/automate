package config

import "github.com/chef/automate/lib/tls/certs"

// Configuration for the Report Manager Service
type ReportManager struct {
	Service         Service
	Log             Log
	certs.TLSConfig `mapstructure:"tls"`
}

type Service struct {
	Port    int
	Host    string
	Message string
}

type Log struct {
	Level  string
	Format string
}
