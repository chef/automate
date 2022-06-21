package config

import "github.com/chef/automate/lib/tls/certs"

// Configuration for the Report Manager Service
type AutomateSupermarket struct {
	Service          Service `mapstructure:"service"`
	Log              Log
	CerealConfig     CerealConfig     `mapstructure:"cereal"`
	ComplianceConfig ComplianceConfig `mapstructure:"compliance"`
	Storage          Storage          `mapstructure:"storage"`
	certs.TLSConfig  `mapstructure:"tls"`
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

type CerealConfig struct {
	Target string `mapstructure:"target"`
}

type ComplianceConfig struct {
	Target string `mapstructure:"target"`
}

type Storage struct {
	URI          string `mapstructure:"uri"`
	DBUser       string `mapstructure:"user"`
	Database     string `mapstructure:"database"`
	SchemaPath   string `mapstructure:"schema_path"`
	MaxOpenConns int    `mapstructure:"max_open_conns"`
	MaxIdleConns int    `mapstructure:"max_idle_conns"`
}
