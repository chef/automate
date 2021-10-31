package config

import "github.com/chef/automate/lib/tls/certs"

// Configuration for the Report Manager Service
type ReportManager struct {
	Service         Service
	Log             Log
	CerealConfig    CerealConfig `mapstructure:"cereal"`
	Storage         Storage      `mapstructure:"storage"`
	certs.TLSConfig `mapstructure:"tls"`
	ObjStore        ObjStore `mapstructure:"objstore"`
}

type ObjStore struct {
	BucketName string `mapstructure:"bucket"`
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

type Storage struct {
	URI          string `mapstructure:"uri"`
	DBUser       string `mapstructure:"user"`
	Database     string `mapstructure:"database"`
	SchemaPath   string `mapstructure:"schema_path"`
	MaxOpenConns int    `mapstructure:"max_open_conns"`
	MaxIdleConns int    `mapstructure:"max_idle_conns"`
}
