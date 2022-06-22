package config

import "github.com/chef/automate/lib/tls/certs"

// Configuration for the Report Manager Service
type ReportManager struct {
	Service          Service `mapstructure:"service"`
	Log              Log
	CerealConfig     CerealConfig     `mapstructure:"cereal"`
	ComplianceConfig ComplianceConfig `mapstructure:"compliance"`
	Storage          Storage          `mapstructure:"storage"`
	certs.TLSConfig  `mapstructure:"tls"`
	ObjStore         ObjStore `mapstructure:"objstore"`
	Minio            Minio    `mapstructure:"minio"`
}

type Minio struct {
	EndPoint                     string `mapstructure:"endpoint"`
	RootUser                     string `mapstructure:"root_user"`
	RootPassword                 string `mapstructure:"root_password"`
	EnableSsl                    bool   `mapstructure:"enable_ssl"`
	Cert                         string `mapstructure:"cert"`
	ConcurrentOpenSearchRequests int    `mapstructure:"concurrent_open_search_requests"`
	ConcurrentMinioRequests      int    `mapstructure:"concurrent_minio_requests"`
}

type ObjStore struct {
	BucketName string `mapstructure:"bucket"`
}

type Service struct {
	Port                 int
	Host                 string
	Message              string
	EnableLargeReporting bool `mapstructure:"enable_large_reporting"`
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
