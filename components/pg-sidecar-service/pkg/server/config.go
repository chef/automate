package server

import (
	"fmt"

	"github.com/chef/automate/lib/tls/certs"
)

// PGWConfig defines the available configuration options for the postgres gateway
type PGWConfig struct {
	Host          string `mapstructure:"host"`
	Port          uint64 `mapstructure:"port"`
	SuperuserName string `mapstructure:"superuser_name"`
}

// Config defines the available configuration options for this service
type Config struct {
	Host     string `mapstructure:"host"`
	Port     int    `mapstructure:"port"`
	LogLevel string `mapstructure:"log_level"`
	//PGW             PGWConfig `mapstructure:"pgw" toml:"pgw"`
	certs.TLSConfig `mapstructure:"tls"`
}

// ListenAddress returns the servers listen address from the Host and Port
func (c *Config) ListenAddress() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}
