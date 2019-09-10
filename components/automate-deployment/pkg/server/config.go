package server

import (
	"fmt"

	api "github.com/chef/automate/api/interservice/deployment"
)

// Config - runtime configuration options for the server
type Config struct {
	ListenAddress            string `mapstructure:"listen_address"`
	Port                     uint32 `mapstructure:"port"`
	LogLevel                 string `mapstructure:"log_level"`
	LogFormat                string `mapstructure:"log_format"`
	StagingDir               string `mapstructure:"staging_dir"`
	ConvergeIntervalSecs     uint32 `mapstructure:"converge_interval_secs"`
	ConvergeDisableFile      string `mapstructure:"converge_disable_file"`
	EnsureStatusTimeoutSecs  uint32 `mapstructure:"ensure_status_timeout_secs"`
	EnsureStatusIntervalSecs uint32 `mapstructure:"ensure_status_interval_secs"`
}

func (cfg *Config) getAddressString() string {
	return fmt.Sprintf("%s:%d", cfg.ListenAddress, cfg.Port)
}

// DefaultServerConfig returns a new instance of the server config with default
// values applied.
func DefaultServerConfig() *Config {
	return &Config{
		ConvergeDisableFile: api.ConvergeDisableFilePath,
	}
}
