package server

import (
	"fmt"
	"net/url"
	"os"
	"path"
	"strconv"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"

	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is the LicenseControlServer client and server configuration
type Config struct {
	Host             string          `mapstructure:"host" toml:"host"`
	Port             int             `mapstructure:"port" toml:"port"`
	LicenseTokenPath string          `mapstructure:"license_token_path" toml:"license_token_path"`
	MigrationsPath   string          `mapstructure:"migrations_path" toml:"migrations_path"`
	PGURL            string          `mapstructure:"pg_url" toml:"pg_url"`
	Database         string          `mapstructure:"database" toml:"database"`
	TLSConfig        certs.TLSConfig `toml:"tls"`
	OptOutPath       string          `mapstructure:"opt_out_path" toml:"opt_out_path"`
	URL              string          `mapstructure:"url" toml:"url"`
	OptOut           string          `mapstructure:"opt_out" toml:"opt_out"`
	TelemetryEnabled bool
	ServiceCerts     *certs.ServiceCerts
}

// ListenAddress is the address where gRPC server will bind and listen
func (c Config) ListenAddress() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

// ConfigFromViper marshals configuration from the LicenseControlServer servers
// configuration file that is parsed Viper into the Config struct.
func ConfigFromViper() (*Config, error) {
	config := &Config{}

	// Unmarshall the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithError(err).Error("Failed to marshall config options to server config")
		return config, err
	}

	// Validates that the configuration has a valid host/port
	_, err := url.ParseRequestURI(path.Join("http://", config.ListenAddress()))
	if err != nil {
		log.WithError(err).Errorf(
			"Listen adddress '%s' is not valid. Please check the 'host' and 'port' configuration",
			config.ListenAddress())
		return config, err
	}

	// Marshall the top-level viper TLS config into the embedded TLS config.
	config.TLSConfig = certs.TLSConfig{
		CertPath:       viper.GetString("cert_path"),
		KeyPath:        viper.GetString("key_path"),
		RootCACertPath: viper.GetString("root_ca_path"),
	}

	// Fix any relative paths that might be in the config file
	config.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := config.TLSConfig.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Failed to loading x509 key pair and/or root CA certificate")
		return config, err
	}
	config.ServiceCerts = serviceCerts

	if config.PGURL == "" {
		config.PGURL, err = platform_config.PGURIFromEnvironment(config.Database)
		if err != nil {
			return config, errors.Wrap(err, "failed to get pg url")
		}
	}

	log.WithFields(log.Fields{
		"telemetry opt out path": config.OptOutPath,
	}).Info("Telemetry opt out path.")

	if config.OptOutPath == "" {
		err = fmt.Errorf("Telemetry opt-out path is blank, unable to determine if telemetry should be disabled.")
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Unable to determine telemetry opt out path.")
		return config, err
	}

	optedOut, err := isOptedOut(config.OptOut, config.OptOutPath)
	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Errorf("Failed to figure out if telemetry opted out config %v", config)
		return config, err
	}

	config.TelemetryEnabled = !optedOut

	return config, nil
}

// This is migrating from using a local file to store if automate has telemetry off
// to using the automate config opt_out parameter.
// if the optOutInput is an empty string then the user has not set the automate config 'opt_out' boolean
// If this has not been updated then check if the user has updated the old way of setting telemetry which was
// the existence of a file at the path of the optOutPath variable.
func isOptedOut(optOutInput string, optOutPath string) (bool, error) {
	migrationOptOutFileExists, err := migrationOptOutFileExists(optOutPath)
	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Failed to check for opt_out file")
		return true, err
	}

	if optOutInput == "" { // has the user set the opt_out flag with the automate config
		return migrationOptOutFileExists, nil
	}

	if migrationOptOutFileExists {
		err := removeOptOutFile(optOutPath)
		if err != nil {
			log.WithFields(log.Fields{
				"err": err.Error(),
			}).Error("Failed to remove opt_out file")
			return true, err
		}
	}

	isOptedOut, err := strconv.ParseBool(optOutInput)
	if err != nil {
		log.WithFields(log.Fields{
			"err":  err.Error(),
			"flag": optOutInput,
		}).Error("Failed to parse opt out flag")

		return true, err
	}

	return isOptedOut, nil
}

func migrationOptOutFileExists(optOutPath string) (bool, error) {
	if _, err := os.Stat(optOutPath); err != nil {
		if os.IsNotExist(err) {
			return false, nil
		}
		return true, err
	}
	return true, nil
}

func removeOptOutFile(optOutPath string) error {
	err := os.Remove(optOutPath)
	if os.IsNotExist(err) {
		err = nil
	}

	return err
}
