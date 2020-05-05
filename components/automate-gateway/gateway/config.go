package gateway

import (
	"net/url"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"

	"github.com/chef/automate/lib/tls/certs"
)

type Config struct {
	ExternalFqdn string       `mapstructure:"external_fqdn" toml:"external_fqdn"`
	GrpcClients  ClientConfig `mapstructure:"grpc_clients" toml:"grpc_clients"`
	GRPCPort     int          `mapstructure:"grpc_port" toml:"grpc_port"`
	Hostname     string       `mapstructure:"host" toml:"host"`
	Log          struct {
		Level string `mapstructure:"level" toml:"level"`
	} `mapstructure:"log" toml:"log"`
	OpenAPIUIDir      string `mapstructure:"open_api_ui_dir" toml:"open_api_ui_dir"`
	Port              int    `mapstructure:"port" toml:"port"`
	ServiceCerts      *certs.ServiceCerts
	TLSConfig         certs.TLSConfig `mapstructure:"tls" toml:"tls"`
	TrialLicenseURL   string          `mapstructure:"trial_license_url" toml:"trial_license_url"`
	EnableAppsFeature bool            `mapstructure:"enable_apps_feature" toml:"enable_apps_feature"`
	DataCollector     struct {
		DisableLimiter     bool `mapstructure:"disable_limiter" toml:"disable_limiter"`
		LimiterMaxRequests int  `mapstructure:"limiter_max_requests" toml:"limiter_max_requests"`
	} `mapstructure:"data_collector" toml:"data_collector"`
}

type gwRouteFeatureFlags map[string]bool

// ConfigFromViper returns a Gateway config from the services configuration
// file and the viper CLI arguments.
func ConfigFromViper() (Config, error) {
	config := Config{}

	// Unmarshall the viper config into the server Config
	if err := viper.Unmarshal(&config); err != nil {
		logrus.WithError(err).Error("Failed to marshall config options to server config")
		return config, err
	}

	return config, config.loadServiceCerts()
}

// New initializes a *Server from the passed options
func New(cfg Config) *Server {
	s := &Server{Config: cfg}
	s.Config.OpenAPIUIDir = "/src/components/automate-gateway/third_party/swagger-ui/"
	s.logger = logrus.WithFields(logrus.Fields{
		"hostname":            cfg.Hostname,
		"https_port":          cfg.Port,
		"grpc_port":           cfg.GRPCPort,
		"null_backend_socket": cfg.GrpcClients.NullBackendSock,
	})

	return s
}

func (c *Config) gwRouteFeatureFlags() gwRouteFeatureFlags {
	g := make(gwRouteFeatureFlags)
	g["applications"] = c.EnableAppsFeature
	return g
}

func (c *Config) loadServiceCerts() error {
	// Fix any relative paths that might be in the config file
	c.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := c.TLSConfig.ReadCerts()
	if err != nil {
		return errors.Wrap(err, "loading x509 key pair and/or root CA certificate")
	}

	c.ServiceCerts = serviceCerts

	return nil
}

func (c *Config) trialLicenseURL() (*url.URL, error) {
	if tlsURL := c.TrialLicenseURL; tlsURL != "" {
		return url.Parse(tlsURL)
	}

	return nil, nil
}

func (c *Config) automateURL() (*url.URL, error) {
	if URL := c.ExternalFqdn; URL != "" {
		return url.Parse(URL)
	}

	return nil, nil
}
