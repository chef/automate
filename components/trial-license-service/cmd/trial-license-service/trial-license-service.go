package main

import (
	"fmt"
	"net/url"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/trial-license-service/pkg/server"
	"github.com/chef/automate/lib/logger"
)

func main() {
	cmd := &cobra.Command{
		Use:   "trial-license-service <CONFIG_FILE>",
		Short: "Vends trial licenses for Chef Automate",
		Args:  cobra.ExactArgs(1),
		Run:   serve,
	}

	if err := cmd.Execute(); err != nil {
		fail(err)
	}
}

type config struct {
	ListenHost        string `mapstructure:"listen_host"`
	ListenPort        string `mapstructure:"listen_port"`
	LicGenURL         string `mapstructure:"lic_gen_url"`
	LicGenAuthToken   string `mapstructure:"lic_gen_auth_token"`
	SegmentIOWriteKey string `mapstructure:"segmentio_write_key"`
	LogFormat         string `mapstructure:"log_format"`
	LogLevel          string `mapstructure:"log_level"`
}

func defaultConfig() config {
	return config{
		LogFormat: "json",
		LogLevel:  "info",
	}
}

func parseConfig(configPath string) (*config, error) {
	viper.SetConfigFile(configPath)
	if err := viper.ReadInConfig(); err != nil {
		msg := fmt.Sprintf("unable to read config file at %q", configPath)
		return nil, errors.Wrap(err, msg)
	}

	cfg := defaultConfig()
	if err := viper.Unmarshal(&cfg); err != nil {
		msg := fmt.Sprintf("unable to parse config file at %q", configPath)
		return nil, errors.Wrap(err, msg)
	}

	required := []struct {
		name  string
		value string
	}{
		{"listen_host", cfg.ListenHost},
		{"listen_port", cfg.ListenPort},
		{"lic_gen_url", cfg.LicGenURL},
		{"lic_gen_auth_token", cfg.LicGenAuthToken},
		{"segmentio_write_key", cfg.SegmentIOWriteKey},
	}
	for _, r := range required {
		if r.value == "" {
			return nil, fmt.Errorf("missing required config key '%s'", r.name)
		}
	}

	return &cfg, nil
}

func serve(_ *cobra.Command, args []string) {
	cfg, err := parseConfig(args[0])
	if err != nil {
		fail(err)
	}

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize logger"))
	}

	lgsURL, err := url.Parse(cfg.LicGenURL)
	if err != nil {
		fail(errors.Wrapf(err, "could not parse licGenURL %q", cfg.LicGenURL))
	}
	l.Info("about to serve...")
	fail(server.Start(cfg.ListenHost, cfg.ListenPort, lgsURL, cfg.LicGenAuthToken, cfg.SegmentIOWriteKey, l))
}

// fail outputs the error and exits with a non-zero code
func fail(err error) {
	// no error check: if this goes wrong, we're in trouble anyways
	fmt.Fprint(os.Stderr, err.Error()) // nolint: gas
	os.Exit(1)
}
