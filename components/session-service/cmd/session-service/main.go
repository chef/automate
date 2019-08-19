package main

import (
	"fmt"
	"net/url"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/session-service/migration"
	"github.com/chef/automate/components/session-service/oidc"
	"github.com/chef/automate/components/session-service/server"
	"github.com/chef/automate/lib/logger"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
)

func main() {
	cmd := &cobra.Command{
		Use:   "session-service [config.yml]",
		Short: "session-service is a2's management service for browser sessions",
		Args:  cobra.MaximumNArgs(1),
		Run:   serve,
	}

	// Note: we don't use global string variables, since all access to flags
	// goes through viper's Unmarshalled config struct below
	cmd.PersistentFlags().StringP("log-level", "l", "info", "log level")
	cmd.PersistentFlags().StringP("log-format", "f", "text", "log format")
	cmd.PersistentFlags().String("http-listen", "127.0.0.1", "listen interface")
	cmd.PersistentFlags().Int("http-port", 7777, "listen port")
	cmd.PersistentFlags().String("issuer-url", "", "issuer URL")
	cmd.PersistentFlags().String("client-id", "automate-session", "client ID")
	cmd.PersistentFlags().String("client-secret", "", "client secret")
	cmd.PersistentFlags().String("redirect-url", "/callback", "redirect URL")
	cmd.PersistentFlags().String("bldr-signin-url", "", "bldr signin URL")
	cmd.PersistentFlags().String("bldr-client-id", "", "bldr oauth2 client id")
	cmd.PersistentFlags().String("bldr-client-secret", "", "bldr oauth2 client secret")
	cmd.PersistentFlags().String("signin-url", "/signin", "signin URL")
	cmd.PersistentFlags().String("postgres-url", "", "postgresql URL")

	// bind all flags to viper config
	if err := viper.BindPFlags(cmd.PersistentFlags()); err != nil {
		fail(err)
	}

	if err := cmd.Execute(); err != nil {
		fail(err)
	}
}

type config struct {
	LogFormat        string `mapstructure:"log-format"`
	LogLevel         string `mapstructure:"log-level"`
	HTTPListen       string `mapstructure:"http-listen"`
	HTTPPort         uint   `mapstructure:"http-port"`
	DexURL           string `mapstructure:"dex-url"`
	IssuerURL        string `mapstructure:"issuer-url"`
	ClientID         string `mapstructure:"client-id"`
	ClientSecret     string `mapstructure:"client-secret"`
	RedirectURL      string `mapstructure:"redirect-url"`
	BldrSignInURL    string `mapstructure:"bldr-signin-url"`
	BldrClientID     string `mapstructure:"bldr-client-id"`
	BldrClientSecret string `mapstructure:"bldr-client-secret"`
	SignInURL        string `mapstructure:"signin-url"`
	PostgresURL      string `mapstructure:"postgres-url"`
	Database         string `mapstructure:"database"`
	MigrationsPath   string `mapstructure:"migrations-path"`
	certs.TLSConfig  `mapstructure:"tls"`
}

func serve(_ *cobra.Command, args []string) {
	if len(args) == 1 {
		viper.SetConfigFile(args[0])
		if err := viper.ReadInConfig(); err != nil {
			fail(errors.Wrap(err, "read config"))
		}
	}
	cfg := config{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(errors.Wrap(err, "unmarshal config"))
	}

	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(errors.Wrap(err, "Failed to load certificates"))
	}

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "init logger"))
	}

	closer, err := tracing.NewGlobalTracer("session-service")
	if err == nil {
		defer tracing.CloseQuietly(closer)
	}

	mustBeADirectory(cfg.MigrationsPath)
	var urlStr string
	if cfg.PostgresURL == "" {
		urlStr, err = platform_config.PGURIFromEnvironment(cfg.Database)
		if err != nil {
			fail(errors.WithMessage(err, "Failed to get pg uri"))
		}
	} else {
		urlStr = cfg.PostgresURL
	}
	pgURL := mustParseURL(urlStr)
	migrationCfg := migration.Config{
		Path:   cfg.MigrationsPath,
		PG:     pgURL,
		Logger: l,
	}

	bind := fmt.Sprintf("%s:%d", cfg.HTTPListen, cfg.HTTPPort)
	signInURL := mustParseURL(cfg.SignInURL)

	redirectURL := mustParseURL(cfg.RedirectURL)
	issuerURL := mustParseURL(cfg.IssuerURL)
	dexURL := mustParseURL(cfg.DexURL)

	bldrSignInURL := mustParseURL(cfg.BldrSignInURL)
	var bldrClient *server.BldrClient
	// config_request validation ensures that if one bldr key is populated they all are.
	if cfg.BldrClientID != "" {
		bldrClient = &server.BldrClient{
			SignInURL:    bldrSignInURL,
			ClientID:     cfg.BldrClientID,
			ClientSecret: cfg.BldrClientSecret,
		}
	}

	oidcConfig := oidc.Config{
		ClientID:     cfg.ClientID,
		ClientSecret: cfg.ClientSecret,
		RedirectURL:  redirectURL,
		IssuerURL:    issuerURL,
		DexURL:       dexURL,
	}

	srv, err := server.New(
		l,
		&migrationCfg,
		oidcConfig,
		bldrClient,
		signInURL,
		serviceCerts)
	if err != nil {
		fail(errors.Wrap(err, "init server"))
	}
	if err := srv.ListenAndServe(bind); err != nil {
		fail(errors.Wrap(err, "listen/bind"))
	}
}

// fail outputs the error and exits with a non-zero code
func fail(err error) {
	// no error check: if this goes wrong, we're in trouble anyways
	fmt.Fprintln(os.Stderr, err.Error()) // nolint: gas
	os.Exit(1)
}

func mustParseURL(s string) *url.URL {
	u, err := url.Parse(s)
	if err != nil {
		fail(errors.Wrapf(err, "parse URL %#v", s))
	}
	return u
}

func mustBeADirectory(path string) {
	stat, err := os.Stat(path)
	if err == nil && stat.IsDir() {
		return // everything's in its right place
	} else if err != nil {
		fail(errors.Wrapf(err, "open path %#v", path))
	}
	fail(fmt.Errorf("path %#v is not a directory", path))
}
