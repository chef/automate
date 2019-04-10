/*
Copyright © 2018 Chef Software

The a1upgrade package represents an A1 -> A2 upgrade. It is intended to extract
the A1 configuration and data and merge it into a deployable A2 deploy
configuration. It will then transform the any requisite service data, and deploy
the services with the transformed data and configuration.

*/

package a1upgrade

import (
	"time"

	dc "github.com/chef/automate/api/config/deployment"
)

const (
	/*
	   NOTE: Right now this might be true, but in the future when we support
	   migrating to non-local targets I can see a case where the user both
	   provides an A2 config to us that describes the target and then we'll
	   also want to analyze the A1 config and merge it into the user provided
	   A2 config. I guess we'll cross that bridge when we get to it.
	*/
	skipA1ConfigImportMsg = // nolint: deadcode, varcheck, megacheck
	`Chef Automate v2 config provided, skipping analysis of v1 configuration:

You have provided a Chef Automate v2 configuration file at the path below. No
settings will be imported from your Chef Automate v1 configuration. If this is
not intended, simply exit the upgrade and delete the file or omit the --config
option to the upgrade command as appropriate and rerun the upgrade command.

%s
`
)

// A1Upgrade represents a given A1Upgrade
type A1Upgrade struct {
	CreatedAt time.Time
	*A1Config
	A2Config                  *dc.AutomateConfig
	SkipUpgradePreflight      bool
	origin                    string
	hartifacts                string
	channel                   string
	upgradeStrategy           string
	adminPassword             string
	manifestDir               string
	PgDumpWait                int
	PgRestoreWait             int
	SkipBackup                bool
	FileMoveTimeout           int
	SkipBackupCheck           bool
	SkipDisasterRecoveryCheck bool
	SkipExternalESCheck       bool
	SkipFIPSCheck             bool
	SkipSAMLCheck             bool
	SkipWorkflowCheck         bool
	EnableChefServer          bool
	EnableWorkflow            bool
}

// Option represents an option that can be applied to an
// A1Upgrade. This is implemented using the "functional options"
// pattern. Various functions return an Option which can then be
// passed as arguments to NewA1Upgrade
//
// ```
// upgrade := NewA1Upgrade(a1upgrade.WithDeliverySecrets("/some/path.json"),
//                         a1upgrade.WithDeliveryRunning("/some/other/path.json"))
// ```
type Option func(*A1Upgrade) error

// NewA1Upgrade initializes a new A1Upgrade and applies any provided optional
// functions.
func NewA1Upgrade(options ...Option) (*A1Upgrade, error) {
	u := A1Upgrade{
		CreatedAt: time.Now(),
		A1Config:  NewA1Config(),
		A2Config:  dc.NewUserOverrideConfig(),
	}

	for _, option := range options {
		if err := option(&u); err != nil {
			return nil, err
		}
	}

	if u.adminPassword == "" {
		generatedPw, err := dc.GeneratePassword()
		if err != nil {
			return nil, err
		}
		u.adminPassword = generatedPw
	}
	err := u.A2Config.AddCredentials("Local Administrator", "admin", u.adminPassword)
	return &u, err
}

// WithDeliverySecrets returns an Option that sets the path to the
// Automate v1 Secrets file that should be used during the upgrade.
func WithDeliverySecrets(path string) Option {
	return func(u *A1Upgrade) error {
		u.DeliverySecretsPath = path
		return u.LoadDeliverySecrets()
	}
}

// WithDeliveryRunning returns an Option that sets the path to the
// Automate v1 configuration that should be used during the upgrade.
func WithDeliveryRunning(path string) Option {
	return func(u *A1Upgrade) error {
		u.DeliveryRunningPath = path
		return u.LoadDeliveryRunning()
	}
}

// WithDeliveryRunning return an Option that sets the path to
// chef-server-running.json and loads it if EnableChefServer is true.
func WithChefServerRunning(path string, EnableChefServer bool) Option {
	// The EnableChefServer argument is kinda redundant with the
	// WithChefServerEnabled functional option, but we need it here so we do not
	// depend on the Options being given/processed in a specific order; i.e., if
	// we check for u.EnableChefServer then we would have to process the
	// WithChefServerEnabled option before this one.
	return func(u *A1Upgrade) error {
		if !EnableChefServer {
			return nil
		}
		u.ChefServerRunningPath = path
		return u.LoadChefServerRunning()
	}
}

// WithHartifactsPath returns an Option that sets the hartifacts path
// in the A2 config.
func WithHartifactsPath(path string) Option {
	return func(u *A1Upgrade) error {
		return u.SetHartifactsPath(path)
	}
}

// WithOverrideOrigin returns an Option that sets the override the in
// the A2 config.
func WithOverrideOrigin(origin string) Option {
	return func(u *A1Upgrade) error {
		return u.SetOverrideOrigin(origin)
	}
}

// WithManifestDir returns an Option that sets the manifest directory
// in the A2 config.
func WithManifestDir(path string) Option {
	return func(u *A1Upgrade) error {
		return u.SetManifestDir(path)
	}
}

// WithChannel returns an Option that sets the channel the in the A2
// config.
func WithChannel(channel string) Option {
	return func(u *A1Upgrade) error {
		return u.SetChannel(channel)
	}
}

// WithUpgradeStrategy returns an Option that sets the upgrade strategy the in
// the A2 config.
func WithUpgradeStrategy(strategy string) Option {
	return func(u *A1Upgrade) error {
		return u.SetUpgradeStrategy(strategy)
	}
}

// WithAdminPassword returns an Option that sets the admin password in
// the A2 configuration.
func WithAdminPassword(password string) Option {
	return func(u *A1Upgrade) error {
		u.adminPassword = password
		return u.SetAdminPassword(password)
	}
}

// WithA2Config returns an Option option that sets the a2 config that
// should be used for the upgrade.
func WithA2Config(a2Config *dc.AutomateConfig) Option {
	return func(u *A1Upgrade) error {
		return u.SetA2Config(a2Config)
	}
}

// WithA2ConfigPath returns an Option that sets the path to a TOML
// representation of the configuration that should be used for the
// upgrade.
func WithA2ConfigPath(path string, options ...dc.AutomateConfigOpt) Option {
	return func(u *A1Upgrade) error {
		if path == "" {
			return nil
		}
		a2Config, err := dc.LoadUserOverrideConfigFile(path, options...)
		if err != nil {
			return err
		}
		return u.SetA2Config(a2Config)
	}
}

func WithWorkflowEnabled(enabled bool) Option {
	return func(u *A1Upgrade) error {
		u.EnableWorkflow = enabled
		return nil
	}
}

func WithChefServerEnabled(enabled bool) Option {
	return func(u *A1Upgrade) error {
		u.EnableChefServer = enabled
		return nil
	}
}

// SkipUpgradePreflight returns an Option that indicates whether
// to skip the upgrade preflight checks.
func SkipUpgradePreflight(skip bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipUpgradePreflight = skip
		return nil
	}
}

// SetPostgresDumpWait returns an Option that sets the PostgreSQL dump
// timeout in seconds.
func SetPostgresDumpWait(seconds int) Option {
	return func(u *A1Upgrade) error {
		u.PgDumpWait = seconds
		return nil
	}
}

// SetPostgresRestoreWait returns an Option that sets the PostgreSQL
// restore timeout in seconds.
func SetPostgresRestoreWait(seconds int) Option {
	return func(u *A1Upgrade) error {
		u.PgRestoreWait = seconds
		return nil
	}
}

// SkipUpgradeBackup returns an Option that indicates whether
// to skip A1 backup during upgrade.
func SkipUpgradeBackup(backup bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipBackup = backup
		return nil
	}
}

// SetFileMoveTimeout returns an Option that sets the timeout in
// seconds for moving files during upgrade from A1.
func SetFileMoveTimeout(seconds int) Option {
	return func(u *A1Upgrade) error {
		u.FileMoveTimeout = seconds
		return nil
	}
}

// SetA2Config replaces upgrade's A2Config with the given a2Config
func (u *A1Upgrade) SetA2Config(a2Config *dc.AutomateConfig) error {
	u.A2Config = a2Config
	return nil
}

// SetHartifactsPath overrides the hartifacts path.
// configuration during initialization.
func (u *A1Upgrade) SetHartifactsPath(path string) error {
	u.hartifacts = path
	return u.A2Config.SetHartifactsPath(path)
}

// SetOverrideOrigin sets the override origin for the upgrade.
func (u *A1Upgrade) SetOverrideOrigin(origin string) error {
	u.origin = origin
	return u.A2Config.SetOverrideOrigin(origin)
}

// SetManifestDir sets the manifest dir for the upgrade.
func (u *A1Upgrade) SetManifestDir(dir string) error {
	u.manifestDir = dir
	return u.A2Config.SetManifestDir(dir)
}

// SetChannel sets the channel for the upgrade.
func (u *A1Upgrade) SetChannel(channel string) error {
	u.channel = channel
	return u.A2Config.SetChannel(channel)
}

// SetUpgradeStrategy sets the upgrade strategy for the upgrade.
func (u *A1Upgrade) SetUpgradeStrategy(strategy string) error {
	u.upgradeStrategy = strategy
	return u.A2Config.SetUpgradeStrategy(strategy)
}

// SetAdminPassword sets the admin password for the upgrade.
func (u *A1Upgrade) SetAdminPassword(password string) error {
	return u.A2Config.SetAdminPassword(password)
}

func (u *A1Upgrade) SetChefServerEnabled(enabled bool) error {
	return u.A2Config.SetChefServerEnabled(enabled)
}

func (u *A1Upgrade) SetWorkflowEnabled(enabled bool) error {
	return u.A2Config.SetWorkflowEnabled(enabled)
}

// Databases returns a slice of A1Databases that need to me migrated
func (u *A1Upgrade) Databases() []A1Database {
	// Default database that we will migrate
	// NOTE: This only includes tables related to compliance & secrets services
	deliveryDB := A1DeliveryDB

	// If Workflow is enabled, we backup everything from the delivery database
	// the main reason is that we need types and functions attached to it
	if u.EnableWorkflow {
		deliveryDB.includedTables = []string{}
		deliveryDB.excludedTables = []string{"pg_stat_repl"}
	}

	dbs := []A1Database{deliveryDB}

	// If ChefServer is enabled, we append more databases
	if u.EnableChefServer {
		dbs = append(dbs, CSDatabases...)
	}

	return dbs
}

// SkipBackupConfiguredCheck returns an Option that indicates whether
// to skip the A1 upgrade preflight check for configured backups.
func SkipBackupConfiguredCheck(backup bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipBackupCheck = backup
		return nil
	}
}

// SkipDisasterRecoveryConfiguredCheck returns an Option that indicates whether
// to skip the A1 upgrade preflight check for disaster recovery.
func SkipDisasterRecoveryConfiguredCheck(dr bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipDisasterRecoveryCheck = dr
		return nil
	}
}

// SkipExternalESConfiguredCheck returns an Option that indicates whether
// to skip A1 upgrade preflight check for external elasticsearch.
func SkipExternalESConfiguredCheck(es bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipExternalESCheck = es
		return nil
	}
}

// SkipFIPSConfiguredCheck returns an Option that indicates whether
// to skip A1 upgrade preflight check for FIPS.
func SkipFIPSConfiguredCheck(fips bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipFIPSCheck = fips
		return nil
	}
}

// SkipSAMLConfiguredCheck returns an Option that indicates whether
// to skip A1 upgrade preflight check for SAML.
func SkipSAMLConfiguredCheck(saml bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipSAMLCheck = saml
		return nil
	}
}

// SkipWorkflowConfiguredCheck  returns an Option that indicates whether
// to skip the A1 upgrade preflight check for workflow.
func SkipWorkflowConfiguredCheck(workflow bool) Option {
	return func(u *A1Upgrade) error {
		u.SkipWorkflowCheck = workflow
		return nil
	}
}

// GenerateA2ConfigIfNoneProvided creates an Automate 2 configuration from
// Automate 1 configuration if a2ConfigPath is the zero value for a string
// (""). If a2ConfigPath is not the zero value, it's assumed that
// 1. an a2 config has been loaded via the initializer options, and:
// 2. the a2 config that we loaded contains user customizations that we must
//    not override.
func (u *A1Upgrade) GenerateA2ConfigIfNoneProvided(a2ConfigPath string) error {
	if a2ConfigPath != "" {
		return nil
	}
	cfg, err := generateMigrationOverrideConfig(u.DeliveryRunning, u.DeliverySecrets)
	if err != nil {
		return err
	}
	u.A2Config = cfg
	// Ensure we re-apply any a2 config that was previously set.
	// There may be a prettier way to do this but this way works regardless of
	// what order we get the options
	err = u.SetHartifactsPath(u.hartifacts)
	if err != nil {
		return err
	}

	err = u.SetOverrideOrigin(u.origin)
	if err != nil {
		return err
	}

	err = u.SetChannel(u.channel)
	if err != nil {
		return err
	}

	err = u.SetAdminPassword(u.adminPassword)
	if err != nil {
		return err
	}

	err = u.SetManifestDir(u.manifestDir)
	if err != nil {
		return err
	}

	err = u.SetChefServerEnabled(u.EnableChefServer)
	if err != nil {
		return err
	}

	err = u.SetWorkflowEnabled(u.EnableWorkflow)
	if err != nil {
		return err
	}

	return u.A2Config.ValidateWithGlobalAndDefaults()
}
