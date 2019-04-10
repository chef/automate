package deployment

import (
	"fmt"
	"hash/fnv"
	"io/ioutil"
	"os"
	"path/filepath"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/pkg/errors"
)

// NewUserOverrideConfig returns a completely blank config struct onto which we
// will merge user-set configs.
//
// *WARN* Only values that are overridden by the user should exist here *WARN*
// *WARN* otherwise these values will be persisted forever *WARN*
func NewUserOverrideConfig() *AutomateConfig { return &AutomateConfig{} }

// AutomateConfigOpt is a functional option for LoadUserOverrideConfigFile
type AutomateConfigOpt func(*AutomateConfig) error

// LoadUserOverrideConfigFile loads a user-specified override config from a toml
// file and also applies specific values to the deployment service's override
// origin and hartifacts path if they are set. the following things are
// validated:
// - hartifacts path is a directory that exists on the local system (where the CLI is run)
// - the resulting config, when merged with defaults, is a valid automate config
func LoadUserOverrideConfigFile(file string, options ...AutomateConfigOpt) (*AutomateConfig, error) {
	data, err := ioutil.ReadFile(file)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to read config file located at %s", file)
	}

	cfg, err := NewUserOverrideConfigFromTOML(data)
	if err != nil {
		return nil, errors.Wrap(err, "Config file must be a valid automate config")
	}

	err = WithConfigOptions(cfg, options...)
	if err != nil {
		return nil, err
	}

	return cfg, nil
}

// WithConfigOptions applies the given AutomateConfigOpts to an AutomateConfig
func WithConfigOptions(cfg *AutomateConfig, options ...AutomateConfigOpt) error {
	for _, option := range options {
		if err := option(cfg); err != nil {
			return err
		}
	}

	return nil
}

/*
NOTE: by allowing users to specify the hartifacts path and override origin via
the CLI instead of in the config, we're forced into a pattern of needing a way
to merge sparse versions of our config struct into the config specified via the
toml file. This will be true for all "deep setters" defined below:
*/

// WithHartifacts returns a functional option to set hartifacts path on
// AutomateConfig in LoadUserOverrideConfigFile
func WithHartifacts(path string) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetHartifactsPath(path)
	}
}

// SetHartifactsPath does a "deep set" to set hartifacts path on
// AutomateConfig, which is needed to be able to set hartifacts path on the
// config object when the path is specified on the command line
func (c *AutomateConfig) SetHartifactsPath(path string) error {
	fqPath, err := expandAndValidateDirectory(path, "HARTIFACTS")
	if err != nil {
		return err
	}

	if fqPath != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						HartifactsPath: w.String(fqPath),
					},
				},
			},
		})
	}
	return nil
}

// WithOrigin returns a functional option to set override origin on
// AutomateConfig in LoadUserOverrideConfigFile
func WithOrigin(origin string) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetOverrideOrigin(origin)
	}
}

// SetOverrideOrigin does a "deep set" to set override origin on
// AutomateConfig, which is needed to support setting override origin on the
// command line
func (c *AutomateConfig) SetOverrideOrigin(origin string) error {
	if origin != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						OverrideOrigin: w.String(origin),
					},
				},
			},
		})
	}
	return nil
}

// WithManifestDir returns an AutomateConfigOpt that sets the manifest path on
// an AutomateConfig
func WithManifestDir(path string) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetManifestDir(path)
	}
}

// SetManifestDir does a "deep set" to set the manifest directory on
// AutomateConfig
func (c *AutomateConfig) SetManifestDir(dir string) error {
	fqPath, err := expandAndValidateDirectory(dir, "MANIFEST_DIR")
	if err != nil {
		return err
	}

	if fqPath != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						ManifestDirectory: w.String(fqPath),
					},
				},
			},
		})
	}
	return nil
}

// WithChannel returns a functional option to set the channel in the deployment
// config.
func WithChannel(channel string) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetChannel(channel)
	}
}

// SetChannel does a "deep set" to set deployment channel in the
// AutomateConfig, which is needed to support setting the channel via the
// command line
func (c *AutomateConfig) SetChannel(channel string) error {
	if channel != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						Channel: w.String(channel),
					},
				},
			},
		})
	}
	return nil
}

// WithUpgradeStrategy returns a functional option to set the upgrade strategy
// in the deployment config.
func WithUpgradeStrategy(strategy string) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetUpgradeStrategy(strategy)
	}
}

// SetUpgradeStrategy does a "deep set" to set deployment upgrade strategy in the
// AutomateConfig, which is needed to support setting the upgrade strategy via the
// command line
func (c *AutomateConfig) SetUpgradeStrategy(strategy string) error {
	if strategy != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						UpgradeStrategy: w.String(strategy),
					},
				},
			},
		})
	}
	return nil
}

func WithChefServerEnabled(enabled bool) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetChefServerEnabled(enabled)
	}
}

func (c *AutomateConfig) SetChefServerEnabled(enabled bool) error {
	return c.OverrideConfigValues(&AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					EnableChefServer: w.Bool(enabled),
				},
			},
		},
	})
}

func WithWorkflowEnabled(enabled bool) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetWorkflowEnabled(enabled)
	}
}

func (c *AutomateConfig) SetWorkflowEnabled(enabled bool) error {
	return c.OverrideConfigValues(&AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					EnableWorkflow: w.Bool(enabled),
				},
			},
		},
	})
}

func WithDeploymentOrderStressMode(enabled bool) func(*AutomateConfig) error {
	return func(c *AutomateConfig) error {
		return c.SetDeploymentOrderStressMode(enabled)
	}
}

func (c *AutomateConfig) SetDeploymentOrderStressMode(enabled bool) error {
	return c.OverrideConfigValues(&AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					EnableDeploymentOrderStressMode: w.Bool(enabled),
				},
			},
		},
	})
}

// SetAdminPassword does a "deep set" to set deployment admin password in the
// AutomateConfig, which is needed to support setting the admin password via the
// command line
func (c *AutomateConfig) SetAdminPassword(password string) error {
	if password != "" {
		return c.OverrideConfigValues(&AutomateConfig{
			Deployment: &ConfigRequest{
				V1: &ConfigRequest_V1{
					Svc: &ConfigRequest_V1_Service{
						AdminUser: &ConfigRequest_V1_AdminUser{
							Password: w.String(password),
						},
					},
				},
			},
		})
	}
	return nil
}

// expandAndValidateDirectory takes as input a path and returns a fully
// qualified path if it exists. It returns an error if the path does not exist
func expandAndValidateDirectory(path string, name string) (string, error) {
	var err error

	if path != "" {
		path, err = filepath.Abs(path)
		if err != nil {
			return "", errors.Wrapf(err, "[%s] must be valid path to a directory", name)
		}
		if _, err = os.Stat(path); err != nil {
			return "", errors.Wrapf(err, "[%s] must be valid path to a directory", name)
		}
	}
	return path, nil
}

// NewUserOverrideConfigFromTOML takes a byte-slice representing a TOML config
// and returns an override config.
//
// *WARN* Same warnings apply as NewUserOverrideConfig *WARN*
func NewUserOverrideConfigFromTOML(data []byte) (*AutomateConfig, error) {
	cfg := NewUserOverrideConfig()
	err := toml.StrictUnmarshal(data, cfg)
	if err != nil {
		return nil, err
	}
	return cfg, nil
}

// MergeWithDefaults takes as an argument an AutomateConfig and returns a new
// instance of an AutomateConfig that has been merged with the defaults from
// DefaultAutomateConfig
func MergeWithDefaults(c *AutomateConfig) (*AutomateConfig, error) {
	merged := NewUserOverrideConfig()
	err := config.Merge(DefaultAutomateConfig(), c, merged)
	if err != nil {
		return nil, err
	}
	return merged, nil
}

// OverrideConfigValues allows the caller to specify a sparse set of config
// values to apply on top of an existing config. This is a useful pattern to
// avoid needing to check whether a nested value of the config is nil or not
// before setting fields on it
func (c *AutomateConfig) OverrideConfigValues(in *AutomateConfig) error {
	merged := NewUserOverrideConfig()
	err := config.Merge(c, in, merged)
	if err != nil {
		return err
	}
	*c = *merged
	return nil
}

// NewDeepCopy will return a new deep copy of the existing struct.
func (c *AutomateConfig) NewDeepCopy() (*AutomateConfig, error) {
	copy := NewUserOverrideConfig()
	blank := NewUserOverrideConfig()
	err := config.Merge(c, blank, copy)
	if err != nil {
		return nil, err
	}

	return copy, nil
}

// Sum64 returns the 64-bit FNV hash of the TOML representation of the Config.
func (c *AutomateConfig) Sum64() (uint64, error) {
	t, err := c.MarshalToTOML()
	if err != nil {
		return 0, err
	}

	h := fnv.New64()
	_, err = h.Write(t)
	if err != nil {
		return 0, err
	}

	return h.Sum64(), nil
}

// ValidateWithGlobalAndDefaults tries merging setting configuration settings
// from the globals section, merges the result with the default
// AutomateConfig and then runs validation against the merged result.
func (c *AutomateConfig) ValidateWithGlobalAndDefaults() error {
	configToMerge, err := MergeWithDefaults(c)
	if err != nil {
		return err
	}
	configToMerge.SetGlobalConfig()
	return configToMerge.Validate()
}

// ValidateWithGlobalAndDefaultsAndCredentials does all of the same validation
// as ValidateWithGlobalAndDefaults and additionally runs ValidateCredentials().
// TODO XXX UGH: this is here because we are trying to remove AdminUser from
// deployment-service's config, but thus far we just have a workaround that
// hides the config from the user. We still need the config when we configure
// deployment for a deploy or upgrade, in which case we need to confirm the
// information is provided.
func (c *AutomateConfig) ValidateWithGlobalAndDefaultsAndCredentials() error {
	configToMerge, err := MergeWithDefaults(c)
	if err != nil {
		return err
	}
	configToMerge.SetGlobalConfig()
	err = configToMerge.Validate()
	if err != nil {
		return err
	}
	return configToMerge.ValidateCredentials()
}

// ValidateCredentials ensures that initial admin credentials are present in
// the configuration.
// TODO XXX UGH: this is here because we are trying to remove AdminUser from
// deployment-service's config, but thus far we just have a workaround that
// hides the config from the user. We still need the config when we configure
// deployment for a deploy or upgrade, in which case we need to confirm the
// information is provided.
func (c *AutomateConfig) ValidateCredentials() error {
	err := c.Deployment.ValidateCredentials()
	if err == nil {
		return nil
	}

	cfgErr, ok := err.(config.Error)
	if ok {
		if cfgErr.IsEmpty() {
			return nil
		}
	}

	return err
}

// AddCredentials sets the required admin name/username/password. It's used as
// a shim while we store these in the config but keep them out of init config
// and hide them from users.
func (c *AutomateConfig) AddCredentials(adminName, adminUsername, adminPassword string) error {
	return c.OverrideConfigValues(&AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					AdminUser: &ConfigRequest_V1_AdminUser{
						Username: w.String(adminUsername),
						Password: w.String(adminPassword),
						Name:     w.String(adminName),
					},
				},
			},
		},
	})
}

// MarshalToTOML marshals the AutomateConfig to a TOML representation and returns
// it as a byte slice.
func (c *AutomateConfig) MarshalToTOML() ([]byte, error) {
	t, err := toml.Marshal(c)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to convert AutomateConfig to TOML")
	}

	return t, nil
}

// RedactedCopy does a TOML round-trip, then returns a redacted version of the
// deep-dup'd AutomateConfig
func (c *AutomateConfig) RedactedCopy() (*AutomateConfig, error) {
	marshaled, err := c.MarshalToTOML()
	if err != nil {
		return nil, err
	}
	copy, err := NewUserOverrideConfigFromTOML(marshaled)
	if err != nil {
		return nil, err
	}
	copy.Redact()
	return copy, nil
}

// Redact removes sensitive values or stuff we just don't want users to see
func (c *AutomateConfig) Redact() {
	c.Deployment.V1.Svc.AdminUser = nil
}

// MarshalToTOMLFile marshals the AutomateConfig to a TOML representation and
// writes it to a file at the given path.
func (c *AutomateConfig) MarshalToTOMLFile(path string, perm os.FileMode) error {
	t, err := c.MarshalToTOML()
	if err != nil {
		return err
	}

	expandedPath, err := filepath.Abs(path)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("Invalid file path: %s", path))
	}

	err = ioutil.WriteFile(expandedPath, t, perm)
	if err != nil {
		return errors.Wrap(err, fmt.Sprintf("Failed to write AutomateConfig to %s", expandedPath))
	}

	return nil
}
