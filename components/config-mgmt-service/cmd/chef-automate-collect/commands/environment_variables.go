package commands

const (
	// Set the Automate URL to use. Overrides all configuration.
	AutomateURLEnvVar = "CHEF_AC_AUTOMATE_URL"
	// Set the Automate Auth Token to use. Overrides all configuration.
	AutomateTokenEnvVar = "CHEF_AC_AUTOMATE_TOKEN"
	// Whether to disable verification of Automate's TLS (SSL) certificates. If
	// set to "false", certificates will be verified. If set to any other value,
	// certificate verification will be disabled.
	AutomateInsecureTLSEnvVar = "CHEF_AC_AUTOMATE_INSECURE_TLS"

	// Directory where the chef-automate-collector should look for per-repo
	// configuration. The config loader will look for files named
	// `.automate_collector.toml`  and `.automate_collector_private.toml` in this
	// directory.
	RepoConfigDirPathEnvVar = "CHEF_AC_REPO_CONFIG_DIR"
	// Directory where the chef-automate-collector should look for per-user
	// configuration. The config loader will look for a file named
	// `automate_collector.toml` inside this directory.
	UserConfigDirPathEnvVar = "CHEF_AC_USER_CONFIG_DIR"
	// Directory where the chef-automate-collector should look for systemwide
	// configuration. The config loader will look for a file named
	// `automate_collector.toml` inside this directory.
	SystemConfigDirPathEnvVar = "CHEF_AC_SYSTEM_CONFIG_DIR"

	// Whether to disable loading per-repo configuration. If set to "false,"
	// per-repo config will be loaded. If set to any other value, per-repo config
	// will not be loaded.
	NoRepoConfigEnvVar = "CHEF_AC_NO_REPO_CONFIG"
	// Whether to disable loading per-user configuration. If set to "false,"
	// per-repo config will be loaded. If set to any other value, per-user config
	// will not be loaded.
	NoUserConfigEnvVar = "CHEF_AC_NO_USER_CONFIG"
	// Whether to disable loading systemwide configuration. If set to "false,"
	// per-repo config will be loaded. If set to any other value, systemwide
	// config will not be loaded.
	NoSystemConfigEnvVar = "CHEF_AC_NO_SYSTEM_CONFIG"

	// Whether to disable reporting new rollouts to Chef Automate. If set to
	// "false," reports will be sent. If set to any other value, reports will not
	// be sent.
	DisableReportNewRolloutEnvVar = "CHEF_AC_DISABLE_COLLECTOR"

	// Name of the git remote to use when detecting the git URL. The default
	// remote name to check is "origin"
	GitRemoteNameEnvVar = "CHEF_AC_GIT_REMOTE_NAME"
)
