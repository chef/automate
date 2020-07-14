package commands

import (
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptrace"
	"net/url"
	"os"
	"runtime"
	"strings"

	fpath "path/filepath"

	"github.com/BurntSushi/toml"
	"github.com/mitchellh/go-homedir"
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/httputils"
	cmd "github.com/chef/automate/lib/platform/command"
)

const (
	TestCreateURLPath     = "/api/beta/cfgmgmt/rollouts/test_create"
	CreateRolloutsURLPath = "/api/beta/cfgmgmt/rollouts/create"

	ConfigFileBasename        = ".automate_collector.toml"
	PrivateConfigFileBasename = ".automate_collector_private.toml"
	NonHiddenConfigBasename   = "automate_collector.toml"

	GitIgnoreContent = `
# .automate_collector_private.toml contains Chef Automate credentials:
.automate_collector_private.toml
`
)

type ConfigLoader struct {
	RepoConfigPath        string
	RepoPrivateConfigPath string
	UserConfigPath        string
	SystemConfigPath      string
	LoadedConfig          *Config
}

type AutomateCollectorConfig interface {
	IsAutomateCollectorConfig() bool
}

type Config struct {
	Automate *AutomateConfig `toml:"automate"`
}

type PrivateConfig struct {
	Automate *PrivateAutomateConfig `toml:"automate"`
}

func (p *PrivateConfig) IsAutomateCollectorConfig() bool {
	return true
}

type AutomateConfig struct {
	URL         string `toml:"url"`
	authToken   string
	InsecureTLS bool `toml:"-"`
}

type PrivateAutomateConfig struct {
	*AutomateConfig
	AuthToken   string `toml:"auth_token"`
	InsecureTLS bool   `toml:"insecure_tls"`
}

func NewConfigLoader() *ConfigLoader {
	c := &ConfigLoader{}
	c.findRepoConfig()
	c.findUserConfig()
	c.findSystemConfig()

	return c
}

func (l *ConfigLoader) ViableConfigPaths() []string {
	paths := []string{}
	// These are in a precedence order from low to high. Later ones should
	// override previous when loading.
	possiblePaths := []string{
		l.SystemConfigPath,
		l.UserConfigPath,
		l.RepoConfigPath,
		l.RepoPrivateConfigPath,
	}

	for _, p := range possiblePaths {
		if p != "" {
			paths = append(paths, p)
		}
	}
	return paths
}

func (l *ConfigLoader) Load() error {
	l.LoadedConfig = &Config{Automate: &AutomateConfig{}}

	for _, p := range l.ViableConfigPaths() {
		configFromFile := &PrivateConfig{}
		fileContent, err := ioutil.ReadFile(p)
		if err != nil {
			return errors.Wrapf(err, "failed to read config file %q", p)
		}
		err = toml.Unmarshal(fileContent, configFromFile)
		if err != nil {
			return errors.Wrapf(err, "cannot decode TOML content in %q", p)
		}
		cliIO.verbose("applying configuration from %q", p)
		l.LoadedConfig.ApplyValuesFrom(configFromFile.ToConfig())
	}

	l.LoadedConfig.ApplyValuesFromEnv()

	return nil
}

func (l *ConfigLoader) findRepoConfig() {
	if value, envSet := os.LookupEnv(NoRepoConfigEnvVar); envSet && value != "false" {
		cliIO.verbose("found environment %s=%q repo config is disabled", NoRepoConfigEnvVar, value)
		return
	}

	if dirFromEnv, envSet := os.LookupEnv(RepoConfigDirPathEnvVar); envSet {
		cliIO.verbose("found environment %s=%q, looking for repo configuration there", RepoConfigDirPathEnvVar, dirFromEnv)
		candidateFilename := fpath.Join(dirFromEnv, ConfigFileBasename)
		_, err := os.Stat(candidateFilename)
		if err != nil {
			cliIO.verbose("candidate config file %q could not be found/accessed, no repo config will be set: %s", candidateFilename, err.Error())
			return
		}
		cliIO.verbose("found repo config file %q", candidateFilename)
		l.RepoConfigPath = candidateFilename

		candidatePrivateConfigFilename := fpath.Join(dirFromEnv, PrivateConfigFileBasename)
		_, err = os.Stat(candidatePrivateConfigFilename)
		if err != nil {
			cliIO.verbose("candidate private config file %q could not be found/accessed: %s", candidatePrivateConfigFilename, err.Error())
			return
		}

		cliIO.verbose("found repo private config file %q", candidatePrivateConfigFilename)
		l.RepoPrivateConfigPath = candidatePrivateConfigFilename
		return
	}

	cwd, err := os.Getwd()
	if err != nil {
		cliIO.verbose("failed to get cwd, cannot find repo config: %s", err.Error())
		return
	}
	var configDir string
	for configDir = cwd; configDir != "/"; configDir = fpath.Dir(configDir) {
		candidateFilename := fpath.Join(configDir, ConfigFileBasename)
		_, err := os.Stat(candidateFilename)
		if err != nil {
			cliIO.verbose("candidate repo config file %q not found: %s", candidateFilename, err.Error())
			continue
		}
		{
			cliIO.verbose("found repo config file %q", candidateFilename)
			l.RepoConfigPath = candidateFilename
			break
		}
	}

	if l.RepoConfigPath == "" {
		return
	}
	candidatePrivateConfigFilename := fpath.Join(configDir, PrivateConfigFileBasename)
	_, err = os.Stat(candidatePrivateConfigFilename)
	if err != nil {
		cliIO.verbose("candidate private config file %q not found: %s", candidatePrivateConfigFilename, err.Error())
		return
	}
	cliIO.verbose("found private config file %q", candidatePrivateConfigFilename)
	l.RepoPrivateConfigPath = candidatePrivateConfigFilename
	return
}

func (l *ConfigLoader) findUserConfig() {
	if value, envSet := os.LookupEnv(NoUserConfigEnvVar); envSet && value != "false" {
		cliIO.verbose("found environment %s=%q repo config is disabled", NoUserConfigEnvVar, value)
		return
	}

	if dirFromEnv, envSet := os.LookupEnv(UserConfigDirPathEnvVar); envSet {
		cliIO.verbose("found environment %s=%q, looking for user configuration there", UserConfigDirPathEnvVar, dirFromEnv)
		candidateFilename := fpath.Join(dirFromEnv, NonHiddenConfigBasename)
		_, err := os.Stat(candidateFilename)
		if err != nil {
			cliIO.verbose("candidate config file %q could not be found/accessed, no user config will be set: %s", candidateFilename, err.Error())
			return
		}
		cliIO.verbose("found user config file %q", candidateFilename)
		l.UserConfigPath = candidateFilename
		return
	}

	configDir, err := homedir.Expand("~/.chef")
	if err != nil {
		cliIO.verbose("cannot locate home directory, cannot find user config: %s", err.Error())
	}
	userConfigFilename := fpath.Join(configDir, NonHiddenConfigBasename)
	_, err = os.Stat(userConfigFilename)
	if err != nil {
		cliIO.verbose("candidate user config filename %q not found: %s", userConfigFilename, err.Error())
		return
	}
	cliIO.verbose("found user config file %q", userConfigFilename)
	l.UserConfigPath = userConfigFilename
	return
}

func (l *ConfigLoader) findSystemConfig() {
	if value, envSet := os.LookupEnv(NoSystemConfigEnvVar); envSet && value != "false" {
		cliIO.verbose("found environment %s=%q repo config is disabled", NoSystemConfigEnvVar, value)
		return
	}

	if dirFromEnv, envSet := os.LookupEnv(SystemConfigDirPathEnvVar); envSet {
		cliIO.verbose("found environment %s=%q, looking for user configuration there", SystemConfigDirPathEnvVar, dirFromEnv)
		candidateFilename := fpath.Join(dirFromEnv, NonHiddenConfigBasename)
		_, err := os.Stat(candidateFilename)
		if err != nil {
			cliIO.verbose("candidate config file %q could not be found/accessed, no system config will be set: %s", candidateFilename, err.Error())
			return
		}
		cliIO.verbose("found system config file %q", candidateFilename)
		l.SystemConfigPath = candidateFilename
		return

	}
	// assume *nix and change it if we detect windows (next)
	systemConfigDir := "/etc/chef"

	// In Chef::Config, the location of the system config is /etc/chef for *nix
	// (including Mac), and on windows uses the following logic:
	// * drive letter is determined from where the Chef package is installed,
	//   defaulting to `C:`
	// * directory name is `chef`
	// Here we skip the drive letter shenanigans for now and assume C:/chef. It
	// can be overridden with environment if needed.
	if runtime.GOOS == "windows" {
		systemConfigDir = "C:/chef"
	}

	candidateFilename := fpath.Join(systemConfigDir, NonHiddenConfigBasename)
	_, err := os.Stat(candidateFilename)
	if err != nil {
		cliIO.verbose("candidate config file %q could not be found/accessed, no system config will be set: %s", candidateFilename, err.Error())
		return
	}
	cliIO.verbose("found system config file %q", candidateFilename)
	l.SystemConfigPath = candidateFilename
	return
}

func (p *PrivateConfig) ToConfig() *Config {
	return &Config{Automate: p.Automate.ToConfig()}
}

func (c *Config) IsAutomateCollectorConfig() bool {
	return true
}

func (c *Config) ApplyValuesFromEnv() {
	c.Automate.ApplyValuesFromEnv()
}

func (c *Config) Redacted() *PrivateConfig {
	return &PrivateConfig{Automate: c.Automate.Redacted()}
}

func (c *Config) WithPrivate() *PrivateConfig {
	return &PrivateConfig{Automate: c.Automate.WithPrivate()}
}

func (c *Config) ApplyValuesFrom(other *Config) {
	c.Automate.ApplyValuesFrom(other.Automate)
}

func (c *Config) WriteRepoConfigFiles() error {

	configDir, err := repoRoot()
	if err != nil {
		return err
	}

	configFilename := fpath.Join(configDir, ConfigFileBasename)
	privateConfigFilename := fpath.Join(configDir, PrivateConfigFileBasename)

	publicFile, err := os.Create(configFilename)
	if err != nil {
		return err
	}

	defer publicFile.Close()
	enc := toml.NewEncoder(publicFile)
	err = enc.Encode(c)
	if err != nil {
		return err
	}

	if inAGitRepo() && !fileIsGitignored(PrivateConfigFileBasename) {
		gitignorePath := fpath.Join(configDir, ".gitignore")
		err := appendFile(gitignorePath, []byte(GitIgnoreContent), 0644)
		if err != nil {
			return errors.Wrapf(err, "unable to create/modify gitignore file %q", gitignorePath)
		}
		cliIO.msg("Your .gitignore has been updated to protect your Chef Automate credentials")
	}

	privateFile, err := os.OpenFile(privateConfigFilename, os.O_RDWR|os.O_CREATE, 0600)
	if err != nil {
		return err
	}
	defer privateFile.Close()
	enc = toml.NewEncoder(privateFile)
	err = enc.Encode(c.WithPrivate())
	if err != nil {
		return err
	}

	return nil
}

func inAGitRepo() bool {
	_, err := cmd.Output("git", cmd.Args("rev-parse", "--git-dir"))
	return err == nil
}

func fileIsGitignored(filename string) bool {
	// if we are in a git directory, check if the private config filename is in
	// gitignore (with -q, check-ignore will exit 1 if the file is NOT ignored):
	o, err := cmd.Output("git", cmd.Args("check-ignore", "-q", filename))
	cliIO.verbose("check gitignore for file %q returned %q %+v", filename, o, err)
	return err == nil
}

func appendFile(filename string, data []byte, perm os.FileMode) error {
	fd, err := os.OpenFile(filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, perm)
	if err != nil {
		return err
	}
	defer fd.Close()
	_, err = fd.Write(data)
	if err != nil {
		return err
	}
	return nil
}

func repoRoot() (string, error) {
	// go's implementation of Getwd honors the PWD env var if set, which handles
	// most cases where the user is navigating into dirs via symlinks. So we
	// should not need any additional heuristics to get the expected dirname
	// https://golang.org/src/os/getwd.go?s=620:656#L16
	cwd, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for dirToCheck := cwd; dirToCheck != "/"; dirToCheck = fpath.Dir(dirToCheck) {
		cliIO.msg("checking for git dir in %q", dirToCheck)
		if isRepoRoot(dirToCheck) {
			return dirToCheck, nil
		}
	}
	// if nothing we tried is the repo root
	cliIO.verbose("no .git dir found in %q and above, falling back to working directory for repo root", cwd)
	return cwd, nil
}

func isRepoRoot(path string) bool {
	_, err := os.Stat(fpath.Join(path, ".git"))
	return err == nil
}

func (c *Config) WriteUserConfigFiles() error {
	configDir, err := homedir.Expand("~/.chef")
	if err != nil {
		return err
	}

	_, err = os.Stat(configDir)
	if err != nil && !os.IsNotExist(err) {
		return err
	}
	if err != nil && os.IsNotExist(err) {
		err := os.Mkdir(configDir, 0700)
		if err != nil {
			return err
		}
	}

	userConfigFilename := fpath.Join(configDir, NonHiddenConfigBasename)

	configFile, err := os.OpenFile(userConfigFilename, os.O_RDWR|os.O_CREATE, 0600)
	if err != nil {
		return err
	}
	defer configFile.Close()
	enc := toml.NewEncoder(configFile)
	err = enc.Encode(c.WithPrivate())
	if err != nil {
		return err
	}

	return nil
}

func newAutomateConfig(givenURL, token string) (*AutomateConfig, error) {
	cleanedURL, err := url.Parse(givenURL)
	if err != nil {
		return nil, err
	}
	if cleanedURL.Scheme != "https" {
		return nil, fmt.Errorf("Automate URL %q is invalid; must use \"https\" protocol", givenURL)
	}
	cleanedURL.Path = ""
	baseURL := cleanedURL.String()

	return &AutomateConfig{
		URL:       baseURL,
		authToken: token,
	}, nil
}

func (a *AutomateConfig) TestURL() (*url.URL, error) {
	u, err := url.Parse(a.URL)
	if err != nil {
		return nil, err
	}
	u.Path = ""

	u.Path = TestCreateURLPath
	return u, nil
}

func (a *AutomateConfig) CreateRolloutURL() (*url.URL, error) {
	u, err := url.Parse(a.URL)
	if err != nil {
		return nil, err
	}
	u.Path = ""

	u.Path = CreateRolloutsURLPath
	return u, nil
}

func (a *AutomateConfig) WithPrivate() *PrivateAutomateConfig {
	return &PrivateAutomateConfig{
		AutomateConfig: a,
		AuthToken:      a.authToken,
		InsecureTLS:    a.InsecureTLS,
	}
}

func (a *AutomateConfig) Redacted() *PrivateAutomateConfig {
	var authTokenReplacement string
	if a.authToken != "" {
		authTokenReplacement = "[REDACTED]"
	}

	return &PrivateAutomateConfig{
		AutomateConfig: a,
		AuthToken:      authTokenReplacement,
		InsecureTLS:    a.InsecureTLS,
	}
}

func (a *AutomateConfig) ApplyValuesFromEnv() {
	if url, envVarSet := os.LookupEnv(AutomateURLEnvVar); envVarSet {
		cliIO.verbose("found environment %s=%q setting Automate URL", AutomateURLEnvVar, url)
		a.URL = url
	}
	if token, envVarSet := os.LookupEnv(AutomateTokenEnvVar); envVarSet {
		cliIO.verbose("found environment %s=%q setting Automate Token", AutomateURLEnvVar, token)
		a.authToken = token
	}
	if val, envVarSet := os.LookupEnv(AutomateInsecureTLSEnvVar); envVarSet {
		if val == "false" {
			a.InsecureTLS = false
		} else {
			a.InsecureTLS = true
		}
	}
}

func (a *AutomateConfig) ApplyValuesFrom(other *AutomateConfig) {
	cliIO.verbose("applying config %+v to existing %+v", *other, *a)
	if other.URL != "" {
		a.URL = other.URL
	}
	if other.authToken != "" {
		a.authToken = other.authToken
	}
	if other.InsecureTLS {
		a.InsecureTLS = true
	}
}

func (a *AutomateConfig) Test() error {
	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{InsecureSkipVerify: a.InsecureTLS}
	httpClient := &http.Client{Transport: tr}

	testURL, err := a.TestURL()
	if err != nil {
		return err
	}

	req, err := http.NewRequest("POST", testURL.String(), nil)
	if err != nil {
		return err
	}

	req.Header["Api-Token"] = []string{a.authToken}

	trace := &httptrace.ClientTrace{
		DNSDone: func(d httptrace.DNSDoneInfo) {
			addrStrs := make([]string, len(d.Addrs))
			for i, addr := range d.Addrs {
				addrStrs[i] = addr.String()
			}

			if d.Err != nil {
				// It wasn't clear to me in the docs
				// whether d.Err != nil is always
				// fatal, so we still log the
				// addresses here.
				cliIO.verbose("HTTP TRACE: %q resolved to %v (with error: %s)",
					req.URL.Host, addrStrs, d.Err.Error())
				return
			}
			cliIO.verbose("HTTP TRACE: %q resolved to %v", req.URL.Host, addrStrs)
		},
		GotConn: func(c httptrace.GotConnInfo) {
			cliIO.verbose("HTTP TRACE: connected to %q (reused: %t) (was idle: %t)",
				c.Conn.RemoteAddr(), c.Reused, c.WasIdle)
		},
	}

	req = req.WithContext(httptrace.WithClientTrace(req.Context(), trace))

	response, err := httpClient.Do(req)
	if err != nil {
		return err
	}
	defer func() {
		_ = response.Body.Close()
	}()

	bodyBytes, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return err
	}

	for k, v := range response.Header {
		cliIO.verbose("HTTP RESPONSE HEADER %s: %s", k, strings.Join(v, ", "))
	}
	cliIO.verbose("HTTP RESPONSE BODY------------")
	cliIO.verbose(string(bodyBytes))
	cliIO.verbose("\nEND HTTP RESPONSE BODY--------")

	if response.StatusCode != 200 {
		cliIO.msg("ERROR: request to %q failed with status code %d", a.URL, response.StatusCode)
		os.Exit(1)
	}

	return nil
}

func (p *PrivateAutomateConfig) ToConfig() *AutomateConfig {
	c := p.AutomateConfig
	c.InsecureTLS = p.InsecureTLS
	c.authToken = p.AuthToken
	return c
}
