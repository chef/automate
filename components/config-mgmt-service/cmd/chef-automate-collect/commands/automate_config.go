package commands

import (
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptrace"
	"net/url"
	"os"
	"strings"

	fpath "path/filepath"

	"github.com/BurntSushi/toml"
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/httputils"
	cmd "github.com/chef/automate/lib/platform/command"
)

const (
	TestCreateEndpoint        = "/api/beta/cfgmgmt/rollouts/test_create"
	ConfigFileBasename        = ".automate_collector.toml"
	PrivateConfigFileBasename = ".automate_collector_private.toml"
	GitIgnoreContent          = `
# .automate_collector_private.toml contains Chef Automate credentials:
.automate_collector_private.toml
`
)

type AutomateCollectorConfig interface {
	IsAutomateCollectorConfig() bool
}

type Config struct {
	Automate []*AutomateConfig `toml:"automate"`
}

func (c *Config) IsAutomateCollectorConfig() bool {
	return true
}

func (c *Config) WithPrivate() *PrivateConfig {
	p := &PrivateConfig{}
	for _, a := range c.Automate {
		p.Automate = append(p.Automate, a.WithPrivate())
	}
	return p
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
	enc.Encode(c)

	fmt.Printf("in a git repo %+v\n", inAGitRepo())
	fmt.Printf("file is gitignored? %+v\n", fileIsGitignored(PrivateConfigFileBasename))

	if inAGitRepo() && !fileIsGitignored(PrivateConfigFileBasename) {
		gitignorePath := fpath.Join(configDir, ".gitignore")
		err := appendFile(gitignorePath, []byte(GitIgnoreContent), 0644)
		if err != nil {
			return errors.Wrapf(err, "unable to create/modify gitignore file %q", gitignorePath)
		}
		(&CLIIO{}).msg("Your .gitignore has been updated to protect your Chef Automate credentials\n")
	}

	privateFile, err := os.OpenFile(privateConfigFilename, os.O_RDWR|os.O_CREATE, 0600)
	if err != nil {
		return err
	}
	defer privateFile.Close()
	enc = toml.NewEncoder(privateFile)
	enc.Encode(c.WithPrivate())

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
	fmt.Printf("check ignore %q %+v\n", o, err)
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
		(&CLIIO{}).msg("checking for git dir in %q\n", dirToCheck)
		if isRepoRoot(dirToCheck) {
			return dirToCheck, nil
		}
	}
	// if nothing we tried is the repo root
	(&CLIIO{}).verbose("no .git dir found in %q and above, falling back to working directory for repo root", cwd)
	return cwd, nil
}

func isRepoRoot(path string) bool {
	_, err := os.Stat(fpath.Join(path, ".git"))
	return err == nil
}

func (c *Config) WriteUserConfigFiles() error {
	// TODO/FIXME windows places?
	return nil
}

func globalConfigDir() (string, error) {

	return "", nil
}

type PrivateConfig struct {
	Automate []*PrivateAutomateConfig `toml:"automate"`
}

func (p *PrivateConfig) IsAutomateCollectorConfig() bool {
	return true
}

type AutomateConfig struct {
	URL         string `toml:"url"`
	authToken   string
	InsecureTLS bool   `toml:"-"`
	TestURL     string `toml:"-"`
}

type PrivateAutomateConfig struct {
	AutomateConfig
	AuthToken   string `toml:"auth_token"`
	InsecureTLS bool   `toml:"insecure_tls"`
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

	testURL := cleanedURL
	testURL.Path = TestCreateEndpoint

	return &AutomateConfig{
		URL:       baseURL,
		authToken: token,
		TestURL:   testURL.String(),
	}, nil
}

func (a *AutomateConfig) WithPrivate() *PrivateAutomateConfig {
	return &PrivateAutomateConfig{
		AutomateConfig: *a,
		AuthToken:      a.authToken,
		InsecureTLS:    a.InsecureTLS,
	}
}

func (a *AutomateConfig) Test() error {
	cliIO := CLIIO{EnableVerbose: genConfigCommands.verbose}

	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{InsecureSkipVerify: genConfigCommands.insecureConnection}
	httpClient := &http.Client{Transport: tr}

	req, err := http.NewRequest("POST", a.TestURL, nil)
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
				cliIO.verbose("HTTP TRACE: %q resolved to %v (with error: %s)\n",
					req.URL.Host, addrStrs, d.Err.Error())
				return
			}
			cliIO.verbose("HTTP TRACE: %q resolved to %v\n", req.URL.Host, addrStrs)
		},
		GotConn: func(c httptrace.GotConnInfo) {
			cliIO.verbose("HTTP TRACE: connected to %q (reused: %t) (was idle: %t)\n",
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
		cliIO.verbose("HTTP RESPONSE HEADER %s: %s\n", k, strings.Join(v, ", "))
	}
	cliIO.verbose("HTTP RESPONSE BODY------------\n")
	cliIO.verbose(string(bodyBytes))
	cliIO.verbose("\nEND HTTP RESPONSE BODY--------\n")

	if response.StatusCode != 200 {
		fmt.Fprintf(os.Stderr, "ERROR: request to %q failed with status code %d\n", a.URL, response.StatusCode)
		os.Exit(1)
	}

	return nil
}
