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

	"github.com/BurntSushi/toml"

	"github.com/chef/automate/lib/httputils"
	cmd "github.com/chef/automate/lib/platform/command"
)

const (
	TestCreateEndpoint    = "/api/beta/cfgmgmt/rollouts/test_create"
	ConfigFilename        = ".automate_collector.toml"
	PrivateConfigFilename = ".automate_collector_private.toml"
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

	publicFile, err := os.Create(ConfigFilename)
	if err != nil {
		return err
	}

	defer publicFile.Close()
	enc := toml.NewEncoder(publicFile)
	enc.Encode(c)

	// Check if we are in a git directory
	_, err = cmd.Output("git", cmd.Args("rev-parse", "--git-dir"))
	if err == nil {
		// if we are in a git directory, check if the private config filename is in
		// gitignore (with -q, check-ignore will exit 1 if the file is NOT ignored):
		_, err = cmd.Output("git", cmd.Args("check-ignore", "-q", PrivateConfigFilename))
		if err != nil {
			// TODO/FIXME: acceptable to just add it? it's less dangerous.
			(&CLIIO{}).msg("WARNING: add the following line to your .gitignore to avoid leaking credentials!\n%s\n", PrivateConfigFilename)
		}
	}

	privateFile, err := os.OpenFile(PrivateConfigFilename, os.O_RDWR|os.O_CREATE, 0600)
	if err != nil {
		return err
	}
	defer privateFile.Close()
	enc = toml.NewEncoder(privateFile)
	enc.Encode(c.WithPrivate())

	return nil
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
