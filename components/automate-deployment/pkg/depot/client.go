package depot

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"

	"github.com/pkg/errors"
)

var defaultURL = "https://bldr.habitat.sh"

// OriginKeyName is the key name, for example core-20180119235000
type OriginKeyName string

// pkgDep is a fully qualified hab package
type pkgDep struct {
	Origin  string `json:"origin"`
	Name    string `json:"name"`
	Version string `json:"version"`
	Release string `json:"release"`
}

// HartHeader is the header of the hart
type HartHeader struct {
	FormatVersion string
	KeyName       OriginKeyName
}

// Client can fetch things from the hab depot
type Client interface {
	// TDepsForPackage fetches all the dependencies for the given package
	// If the given package is A, and A -> B -> C, this function will return
	// B and C
	TDepsForPackage(habpkg.VersionedPackage) ([]habpkg.HabPkg, error)
	// DownloadPackage downloads the hartifact for the given package
	DownloadPackage(habpkg.VersionedPackage, io.Writer) (*HartHeader, error)
	// DownloadOriginKey downloads the public origin key
	DownloadOriginKey(OriginKeyName, io.Writer) error
}

type errPackageNotFound struct {
	path string
}

func (e *errPackageNotFound) Error() string {
	return fmt.Sprintf("Could not find %s", e.path)
}

func IsErrNotFound(e error) bool {
	_, ok := e.(*errPackageNotFound)
	return ok
}

type client struct {
	url string
}

// ClientOpt are functional client options
type ClientOpt func(c *client)

// NewClient returns a client that can talk to the depot
func NewClient(opts ...ClientOpt) Client {
	c := &client{
		url: defaultURL,
	}

	for _, o := range opts {
		o(c)
	}

	return c
}

func (c *client) TDepsForPackage(pkg habpkg.VersionedPackage) ([]habpkg.HabPkg, error) {
	resp, err := http.Get(fmt.Sprintf("%s/v1/depot/pkgs/%s/%s/%s/%s",
		c.url, pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release()))

	if err != nil {
		return nil, errors.Wrap(err, "Request to get TDeps for package failed")
	}

	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, errors.Errorf("Could not get TDeps for package. Got status %s", resp.Status)
	}

	respObj := struct {
		TDeps []pkgDep `json:"tdeps"`
	}{}

	err = json.NewDecoder(resp.Body).Decode(&respObj)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to decode response")
	}

	tdeps := make([]habpkg.HabPkg, len(respObj.TDeps))

	for i, dep := range respObj.TDeps {
		tdeps[i] = habpkg.NewFQ(dep.Origin, dep.Name, dep.Version, dep.Release)
	}

	return tdeps, nil
}

func (c *client) DownloadPackage(pkg habpkg.VersionedPackage, writer io.Writer) (*HartHeader, error) {
	resp, err := http.Get(fmt.Sprintf("%s/v1/depot/pkgs/%s/%s/%s/%s/download",
		c.url, pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release()))

	if err != nil {
		return nil, errors.Wrap(err, "Request to download package failed")
	}

	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, errors.Errorf("Could not get download package. Got status %s", resp.Status)
	}

	return copyPackage(resp.Body, writer)
}

func copyPackage(src io.Reader, dst io.Writer) (*HartHeader, error) {
	tee := io.TeeReader(src, dst)

	bufReader := bufio.NewReader(tee)

	hartFormat, err := bufReader.ReadString('\n')
	if err != nil {
		return nil, errors.Wrap(err, "Could not read hart format")
	}

	hartFormat = strings.TrimSpace(hartFormat)
	if strings.TrimSpace(hartFormat) != "HART-1" {
		return nil, errors.Wrapf(err, "Unknown hart format %s", hartFormat)
	}

	keyName, err := bufReader.ReadString('\n')
	if err != nil {
		return nil, errors.Wrap(err, "Could not read key name")
	}

	keyName = strings.TrimSpace(keyName)

	_, err = io.Copy(ioutil.Discard, bufReader)
	if err != nil {
		return nil, errors.Wrap(err, "Could not download package")
	}

	return &HartHeader{
		FormatVersion: hartFormat,
		KeyName:       OriginKeyName(keyName),
	}, nil
}

func (c *client) DownloadOriginKey(keyName OriginKeyName, writer io.Writer) error {
	parts := strings.Split(string(keyName), "-")
	origin := strings.Join(parts[0:len(parts)-1], "-")
	release := parts[len(parts)-1]

	resp, err := http.Get(fmt.Sprintf("%s/v1/depot/origins/%s/keys/%s",
		c.url, origin, release))

	if err != nil {
		return errors.Wrap(err, "Failed to download origin key")
	}

	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return errors.Errorf("Failed to download origin key. Got status %s", resp.Status)
	}

	_, err = io.Copy(writer, resp.Body)
	if err != nil {
		return errors.Wrap(err, "Failed to download origin key")
	}

	return nil
}
