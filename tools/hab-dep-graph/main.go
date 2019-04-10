// Query the dependencies of our habitat packages and build
// a complete graph of the dependency tree for to:
//
// go run ./tools/hab-dep-graph | dot -Tsvg -odeps.svg
//

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/io/fileutils"
)

type packageInfo struct {
	Deps []depInfo `json:"deps"`
}

type depInfo struct {
	// NOTE(ssd) 2019-03-07: Pkg-prefix here is to be able to
	// implement the VersionedArtifact interface below without
	// issue.
	PkgName    string `json:"name"`
	PkgOrigin  string `json:"origin"`
	PkgVersion string `json:"version"`
	PkgRelease string `json:"release"`
}

func (d depInfo) Name() string    { return d.PkgName }
func (d depInfo) Origin() string  { return d.PkgOrigin }
func (d depInfo) Version() string { return d.PkgVersion }
func (d depInfo) Release() string { return d.PkgRelease }

type dependencyMap map[string][]string

var opts = struct {
	Debug          bool
	IgnoreVersions bool
	Channel        string
}{}

func main() {
	cmd := &cobra.Command{
		Use:   "hab-dep-graph [PKG_IDENT...]",
		Short: "Generate dot file for Habitat package dependencies",
		Long: `Generate dot file for Habtiat package dependencies

The dot file can be passed into the dot command from GraphViz

    go run ./tools/hab-dep-graph | dot -Tsvg -odeps.svg

You can generate the graph for a particular package by passing it as an argument:

    go run ./tools/hab-dep-graph chef/compliance-service

Or combine this with other tools to create the dep graph for a particular release:

    pkg_list="$(curl https://packages.chef.io/manifests/dev/automate/latest.json 2>/dev/null | jq -r '.packages | .[]')"
    go run ./tools/hab-dep-graph $pkg_list
`,

		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			} else {
				logrus.SetLevel(logrus.InfoLevel)
			}
		},
		RunE: run,
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().StringVarP(
		&opts.Channel,
		"channel",
		"c",
		"unstable",
		"Channel to pull non-fully-qualified package dependency data from",
	)

	cmd.PersistentFlags().BoolVarP(
		&opts.IgnoreVersions,
		"ignore-versions",
		"",
		false,
		"Ignore dependency versions when building the graph",
	)

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

func run(_ *cobra.Command, pkgNames []string) error {
	if len(pkgNames) == 0 {
		var err error
		logrus.Info("Reading package names from components/")
		pkgNames, err = getPackagesFromDir("components")
		if err != nil {
			return errors.Wrap(err, "could not get package names from component dir")
		}
	}

	pkgs, err := habpkg.FromList(pkgNames)
	if err != nil {
		return errors.Wrap(err, "could not parse package names")
	}

	depMap := make(dependencyMap, 100)
	for _, p := range pkgs {
		err := addDeps(depMap, &p)
		if err != nil {
			return err
		}
	}

	fmt.Println("digraph \"a2_deps\" {")
	for k, vv := range depMap {
		for _, v := range vv {
			fmt.Printf("\"%s\" -> \"%s\"\n", k, v)
		}
	}
	fmt.Println("}")
	return nil
}

func getPackagesFromDir(dir string) ([]string, error) {
	componentDir, _ := filepath.Abs(dir)
	dentries, err := ioutil.ReadDir(componentDir)
	if err != nil {
		return nil, errors.Wrap(err, "readdir")
	}
	pkgs := make([]string, 0, 35)
	for _, entry := range dentries {
		if !entry.IsDir() {
			continue
		}
		planPath := filepath.Join(componentDir, entry.Name(), "habitat/plan.sh")
		exists, err := fileutils.PathExists(planPath)
		if err != nil {
			return nil, errors.Wrap(err, "could not lookup plan.sh")
		}

		if !exists {
			continue
		}

		name, err := getNameFromPlan(planPath)
		if err != nil {
			return nil, errors.Wrap(err, "could not retrieve name from plan.sh")
		}
		pkgs = append(pkgs, name)
	}

	return pkgs, nil
}

func getNameFromPlan(planPath string) (string, error) {
	script := fmt.Sprintf(`
source %s
echo "$pkg_origin/$pkg_name"
`, planPath)

	cmd := exec.Command("bash")
	cmd.Stdin = strings.NewReader(script)
	out, err := cmd.Output()
	if err != nil {
		return "", errors.Wrap(err, "executing bash script for pkg_name")
	}
	lines := bytes.Split(out, []byte("\n"))
	last := bytes.TrimSpace(lines[len(lines)-2])
	return string(last), nil
}

func addDeps(m dependencyMap, pkg habpkg.VersionedPackage) error {
	var ident string
	if opts.IgnoreVersions {
		ident = habpkg.ShortIdent(pkg)
	} else {
		ident = habpkg.Ident(pkg)
	}

	_, ok := m[ident]
	if ok {
		return nil
	}

	deps, err := fetchDeps(pkg)
	if err != nil {
		return err
	}

	depList := make([]string, len(deps))
	for i, d := range deps {
		if opts.IgnoreVersions {
			depList[i] = habpkg.ShortIdent(d)
		} else {
			depList[i] = habpkg.Ident(d)
		}
		err := addDeps(m, d)
		if err != nil {
			return err
		}
	}

	m[ident] = depList
	return nil
}

func fetchDeps(pkg habpkg.VersionedPackage) ([]depInfo, error) {
	ident := habpkg.Ident(pkg)
	fullyQualified := habpkg.IsFullyQualified(pkg)
	if fullyQualified {
		localDepFile, err := ioutil.ReadFile(fmt.Sprintf("/hab/pkgs/%s/%s/%s/%s/DEPS",
			pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release()))
		if err != nil {
			if !os.IsNotExist(err) {
				return nil, err
			} else {

			}
		} else {
			logrus.Infof("Using local install of %s for dependency information", ident)
			deps, err := habpkg.FromStrings(string(localDepFile))
			if err != nil {
				return nil, errors.Wrap(err, "could not parse local DEP file")
			}
			ret := make([]depInfo, len(deps))
			for i, d := range deps {
				ret[i] = depInfo{
					PkgOrigin:  d.Origin(),
					PkgName:    d.Name(),
					PkgVersion: d.Version(),
					PkgRelease: d.Release(),
				}
			}
			return ret, nil
		}
	}

	logrus.Infof("Fetching deps for %s from Habitat depot", ident)
	var pkgURL string
	if fullyQualified {
		pkgURL = fmt.Sprintf("https://bldr.habitat.sh/v1/depot/channels/%s/unstable/pkgs/%s/%s/%s",
			pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	} else {
		pkgURL = fmt.Sprintf("https://bldr.habitat.sh/v1/depot/channels/%s/%s/pkgs/%s/latest",
			pkg.Origin(), opts.Channel, pkg.Name())
	}

	response, err := http.Get(pkgURL)
	if err != nil {
		return nil, errors.Wrap(err, "could not fetch package info")
	}
	defer response.Body.Close() // nolint: errcheck

	if response.StatusCode == 404 {
		logrus.Warnf("Could not find %s in Habitat depot", habpkg.Ident(pkg))
		return nil, nil
	}

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, errors.Wrap(err, "could not read package info body")
	}

	info := &packageInfo{}
	err = json.Unmarshal(body, info)
	if err != nil {
		return nil, errors.Wrap(err, "could not marshal package info")
	}

	return info.Deps, nil
}
