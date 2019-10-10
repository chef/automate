package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
)

const (
	habPkgPath  = "HABITAT_PACKAGES"
	bldrCfgPath = ".bldr.toml"
)

type PackageSpec struct {
	name string
	path string
}

type GoDepInfo struct {
	deps []string
}

var opts = struct {
	Debug     bool
	PrintOnly bool
}{}

func main() {
	cmd := &cobra.Command{
		Use:           "bldr-config-gen",
		Short:         "Config file generator for .bldr.toml",
		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			} else {
				logrus.SetLevel(logrus.WarnLevel)
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

	cmd.PersistentFlags().BoolVarP(
		&opts.PrintOnly,
		"print-only",
		"",
		false,
		"Print generated config to STDOUT, don't update .expeditor.yml",
	)

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

func run(*cobra.Command, []string) error {
	expectFile(habPkgPath)
	if !opts.PrintOnly {
		expectFile(bldrCfgPath)
	}

	habPackages, err := parseHabPackageFile(habPkgPath)
	if err != nil {
		return err
	}

	var outBuf io.Writer
	if opts.PrintOnly {
		outBuf = os.Stdout
	} else {
		outBuf = &bytes.Buffer{}
	}

	err = generateHabPackageConfig(outBuf, habPackages)
	if err != nil {
		return err
	}

	if opts.PrintOnly {
		return nil
	}

	return writeBldrConfig(outBuf.(*bytes.Buffer).Bytes(), bldrCfgPath)
}

// parseHabPackageFile parses HABITAT_PACKAGES which is a custom text
// file format to enumerate the packages in our repository. The format
// is
//
// # comments
// package_name package_dir
//
// since this isn't reallly a full parser/tokenizer we don't allow
// comments on the same line as package names.
func parseHabPackageFile(path string) ([]PackageSpec, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, errors.Wrap(err, "open")
	}
	defer f.Close() // nolint: errcheck

	ret := make([]PackageSpec, 0)
	scanner := bufio.NewScanner(f)
	lineno := 1
	for scanner.Scan() {
		line := scanner.Text()
		line = strings.TrimSpace(line)
		if line[0] != '#' {
			fields := strings.Fields(line)
			if len(fields) != 2 {
				return nil, errors.Errorf(
					"wrong number of fields (have %d want 2) at line %d: %s",
					len(fields),
					lineno,
					line,
				)
			}
			ret = append(ret, PackageSpec{
				name: fields[0],
				path: strings.TrimRight(fields[1], "/"),
			})

		}
		lineno++
	}
	if err := scanner.Err(); err != nil {
		return nil, errors.Wrap(err, "error scanning habitat package file")
	}

	return ret, nil
}

func generateHabPackageConfig(outBuf io.Writer, habPackages []PackageSpec) error {
	for i, p := range habPackages {
		depInfo, err := getGoDepInfo(p.path)
		if err != nil {
			return err
		}
		if i != 0 {
			fmt.Fprintf(outBuf, "\n")
		}
		fmt.Fprintf(outBuf, "[%s]\n", p.name)
		fmt.Fprintf(outBuf, "plan_path = \"%s\"\n", p.path)
		fmt.Fprintf(outBuf, "paths = [\n")
		fmt.Fprintf(outBuf, "  \"%s/*\"", p.path)
		for _, dep := range depInfo.deps {
			fmt.Fprintf(outBuf, ",\n  \"%s/*\"", dep)
		}
		fmt.Fprintf(outBuf, "\n]\n")
	}
	return nil
}

// getGoDepInfo gets all of the non-standard lib deps and reduces their import
// paths according to desired reduce rules. This allows us to tailor exactly
// the depths we want to go for a desired package before rebuildling.
func getGoDepInfo(path string) (GoDepInfo, error) {
	path = strings.TrimLeft(path, "/")
	goPkgPath := fmt.Sprintf("./%s/...", path)
	deps, err := command.Output("go",
		command.Args("list", "-deps", "-f", "{{if not .Standard}}{{.ImportPath}}{{end}}", goPkgPath),
		command.Envvar("GOOS", "linux"),
	)
	if err != nil {
		return GoDepInfo{}, errors.Wrapf(err, "could not query go dependencies: %s", command.StderrFromError(err))
	}

	dp := &depImportReducer{}
	dp.RegisterReduceRules([]depImportReduceRule{
		{reduce: "github.com/chef/automate/api", to: 6},
		// Many services depend on automate-gateway right now because of some
		// dependency relations around the debug API and auth. Including deeper
		// matches helps reduce rebuilds for non-auth changes in the gateway.
		// If there are cross-component dependencies, we don't look at subdirs
		// so that people can move code around inside their own projects without
		// having to constantly update this file.
		{reduce: "github.com/chef/automate/components/automate-gateway", to: 7},
		{reduce: "github.com/chef/automate/lib/platform", to: 6},
		{reduce: "github.com/chef/automate/lib/", to: 5},
		{reduce: "github.com/chef/automate/components", to: 5},
		{reduce: "github.com/chef/automate", to: 5},
		{reduce: "google.golang.org", to: 2},
		{reduce: "", to: 3}, // everything else gets trimmed to 3
	}...)

	return dp.Reduce(path, deps)
}

type depImportReducer struct {
	rules []depImportReduceRule
}

func (p *depImportReducer) RegisterReduceRules(f ...depImportReduceRule) {
	rules := append(p.rules, f...)

	sort.Slice(rules, func(i, j int) bool {
		// If the reduce pattern isn't set we'll move it to the bottom
		if rules[i].reduce == "" {
			return false
		}

		// Longer reduce strings are matched first
		return len(strings.Split(rules[i].reduce, "/")) > len(strings.Split(rules[j].reduce, "/"))
	})

	p.rules = rules
}

// Reduce the imports paths according to the reduce rules, sort them and uniq
// them.
func (p *depImportReducer) Reduce(path, deps string) (GoDepInfo, error) {
	info := GoDepInfo{}

	scanner := bufio.NewScanner(strings.NewReader(deps))
	for scanner.Scan() {
		line := scanner.Text()
		line = strings.TrimLeft(line, "/")

		if strings.Contains(line, path) {
			continue
		}

		parts := strings.Split(line, "/")
		dep := ""

		for _, f := range p.rules {
			if strings.HasPrefix(line, f.reduce) {
				if f.to > 0 {
					dep = strings.Join(parts[0:min(f.to, len(parts))], "/")
				} else {
					dep = line
				}

				if strings.HasPrefix(dep, "github.com/chef/automate/") {
					dep = strings.TrimPrefix(dep, "github.com/chef/automate/")
				} else {
					dep = fmt.Sprintf("vendor/%s", dep)
				}

				break
			}
		}

		if dep == "" {
			dep = line
		}

		info.deps = append(info.deps, dep)
	}
	if err := scanner.Err(); err != nil {
		return info, errors.Wrap(err, "error scanning go list output")
	}

	// We create a sorted uniq list of dependencies
	sort.Strings(info.deps)

	last := ""
	uniqDeps := make([]string, 0, len(info.deps))
	for _, l := range info.deps {
		if l != last {
			uniqDeps = append(uniqDeps, l)
		}
		last = l
	}
	info.deps = uniqDeps

	return info, nil
}

// depImportReduceRule is a filter for our depImportReducer.
type depImportReduceRule struct {
	reduce string // the import path to match
	to     int    // the depth we want to reduce the path to
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

const (
	startGenMarker = "# GENERATED_HAB_PACKAGE_CONFIG_START"
	endGenMarker   = "# GENERATED_HAB_PACKAGE_CONFIG_END"
	genComment     = `# The following habitat package configuration is auto-generated
# To update this content run:
#     go run ./tools/bldr-config-gen`
)

func writeBldrConfig(data []byte, path string) error {
	f, err := os.Open(path)
	if err != nil {
		return errors.Wrap(err, "open")
	}
	defer f.Close() // nolint: errcheck

	scanner := bufio.NewScanner(f)

	return fileutils.AtomicWriter(path, func(w io.Writer) error {
		foundGenStart := false
		inGenContent := false
		lineno := 1
		for scanner.Scan() {
			line := scanner.Text()
			if !inGenContent {
				_, err := fmt.Fprintln(w, line)
				if err != nil {
					return errors.Wrap(err, "write")
				}
			}

			if line == startGenMarker {
				if inGenContent {
					return errors.Errorf(
						"malformed expeditor configuration, found second generated content start marker at line %d: %s",
						lineno,
						line)
				}
				inGenContent = true
				foundGenStart = true
				_, err := fmt.Fprintln(w, genComment)
				if err != nil {
					return errors.Wrap(err, "write")
				}

				_, err = w.Write(data)
				if err != nil {
					return errors.Wrap(err, "write")
				}
			} else if line == endGenMarker {
				_, err := fmt.Fprintln(w, line)
				if err != nil {
					return errors.Wrap(err, "write")
				}
				inGenContent = false
			}
			lineno++
		}
		if err := scanner.Err(); err != nil {
			return errors.Wrap(err, "error scanning existing expeditor config")
		}

		if !foundGenStart {
			return errors.New("could not find start of generated content in expeditor configuration")
		}

		if inGenContent {
			return errors.New("could not find end of generated content in expeditor configuration")
		}

		return nil
	})
}

func expectFile(path string) {
	exists, _ := fileutils.PathExists(path)
	if !exists {
		logrus.Fatalf("expected file %s to exist, are you running from the project root directory?", path)
	}
}
