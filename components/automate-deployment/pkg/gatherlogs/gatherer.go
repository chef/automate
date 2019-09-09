package gatherlogs

import (
	"fmt"
	"io"
	"os"
	"path"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/command"
)

// DefaultLogLines in the default number of log lines we should ingest
// from journald.
const DefaultLogLines = 500000

// Gatherer defines values used in the log/data collection process
type Gatherer struct {
	archiveRoot    string // top level of the resulting archive file
	targetDir      string // directory we collect data into
	bundleFileName string // the filename for our compressed archive file
	bundleFilePath string // full path to the archive file
	fqdn           string // fqdn of the host gather-logs is running on
	operations     []executer
}

// NewGatherer returns an initialized Gatherer
func NewGatherer(stagingDir, archiveRoot, fqdn string, now time.Time) *Gatherer {
	timestamp := nameFromTime(now)

	g := &Gatherer{
		archiveRoot:    archiveRoot,
		fqdn:           fqdn,
		bundleFileName: fmt.Sprintf("%s-%s.tar.gz", fqdn, timestamp),
	}

	// bundle file is created outside the temporary directory because they're
	// deleted at different times (bundle is downloaded on a second request)
	g.bundleFilePath = path.Join(stagingDir, g.bundleFileName)

	// destination directory for all of the collected data
	g.targetDir = path.Join(g.archiveRoot, g.fqdn, timestamp)

	return g
}

// CreateBundleDir will create the full path (mkdir -p) to the directory where
// all of the data will be collected for this run of the log gatherer
func (g *Gatherer) CreateBundleDir() error {
	if err := os.MkdirAll(g.targetDir, 0700); err != nil {
		log.WithFields(
			log.Fields{"error": err, "path": g.targetDir},
		).Error("Failed to create bundle directory.")

		return err
	}

	return nil
}

// FindFilesToCopy find files under a given path
func (g *Gatherer) FindFilesToCopy(path, name string) []string {
	out, err := command.CombinedOutput("find",
		command.Args(path, "-name", name))
	if err != nil {
		log.WithError(err).Warn("Failed to execute find.")
		return []string{}
	}

	return strings.Split(strings.TrimSpace(out), "\n")
}

func (g *Gatherer) setArchiveTreePermissions() error {
	chmodArgs := command.Args(
		"--recursive",
		"u+rw",
		g.archiveRoot,
	)

	// we use exec.Command() because os.Chmod() only accepts octal mode
	// specification but we want to avoid changing other permissions
	out, err := command.CombinedOutput("chmod", chmodArgs)
	if err != nil {
		log.WithFields(
			log.Fields{"error": err, "output": out},
		).Error("Failed to update permissions")

		return err
	}

	return nil
}

// CreateBundleFile creates a compressed archive (currently tar/gzip) of all
// the data collected on this run
func (g *Gatherer) CreateBundleFile() (BundleInfo, error) {
	if err := g.setArchiveTreePermissions(); err != nil {
		return BundleInfo{}, err
	}

	tarArgs := command.Args(
		"--verbose",
		"--create",
		"--gzip",
		"--file",
		g.bundleFilePath,
		"--directory",
		g.archiveRoot,
		g.fqdn,
	)

	out, err := command.CombinedOutput("tar", tarArgs)
	if err != nil {
		log.WithFields(
			log.Fields{
				"error":      err,
				"archive":    g.bundleFilePath,
				"tar_output": out,
			},
		).Error("Failed to create archive")

		return BundleInfo{}, err
	}

	return NewBundleInfo(g.bundleFilePath)
}

type executer interface {
	execute() error
}

// ExecuteAll executes all of the operations
func (g *Gatherer) ExecuteAll() {
	for _, e := range g.operations {
		_ = e.execute()
	}
}

// AddCommand loads a new command into our list of commands to run
func (g *Gatherer) AddCommand(name, cmd string, args ...string) {
	g.addOperation(&Command{
		Name:          name,
		Cmd:           cmd,
		Args:          args,
		OutputHandler: outputWriter(name, g.targetDir),
	})
}

// AddURL adds an operation to fetch output from a given URL
func (g *Gatherer) AddURL(name, url string) {
	g.addOperation(&URL{
		Name:          name,
		URL:           url,
		OutputHandler: outputWriter(name, g.targetDir),
	})
}

// AddCopy adds a copy operation for a given path
func (g *Gatherer) AddCopy(srcPath string) {
	g.addOperation(&Copy{
		SrcPath:  srcPath,
		DestPath: g.targetDir,
	})
}

// AddCopiesFromPath finds files matching a given name under a given path, and
// adds them as copy operations
func (g *Gatherer) AddCopiesFromPath(fileName, path string) {
	for _, str := range g.FindFilesToCopy(path, fileName) {
		g.AddCopy(str)
	}
}

// AddOther adds a generic operation to the list of gatherers
func (g *Gatherer) AddOther(name string, execFunc func() ([]byte, error)) {
	g.addOperation(&Other{
		Name:          name,
		ExecFunc:      execFunc,
		OutputHandler: outputWriter(name, g.targetDir),
	})
}

func (g *Gatherer) addOperation(x executer) {
	g.operations = append(g.operations, x)
}

// Generate a string from a time value we can use to name files and dirs
func nameFromTime(now time.Time) string {
	return strings.NewReplacer("-", "", ":", "").Replace(now.Format(time.RFC3339))
}

func outputWriter(name string, targetDir string) func() (io.WriteCloser, error) {
	filePath := path.Join(targetDir, name) + ".txt"
	return func() (io.WriteCloser, error) {
		return os.OpenFile(filePath, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	}
}

// FetchHostname - Get the hostname for the currently running machine
func (g *Gatherer) FetchHostname(args ...string) string {
	hostnameOutput, err := command.Output("hostname", command.Args(args...))
	if err != nil {
		log.WithError(err).Error("hostname command failed. Later attempts to ping hostname will fail.")
	}
	return strings.TrimSpace(hostnameOutput)
}
