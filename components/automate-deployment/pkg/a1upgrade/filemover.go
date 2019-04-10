package a1upgrade

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
)

const defaultFileMoverTimeout = 300 * time.Second
const defaultUser = "hab"
const defaultGroup = "hab"
const defaultRsyncCmd = "hab"

// A FileMover moves directories of files on the same system.
// Abstracted into our own type so we can consistently add behaviors
// such as permissions setting and timeouts.
type FileMover struct {
	RelDestPath string
	ServiceName string
	SrcPath     string
	Timeout     time.Duration
	c           command.Executor
	User        string
	Group       string
	RsyncCmd    string
	forceCopy   bool
	habBasePath string
}

// var rather than const here since we reset these in some tests
var defaultHabBaseDir = "/hab/svc"

// FileMoversForConfig returns the file migrations that should take
// place given the passed DeliveryRunning configuration
func FileMoversForConfig(d *DeliveryRunning, workflow bool) []*FileMover {
	fileMovers := []*FileMover{
		NewFileMover(d.Delivery.Insights.DataDirectory, "automate-elasticsearch", "data"),
		NewFileMover(d.Delivery.Compliance.ProfilesPath, "compliance-service", "data/profiles"),
	}

	// if workflow is enabled, we will add the file mover for the git repositories
	if workflow {
		fileMovers = append(
			fileMovers,
			NewFileMover(d.Delivery.Delivery.GitRepos, "automate-workflow-server", "data/git/repos"),
		)
	}

	return fileMovers
}

// FileMoverOpt sets properties of the file mover
type FileMoverOpt func(*FileMover)

// ForceCopy is a FileMover option that forces the file to
// be copied
func ForceCopy() FileMoverOpt {
	return func(f *FileMover) {
		f.forceCopy = true
	}
}

// NewFileMover returns a FileMover with the default timeout and
// commandExector.
func NewFileMover(src string, serviceName string, relDst string, opts ...FileMoverOpt) *FileMover {
	f := &FileMover{
		SrcPath:     src,
		ServiceName: serviceName,
		RelDestPath: strings.TrimLeft(relDst, "/"),
		Timeout:     defaultFileMoverTimeout,
		User:        defaultUser,
		Group:       defaultGroup,
		RsyncCmd:    defaultRsyncCmd,
		c:           command.NewExecExecutor(),
		habBasePath: defaultHabBaseDir,
	}

	for _, o := range opts {
		o(f)
	}

	return f
}

func (f *FileMover) DestPath() string {
	return path.Join(f.habBasePath, f.ServiceName, f.RelDestPath)
}

func (f *FileMover) doneSentinelPath() string {
	destPath := f.DestPath()
	dir := filepath.Dir(destPath)
	base := filepath.Base(destPath)
	return filepath.Join(dir, fmt.Sprintf(".%s-a1-migration-move-complete", base))
}

func (f *FileMover) startedSentinelPath() string {
	destPath := f.DestPath()
	dir := filepath.Dir(destPath)
	base := filepath.Base(destPath)
	return filepath.Join(dir, fmt.Sprintf(".%s-a1-migration-move-started", base))
}

// AlreadyMoved returns true if the directory has already been
// migrated.
func (f *FileMover) AlreadyMoved() (bool, error) {
	exists, err := fileutils.PathExists(f.doneSentinelPath())
	if err != nil {
		return false, err
	}
	return exists, nil
}

// MoveStarted returns true if we've previously attempted to move this
// directory.
func (f *FileMover) MoveStarted() (bool, error) {
	exists, err := fileutils.PathExists(f.startedSentinelPath())
	if err != nil {
		return false, err
	}
	return exists, nil
}

func (f *FileMover) markAsMoved() error {
	return os.Rename(f.startedSentinelPath(), f.doneSentinelPath())
}

func (f *FileMover) markStarted() error {
	file, err := os.OpenFile(f.startedSentinelPath(), os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return err
	}
	return file.Close()
}

// Move performs the file move
func (f *FileMover) Move(w cli.BodyWriter) error {
	destPath := f.DestPath()
	destParent := filepath.Dir(destPath)
	err := os.MkdirAll(destParent, os.ModePerm)
	if err != nil {
		return errors.Wrapf(err, "could not create destination data directory parent %s", destParent)
	}

	moved, err := f.AlreadyMoved()
	if err != nil {
		return errors.Wrap(err, "could not determine if directory has already been moved")
	}

	if !moved {
		err = f.doMove(w)
		if err != nil {
			return err
		}
	} else {
		w.Bodyf("skipping %s -> %s (already moved)", f.SrcPath, destPath)
	}

	return f.fixupPermissions(w)
}

// fixupPermissions on the data directory to the configured user and group.
// TODO: (yzl) perms should match just deploying a hab package
func (f *FileMover) fixupPermissions(w cli.BodyWriter) error {
	svcPath := path.Join(f.habBasePath, f.ServiceName)
	splitRelPath := strings.Split(f.RelDestPath, "/")
	chownPath := path.Join(svcPath, splitRelPath[0])

	ownerSpec := fmt.Sprintf("%s:%s", f.User, f.Group)
	w.Bodyf("setting ownership on %s to %s", chownPath, ownerSpec)
	_, err := f.c.Output("chown", command.Args("-R", ownerSpec, chownPath), command.Timeout(f.Timeout))
	if err != nil {
		return errors.Wrapf(err, "error changing ownership of new data directory %s: %s", chownPath, command.StderrFromError(err))
	}
	return nil
}

func (f *FileMover) doMove(w cli.BodyWriter) error {
	err := f.markStarted()
	if err != nil {
		return errors.Wrapf(err, "error creating file migration sentinel file")
	}

	destPath := f.DestPath()
	sameFs, err := IsSameFilesystem(f.SrcPath, destPath)
	if err != nil {
		return err
	}

	dstExist, err := fileutils.PathExists(destPath)
	if err != nil {
		return err
	}

	if sameFs && !dstExist && !f.forceCopy {
		w.Bodyf("moving %s -> %s (rename)", f.SrcPath, destPath)
		err = f.doRenameMove()
	} else {
		w.Bodyf("moving %s -> %s (rsync)", f.SrcPath, destPath)
		err = f.doRsyncMove()
	}

	if err != nil {
		return err
	}

	err = f.markAsMoved()
	if err != nil {
		// TODO(ssd) 2018-03-16: This is a pretty nasty case.
		// If we just fail marking it as moved, we can't retry
		// safely :/
		return errors.Wrapf(err, "error marking data directory as moved")
	}

	return nil
}

func (f *FileMover) doRsyncMove() error {
	// Rsync options quick reference:
	//
	// -h: output numbers in a human-readable format
	// -S: handle sparse files efficiently
	// -a: "archive", equals -rlptgoD (no -H,-A,-X)
	//
	//      -r: recurses into directories
	//      -l: preserve links
	//      -p: preserve permissions
	//      -t: preserve modification times
	//      -g: preserve group
	//      -o: preserve owner
	//      -D: preserve special files and devices
	//
	// We want to generate an rsync command in the form of:
	//
	// For dir sources:
	// 	rsync OPTS /SRC_PARENT_PATH/src/ /DST_PARENTPATH/dst
	// For file sources:
	// 	rsync OPTS /SRC_PARENT_PATH/src /DST_PARENTPATH
	destPath := f.DestPath()
	rsyncSrc := f.SrcPath
	rsyncDst := destPath

	stat, err := os.Stat(rsyncSrc)

	if err != nil {
		return errors.Wrapf(err, "Failed to stat %s", rsyncSrc)
	}

	if stat.IsDir() {
		if !strings.HasSuffix(rsyncSrc, "/") {
			rsyncSrc = rsyncSrc + "/"
		}

		rsyncDst = strings.TrimRight(destPath, "/")
	}

	args := []string{
		"-haS",
		rsyncSrc,
		rsyncDst,
	}

	// Special case for hab-based rsync.  Actually, the common case.
	if f.RsyncCmd == "hab" {
		args = append([]string{"pkg", "exec", "core/rsync", "rsync"}, args...)
	}

	_, err = f.c.Output(f.RsyncCmd, command.Args(args...), command.Timeout(f.Timeout))
	if err != nil {
		return errors.Wrapf(err, "rsync failed: %s", command.StderrFromError(err))
	}
	return nil
}

func (f *FileMover) doRenameMove() error {
	destPath := f.DestPath()
	err := os.Rename(f.SrcPath, destPath)
	if err != nil {
		return errors.Wrap(err, "rename failed")
	}

	return nil
}
