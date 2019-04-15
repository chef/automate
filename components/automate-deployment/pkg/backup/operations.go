package backup

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/user"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/lib/io/fileutils"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

const (
	defaultDirPerms = 0755
)

// Operation is an interface for a backup operation.
type Operation interface {
	Backup(ctx Context, om ObjectManifest, prog chan OperationProgress) error
	Restore(ctx Context, serviceName string, verifier ObjectVerifier, prog chan OperationProgress) error
	Delete(ctx Context) error
	String() string
}

// testOperation is an operation that sleeps and outputs events. It's only use
// is demonstrating the backup execution event system. It should not be used
// in the final version.
type testOperation struct {
	name       string
	fail       bool
	noProgress bool
}

var _ Operation = &testOperation{}

// Backup executes a backup specification.
func (t *testOperation) Backup(backupCtx Context, _ ObjectManifest, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	if !t.noProgress {
		progChan <- OperationProgress{
			Name:     t.name,
			Progress: float64(0),
		}
	}

	if t.fail {
		return errors.New("test operation failed")
	}

	if !t.noProgress {
		progChan <- OperationProgress{
			Name:     t.name,
			Progress: float64(50),
		}

		progChan <- OperationProgress{
			Name:     t.name,
			Progress: float64(100),
		}
	}

	return nil
}

// Restore executes a backup specification.
func (t *testOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	return nil
}

func (t *testOperation) Delete(ctx Context) error {
	return nil
}

// String returns the string representation of the operation
func (t *testOperation) String() string {
	return t.name
}

// PathCopyOperation backups a source path to a destination path. It also
// supports a slice of optional Rsync matchers to include or exclude files or
// directories from the source path.
type PathCopyOperation struct {
	Name          string         `json:"name"`
	ObjectName    []string       `json:"storage_key"`
	SrcPath       string         `json:"src_path"`
	RsyncMatchers []RsyncMatcher `json:"-"` // don't marshal an interface
	Owner         string
	cmdExecutor   command.Executor
}

var _ Operation = &PathCopyOperation{}

// RsyncMatcher is a rsync include or exclude pattern
type RsyncMatcher interface {
	RsyncArgs() (flag, matcher string)
}

// Include creates an RsyncMatcher representing a `--include PATTERN` argument
// to rsync
func Include(rsyncPattern string) RsyncMatcher {
	return &RsyncInclude{Pattern: rsyncPattern}
}

// RsyncInclude is the concrete type for an rsync include rule
type RsyncInclude struct {
	Pattern string
}

// RsyncArgs gives the rsync args for the include filter
func (r *RsyncInclude) RsyncArgs() (flag, matcher string) {
	return "--include", r.Pattern
}

// Exclude creates an RsyncMatcher representing a `--exclude PATTERN` argument
// to rsync
func Exclude(rsyncPattern string) RsyncMatcher {
	return &RsyncExclude{Pattern: rsyncPattern}
}

// RsyncExclude is the concreate type for an rsync exclude rule
type RsyncExclude struct {
	Pattern string
}

// RsyncArgs give the rsync args for the exclude filter
func (r *RsyncExclude) RsyncArgs() (flag, matcher string) {
	return "--exclude", r.Pattern
}

// Backup backs up a path using the defined fields on the struct and publishes
// progress events to the progress channel.
func (p *PathCopyOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     p.String(),
		Progress: float64(0),
	}

	src := fmt.Sprintf("%s/", p.SrcPath)

	log := logrus.WithFields(logrus.Fields{
		"name":      p.Name,
		"backup_id": backupCtx.backupTask.TaskID(),
		"src_path":  p.SrcPath,
		"operation": "path_copy",
		"action":    "backup",
	})
	log.Info("Running backup operation")

	// TODO (jaym): This seems overkill. Surely we are capable of implementing these
	//              matches with some go code.
	// Get the matching files from rsync. We used to use rsync to backup the files,
	// but to support blob storage it was removed.
	cmd, args := rsyncListOnlyCmd(src, p.RsyncMatchers)

	out, err := p.cmdExecutor.Output(cmd, command.Args(args...))
	if err != nil {
		return errors.Wrapf(err, "command %s %s failed with output '%s'", cmd, strings.Join(args, " "), out)
	}

	// An example of what out will look like:
	// ➜ rsync -a --list-only tmp/ | head
	// drwxr-xr-x          4,096 2018/08/14 09:49:18 .
	// -rw-r--r--      5,510,872 2017/07/15 14:16:59 big_buck_bunny.mp4
	//
	lines := strings.Split(out, "\n")
	for _, line := range lines {
		fields := strings.Fields(line)
		log.WithFields(logrus.Fields{
			"line":   line,
			"fields": fields,
		}).Debug("read line")

		// The last line will not have fields
		if len(fields) != 5 {
			continue
		}

		fpath := path.Join(p.SrcPath, fields[4])

		s, err := os.Stat(fpath)
		if err != nil {
			log.WithField("fpath", fpath).WithError(err).Error("Failed to determine file status")
			return errors.Wrapf(err, "Failed to stat %s", fpath)
		}

		if !s.IsDir() {
			// We create a closure to run backup up each file because we want to
			// defer close any files we open. Using a closure ensures the file is
			// closed and the end of the call to the closure
			objectName := path.Join(path.Join(p.ObjectName...), fields[4])

			err := func() error {
				f, err := os.Open(fpath)
				if err != nil {
					return err
				}
				defer f.Close()
				log.WithFields(logrus.Fields{
					"from_local_path": fpath,
					"to_object_name":  objectName,
				}).Info("backing up file")
				writer, err := backupCtx.bucket.NewWriter(backupCtx.ctx, objectName)
				if err != nil {
					return errors.Wrapf(err, "failed to create writer for %s", objectName)
				}

				_, err = io.Copy(writer, f)
				if err != nil {
					return writer.Fail(err)
				}
				if err = writer.Close(); err != nil {
					return err
				}
				om.WriteFinished(objectName, writer)

				return nil
			}()

			if err != nil {
				return errors.Wrapf(err, "failed to back up file %s to %s", fpath, objectName)
			}
		}
	}

	progChan <- OperationProgress{
		Name:     p.String(),
		Progress: float64(100),
	}

	return nil
}

// TODO (jaym): gather this from the plans
func defaultOwner(serviceName string) string {
	switch serviceName {
	case "deployment-service", "automate-ui", "automate-load-balancer", "automate-cs-nginx":
		return "root"
	default:
		return "hab"

	}
}

// writeFileForService creates all the needed directories to write the file.
// The ownership of the files will be based on the service name provided if an
// explicit owner was not provided.
// For example, deployment-service runs as root, so we'll restore all the files
// as root.
// It the service runs as hab, everything up to /hab/svc/service-name will be owned
// by root. The remaining will be owned by hab. habitat may decide to change the
// ownership on some of the directories (for example, it might say /hab/svc/service-name/config is root owned).
func writeFileForService(serviceName string, owner string, dstPath string, reader io.Reader) error {
	dstDir := path.Dir(dstPath)
	if err := os.MkdirAll(dstDir, defaultDirPerms); err != nil {
		return errors.Wrapf(err, "failed to create destination directory %s", dstPath)
	}

	err := fileutils.AtomicWrite(dstPath, reader, fileutils.WithAtomicWriteFileMode(0600))
	if err != nil {
		return errors.Wrapf(err, "failed to write file %s", dstPath)
	}

	// Everything outside /hab/svc will be root always
	svcDir := "/hab/svc/" + serviceName
	if !strings.HasPrefix(dstPath, svcDir) {
		return nil
	}
	if owner == "" {
		owner = defaultOwner(serviceName)
	}

	// TODO (jaym): We should really cache the UID and GID
	var uid int
	var gid int

	usr, err := user.Lookup(owner)
	if err != nil {
		logrus.WithError(err).Warnf("Could not find user %s", owner)
		return errors.Wrapf(err, "failed to get uid for user '%s'", owner)
	}

	uid, err = strconv.Atoi(usr.Uid)
	if err != nil {
		logrus.WithError(err).Warnf("Invalid uid %q", uid)
		return errors.Wrapf(err, "failed to parse uid (%s) for user '%s'", usr.Uid, owner)
	}

	grp, err := user.LookupGroup("hab")
	var gidStr string
	if err != nil {
		logrus.WithError(err).Warnf("Could not find group hab")
		// fallback to the default group
		gidStr = usr.Gid
	} else {
		gidStr = grp.Gid
	}

	gid, err = strconv.Atoi(gidStr)
	if err != nil {
		logrus.WithError(err).Warnf("Invalid gid %q", gid)
		gid = -1
	}

	// For all the directories and files between /hab/svc/service-name and
	// /hab/svc/service-name/foo/.../bar.baz, make sure the owner matches matches
	// the pkg_svc_user for /hab/svc/service-name/foo ... /hab/svc/service-name/foo/bar.baz
	relPath, err := filepath.Rel(svcDir, dstPath)
	if err != nil {
		// This will happen if a service tries to restore a file from another service
		return errors.Wrapf(err, "failed to get relative path from %s to %s", svcDir, dstPath)
	}

	for {
		if relPath == "." {
			break
		}

		p := path.Join(svcDir, relPath)
		logrus.WithFields(logrus.Fields{
			"path": p,
			"uid":  uid,
			"gid":  gid,
		}).Debug("chowning file")
		err := os.Chown(p, uid, gid)

		if err != nil {
			logrus.WithError(err).Warnf("Failed to chown %s", p)
		}

		relPath = path.Dir(relPath)
	}

	return nil
}

func (p *PathCopyOperation) Delete(backupCtx Context) error {
	// Make sure the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	basePath := path.Join(path.Join(p.ObjectName...), "/")

	logrus.WithFields(logrus.Fields{
		"name":      p.Name,
		"backup_id": backupCtx.backupTask.TaskID(),
		"base_path": basePath,
		"operation": "path_copy",
		"action":    "delete",
	}).Info("Running backup operation")
	deleteObjs, _, err := backupCtx.bucket.List(backupCtx.ctx, basePath, false)
	if err != nil {
		return errors.Wrapf(err, "failed to list objects at %s", basePath)
	}

	// TODO(jaym): bucket.List should just return a list of strings
	objPaths := make([]string, len(deleteObjs))
	for i, o := range deleteObjs {
		objPaths[i] = o.Name
	}

	if err := backupCtx.bucket.Delete(backupCtx.ctx, objPaths); err != nil {
		return errors.Wrapf(err, "failed to delete object")
	}

	return nil
}

// Restore restores a path using the defined fields on the struct and publishes
// progress events to the progress channel.
func (p *PathCopyOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     p.String(),
		Progress: float64(0),
	}

	// reverse the source and destination paths because we're restoring
	basePath := path.Join(path.Join(p.ObjectName...), "/")

	dstPath := p.SrcPath

	log := logrus.WithFields(logrus.Fields{
		"name":       p.Name,
		"backup_id":  backupCtx.backupTask.TaskID(),
		"restore_id": backupCtx.restoreTask.TaskID(),
		"base_path":  basePath,
		"operation":  "path_copy",
		"action":     "restore",
	})
	log.Info("Running backup operation")

	restoreObjs, _, err := backupCtx.bucket.List(backupCtx.ctx, basePath, false)
	if err != nil {
		return errors.Wrapf(err, "failed to list objects at %s", basePath)
	}

	for _, obj := range restoreObjs {
		if err := verifier.ObjectValid(obj.Name); err != nil {
			return err
		}

		// The relPath gets the key relative to the
		relPath, err := filepath.Rel(basePath, obj.Name)
		if err != nil {
			return errors.Wrapf(err, "failed to get relative path from %s to %s", basePath, obj.Name)
		}
		dst := path.Join(dstPath, relPath)

		log.WithFields(logrus.Fields{
			"object_name": p.Name,
			"object_path": dst,
		}).Info("Restoring file")

		reader, err := backupCtx.bucket.NewReader(backupCtx.ctx, obj.Name, verifier)
		if err != nil {
			return errors.Wrapf(err, "could not open object with name %s", obj.Name)
		}
		defer reader.Close()

		err = writeFileForService(serviceName, p.Owner, dst, reader)
		if err != nil {
			return errors.Wrapf(err, "failed to restore %s to %s", obj.Name, dst)
		}
	}

	progChan <- OperationProgress{
		Name:     p.String(),
		Progress: float64(100),
	}

	return nil
}

// String returns the string representation of the operation
func (p *PathCopyOperation) String() string {
	return p.Name
}

// MetadataWriterOperation represents an operation that writes the metadata.json
type MetadataWriterOperation struct {
	Spec       *Spec    `json:"spec"`
	ObjectName []string `json:"storage_key"`
}

var _ Operation = &MetadataWriterOperation{}

// Backup executes a backup specification.
func (m *MetadataWriterOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     m.String(),
		Progress: float64(0),
	}

	md := &Metadata{
		Spec:                     m.Spec,
		Task:                     backupCtx.backupTask,
		DeploymentServiceVersion: "foo", // wat?
		ContentsSHA256:           om.ObjectSHA256s(),
	}

	data, err := json.MarshalIndent(md, "", "    ")
	if err != nil {
		return errors.Wrap(err, "failed to marshal backup specification into json")
	}
	data = append(data, '\n')

	objectName := path.Join(path.Join(m.ObjectName...), metadataFileBaseName)

	logrus.WithFields(logrus.Fields{
		"name":      m.Spec.Name,
		"backup_id": backupCtx.backupTask.TaskID(),
		"operation": "metadata_writer",
		"action":    "backup",
	}).Info("Running backup operation")

	writer, err := backupCtx.bucket.NewWriter(backupCtx.ctx, objectName)
	if err != nil {
		return errors.Wrapf(err, "failed to create writer for %s", objectName)
	}

	_, err = writer.Write(data)
	if err != nil {
		return writer.Fail(
			errors.Wrapf(err, "failed to write metadata to %s", objectName),
		)
	}

	// TODO: make the atomic writer capable of handling multiple closes
	err = writer.Close()
	if err != nil {
		return errors.Wrapf(err, "failed to commit metadata to %s", objectName)
	}

	backupCtx.MetadataWritten(objectName, data)

	progChan <- OperationProgress{
		Name:     m.String(),
		Progress: float64(100),
	}

	return nil
}

// Restore executes a backup specification.
func (m *MetadataWriterOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	return nil
}

func (m *MetadataWriterOperation) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	objectPath := path.Join(path.Join(m.ObjectName...), metadataFileBaseName)
	logrus.WithFields(logrus.Fields{
		"name":        m.Spec.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"object_path": objectPath,
		"operation":   "metadata_writer",
		"action":      "delete",
	}).Info("Running backup operation")

	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{objectPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", objectPath)
	}

	return nil
}

// String returns the string representation of the operation
func (m *MetadataWriterOperation) String() string {
	return m.Spec.Name
}

// CommandExecuteOperation represets a command running execution
type CommandExecuteOperation struct {
	Name        string   `json:"name"`
	ObjectName  []string `json:"storage_key"`
	Cmd         Cmd      `json:"cmd"`
	PkgOrigin   string   `json:"pkg_origin"`
	PkgName     string   `json:"pkg_name"`
	cmdExecutor command.Executor
}

var _ Operation = &CommandExecuteOperation{}

// Cmd represents a command to be run to dump/restore data from an a2 service.
// Commands provide a way to backup/restore data that's not stored in postgres
// or elasticsearch and cannot be safely copied from a service's data dir.
type Cmd struct {
	Name    string   `json:"name"`
	Dump    []string `json:"dump"`
	Restore []string `json:"restore"`
}

// Backup executes a backup specification.
func (c *CommandExecuteOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     c.String(),
		Progress: float64(0),
	}

	objectName := path.Join(path.Join(c.ObjectName...), c.Cmd.Name)

	var pkgSpec string
	if backupCtx.releaseManifest != nil {
		pkg := manifest.VersionedPackageFromManifest(backupCtx.releaseManifest, c.PkgName)
		if pkg != nil {
			pkgSpec = fmt.Sprintf("%s/%s/%s/%s", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
		}
	}

	if pkgSpec == "" {
		pkgSpec = fmt.Sprintf("%s/%s", c.PkgOrigin, c.PkgName)
	}

	cmdToExec := []string{"hab", "pkg", "exec", pkgSpec}
	cmdToExec = append(cmdToExec, c.Cmd.Dump...)

	writer, err := backupCtx.bucket.NewWriter(backupCtx.ctx, objectName)
	if err != nil {
		return errors.Wrapf(err, "failed to create writer for %s", objectName)
	}

	logrus.WithFields(logrus.Fields{
		"name":        c.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"cmd":         strings.Join(cmdToExec, " "),
		"object_name": objectName,
		"operation":   "command_execute",
		"action":      "backup",
	}).Info("Running backup operation")

	err = c.cmdExecutor.Run(cmdToExec[0],
		command.Args(cmdToExec[1:]...),
		command.Stdout(writer))
	if err != nil {
		err = writer.Fail(err)
		cmd := strings.Join(cmdToExec, " ")
		if ee, ok := err.(*exec.ExitError); ok {
			return errors.Wrapf(err, "failed to run %s backup command %s - stderr output: %s",
				c.Name,
				cmd,
				ee.Stderr)
		}
		return errors.Wrapf(err, "failed to run %s backup command %s", c.Name, cmd)
	}

	err = writer.Close()
	if err != nil {
		return errors.Wrapf(err, "failed to commit to %s", objectName)
	}

	om.WriteFinished(objectName, writer)

	progChan <- OperationProgress{
		Name:     c.String(),
		Progress: float64(100),
	}

	return nil
}

// Restore executes a backup specification.
func (c *CommandExecuteOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     c.String(),
		Progress: float64(0),
	}

	// Exit early if a restore command is not set
	if c.Cmd.Restore == nil {
		progChan <- OperationProgress{
			Name:     c.String(),
			Progress: float64(100),
		}

		return nil
	}

	var pkgSpec string
	if backupCtx.releaseManifest != nil {
		pkg := manifest.VersionedPackageFromManifest(backupCtx.releaseManifest, c.PkgName)
		if pkg != nil {
			pkgSpec = fmt.Sprintf("%s/%s/%s/%s", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
		}
	}

	if pkgSpec == "" {
		pkgSpec = fmt.Sprintf("%s/%s", c.PkgOrigin, c.PkgName)
	}

	cmd := []string{"hab", "pkg", "exec", pkgSpec}
	cmd = append(cmd, c.Cmd.Restore...)

	objectName := path.Join(path.Join(c.ObjectName...), c.Cmd.Name)

	if err := verifier.ObjectValid(objectName); err != nil {
		return err
	}

	reader, err := backupCtx.bucket.NewReader(backupCtx.ctx, objectName, verifier)
	if err != nil {
		return errors.Wrapf(err, "could not open object with name %s", objectName)
	}
	defer reader.Close()

	err = c.cmdExecutor.Run(cmd[0], command.Args(cmd[1:]...), command.Stdin(reader))
	if err != nil {
		return errors.Wrapf(err, "command %s failed with error '%s'", strings.Join(cmd, " "), command.StderrFromError(err))
	}

	progChan <- OperationProgress{
		Name:     c.String(),
		Progress: float64(100),
	}

	return nil
}

func (c *CommandExecuteOperation) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	objectPath := path.Join(path.Join(c.ObjectName...), c.Cmd.Name)

	logrus.WithFields(logrus.Fields{
		"name":        c.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"object_path": objectPath,
		"operation":   "command_execute",
		"action":      "delete",
	}).Info("Running backup operation")

	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{objectPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", objectPath)
	}

	return nil
}

// String returns the string representation of the operation
func (c *CommandExecuteOperation) String() string {
	return c.Name
}

// DatabaseDumpOperation represents a database that the service depends on
type DatabaseDumpOperation struct {
	Name        string   `json:"name"`
	User        string   `json:"user"`
	ObjectName  []string `json:"storage_key"`
	cmdExecutor command.Executor
}

var _ Operation = &DatabaseDumpOperation{}

// Backup executes a backup operation.
func (d *DatabaseDumpOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	// DatabaseDumpOperation should no longer be used for backing up.
	// Use DatabaseDumpOperationV2. DatabaseDumpOperation dumped the
	// roles in the db dump, which was problematic for external pg.
	return errors.New("Unimplemented")
}

// Restore executes a backup operation.
func (d *DatabaseDumpOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(0),
	}

	objectName := path.Join(path.Join(d.ObjectName...), fmt.Sprintf("%s.sql", d.Name))

	logrus.WithFields(logrus.Fields{
		"name":        d.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"restore_id":  backupCtx.restoreTask.TaskID(),
		"user":        d.User,
		"object_name": objectName,
		"operation":   "database_dump",
		"action":      "restore",
	}).Info("Running backup operation")

	if err := verifier.ObjectValid(objectName); err != nil {
		return err
	}

	reader, err := backupCtx.bucket.NewReader(backupCtx.ctx, objectName, verifier)
	if err != nil {
		return errors.Wrapf(err, "could not open object with name %s", objectName)
	}
	defer reader.Close()

	p := &pg.DatabaseExporter{
		Name:              d.Name,
		User:              d.User,
		DisableRoleCreate: backupCtx.IsExternalPG(),
		CmdExecutor:       d.cmdExecutor,
		ConnInfo:          backupCtx.pgConnInfo,
		Stdin:             reader,
	}

	if err := p.Import(false); err != nil {
		return errors.Wrapf(err, "failed to import database dump from %s", objectName)
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *DatabaseDumpOperation) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	objectPath := path.Join(path.Join(d.ObjectName...), fmt.Sprintf("%s.sql", d.Name))

	logrus.WithFields(logrus.Fields{
		"name":        d.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"object_path": objectPath,
		"operation":   "database_dump",
		"action":      "delete",
	}).Info("Running backup operation")

	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{objectPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", objectPath)
	}

	return nil
}

// String returns the string representation of the operation
func (d *DatabaseDumpOperation) String() string {
	return d.Name
}

// ElasticsearchOperation represents a index in ES a service needs backed up / restored
type ElasticsearchOperation struct {
	ServiceName    string
	MultiIndexSpec string
}

var _ Operation = &ElasticsearchOperation{}

// Backup backs up indices in ES
func (esop *ElasticsearchOperation) Backup(backupCtx Context, _ ObjectManifest, progChan chan OperationProgress) error {
	backupID := backupCtx.backupTask.TaskID()

	log := logrus.WithFields(logrus.Fields{
		"backup_id":        backupID,
		"service_name":     esop.ServiceName,
		"multi_index_spec": esop.MultiIndexSpec,
		"operation":        "elasticsearch_snapshot",
		"action":           "backup",
	})
	log.Info("Running backup operation")

	conn, err := backupCtx.connFactory.DialContext(backupCtx.ctx, "es-sidecar-service", backupCtx.esSidecarInfo.Address())
	if err != nil {
		return errors.Wrap(err, "Could not connect to es-sidecar-service for backup operation")
	}
	defer conn.Close()

	client := es_sidecar.NewEsSidecarClient(conn)

	_, err = client.CreateSnapshot(backupCtx.ctx, &es_sidecar.CreateSnapshotRequest{
		ServiceName:             esop.ServiceName,
		MultiIndexSpecification: esop.MultiIndexSpec,
		BackupId:                backupID,
	})

	if err != nil {
		return errors.Wrap(err, "es-sidecar-service failed to create snapshot")
	}

	return monitorEsProgress(backupCtx.ctx, esop.String(), log, progChan, func() (esProgress, error) {
		return client.CreateSnapshotStatus(backupCtx.ctx, &es_sidecar.CreateSnapshotStatusRequest{
			ServiceName: esop.ServiceName,
			BackupId:    backupID,
		})
	})
}

// Restore executes a backup operation.
func (esop *ElasticsearchOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	backupID := backupCtx.restoreTask.Backup.TaskID()

	log := logrus.WithFields(logrus.Fields{
		"backup_id":    backupID,
		"restore_id":   backupCtx.restoreTask.TaskID(),
		"service_name": esop.ServiceName,
		"operation":    "elasticsearch_snapshot",
		"action":       "restore",
	})
	log.Info("Running backup operation")

	conn, err := backupCtx.connFactory.DialContext(backupCtx.ctx, "es-sidecar-service", backupCtx.esSidecarInfo.Address())
	if err != nil {
		return errors.Wrap(err, "Could not connect to es-sidecar-service for restore operation")
	}
	defer conn.Close()

	client := es_sidecar.NewEsSidecarClient(conn)

	_, err = client.RestoreSnapshot(backupCtx.ctx, &es_sidecar.RestoreSnapshotRequest{
		ServiceName: esop.ServiceName,
		BackupId:    backupID,
	})

	if err != nil {
		return errors.Wrap(err, "es-sidecar-service failed to restore snapshot")
	}

	return monitorEsProgress(backupCtx.ctx, esop.String(), log, progChan, func() (esProgress, error) {
		return client.RestoreSnapshotStatus(backupCtx.ctx, &es_sidecar.RestoreSnapshotStatusRequest{
			ServiceName: esop.ServiceName,
			BackupId:    backupID,
		})
	})
}

func (esop *ElasticsearchOperation) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	backupID := backupCtx.backupTask.TaskID()

	logrus.WithFields(logrus.Fields{
		"backup_id":    backupID,
		"service_name": esop.ServiceName,
		"operation":    "elasticsearch_snapshot",
		"action":       "delete",
	}).Info("Running backup operation")

	conn, err := backupCtx.connFactory.DialContext(backupCtx.ctx, "es-sidecar-service", backupCtx.esSidecarInfo.Address())
	if err != nil {
		return errors.Wrap(err, "Could not connect to es-sidecar-service for restore operation")
	}
	defer conn.Close()

	client := es_sidecar.NewEsSidecarClient(conn)
	if _, err := client.DeleteSnapshot(backupCtx.ctx, &es_sidecar.DeleteSnapshotRequest{
		ServiceName: esop.ServiceName,
		BackupId:    backupID,
	}); err != nil {
		return errors.Wrapf(err, "failed to delete elasticsearch backup %s for %s", backupID, esop.ServiceName)
	}

	return nil
}

type esProgress interface {
	GetSnapshotState() es_sidecar.SnapshotState
	GetProgressPercentage() float64
	GetMessage() string
}
type esStatusFunc func() (esProgress, error)

func monitorEsProgress(ctx context.Context,
	name string,
	log *logrus.Entry,
	progChan chan OperationProgress,
	f esStatusFunc) error {

	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     name,
		Progress: float64(0),
	}

	retries := 0
Progress:
	for {
		resp, err := f()

		if err != nil {
			if retries >= 2 {
				log.WithError(err).Error("Failed to get status")
				return err
			}
			log.WithError(err).Warn("Failed to get status")
			retries++
		} else {
			log.WithFields(logrus.Fields{
				"message":  resp.GetMessage(),
				"state":    resp.GetSnapshotState(),
				"progress": resp.GetProgressPercentage(),
			}).Infof("Elasticsearch operation progress")

			switch resp.GetSnapshotState() {
			case es_sidecar.SnapshotState_IN_PROGRESS:
				progChan <- OperationProgress{
					Name:     name,
					Progress: resp.GetProgressPercentage(),
				}
			case es_sidecar.SnapshotState_SUCCESS:
				break Progress
			default:
				return fmt.Errorf("Elasticsearch operation failed: %s", resp.GetMessage())
			}
		}

		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(1 * time.Second):
		}
	}

	progChan <- OperationProgress{
		Name:     name,
		Progress: float64(100),
	}

	return nil
}

func (esop *ElasticsearchOperation) String() string {
	return esop.ServiceName
}

func rsyncListOnlyCmd(src string, matchers []RsyncMatcher) (cmd string, args []string) {
	rsyncCmd := []string{
		"hab", "pkg", "exec", "core/rsync",
		"rsync",
		"--recursive",
		"--list-only",
	}
	for _, matcher := range matchers {
		flag, pattern := matcher.RsyncArgs()
		rsyncCmd = append(rsyncCmd, flag, pattern)
	}
	rsyncCmd = append(rsyncCmd, src)
	return rsyncCmd[0], rsyncCmd[1:]
}

// DatabaseDumpOperationV2 represents a database that the service depends on.
type DatabaseDumpOperationV2 struct {
	Name        string   `json:"name"`
	User        string   `json:"user"`
	ObjectName  []string `json:"storage_key"`
	cmdExecutor command.Executor
}

var _ Operation = &DatabaseDumpOperation{}

// Backup executes a backup operation.
func (d *DatabaseDumpOperationV2) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(0),
	}

	objectName := path.Join(path.Join(d.ObjectName...), fmt.Sprintf("%s.fc", d.Name))
	writer, err := backupCtx.bucket.NewWriter(backupCtx.ctx, objectName)
	if err != nil {
		return errors.Wrapf(err, "failed to create writer for %s", objectName)
	}

	p := pg.DatabaseExporter{
		Name:              d.Name,
		DisableRoleCreate: backupCtx.IsExternalPG(),
		CmdExecutor:       d.cmdExecutor,
		ConnInfo:          backupCtx.pgConnInfo,
		Stdout:            writer,
		UseCustomFormat:   true,
	}

	logrus.WithFields(logrus.Fields{
		"name":        d.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"object_name": objectName,
		"operation":   "database_dump_v2",
		"action":      "backup",
	}).Info("Running backup operation")

	// TODO(ssd) 2018-04-16: Pass the progress bar and context
	// into here.
	err = p.Export()
	if err != nil {
		return writer.Fail(
			errors.Wrapf(err, "failed to export database for %s", objectName),
		)
	}

	err = writer.Close()
	if err != nil {
		return errors.Wrapf(err, "failed to commit database dump to %s", objectName)
	}

	om.WriteFinished(objectName, writer)

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

// Restore executes a backup operation.
func (d *DatabaseDumpOperationV2) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	// Make the context isn't dead before we start the operation
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(0),
	}

	objectName := path.Join(path.Join(d.ObjectName...), fmt.Sprintf("%s.fc", d.Name))

	logrus.WithFields(logrus.Fields{
		"name":        d.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"restore_id":  backupCtx.restoreTask.TaskID(),
		"object_name": objectName,
		"operation":   "database_dump_v2",
		"action":      "restore",
	}).Info("Running backup operation")

	if err := verifier.ObjectValid(objectName); err != nil {
		return err
	}

	reader, err := backupCtx.bucket.NewReader(backupCtx.ctx, objectName, verifier)
	if err != nil {
		return errors.Wrapf(err, "could not open object with name %s", objectName)
	}
	defer reader.Close()

	p := &pg.DatabaseExporter{
		Name:              d.Name,
		DisableRoleCreate: true,
		CmdExecutor:       d.cmdExecutor,
		ConnInfo:          backupCtx.pgConnInfo,
		Stdin:             reader,
		UseCustomFormat:   true,
	}

	if err := p.Import(false); err != nil {
		return errors.Wrapf(err, "failed to import database dump from %s", objectName)
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *DatabaseDumpOperationV2) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	objectPath := path.Join(path.Join(d.ObjectName...), fmt.Sprintf("%s.fc", d.Name))

	logrus.WithFields(logrus.Fields{
		"name":        d.Name,
		"backup_id":   backupCtx.backupTask.TaskID(),
		"object_path": objectPath,
		"operation":   "database_dump_v2",
		"action":      "delete",
	}).Info("Running backup operation")

	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{objectPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", objectPath)
	}

	return nil
}

// String returns the string representation of the operation
func (d *DatabaseDumpOperationV2) String() string {
	return d.Name
}
