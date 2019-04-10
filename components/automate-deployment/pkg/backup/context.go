package backup

import (
	"context"
	"encoding/json"
	"time"

	"go.uber.org/multierr"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/grpc/secureconn"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/platform/pg"
)

type BackupStatus string

const (
	// BackupStatusCompleted is written to a backup that has been
	// completed successfully
	BackupStatusCompleted BackupStatus = "completed"
	// BackupStatusInProgress is written to a backup when it starts
	BackupStatusInProgress BackupStatus = "in_progress"
	// BackupStatusFailed is written to a backup when it fails
	BackupStatusFailed BackupStatus = "failed"
	// BackupStatusDeleting is written to a backup we start deleting it
	BackupStatusDeleting BackupStatus = "deleting"
)

const (
	metadataChecksumsObjectName = "checksums.json"
	statusObjectName            = ".status"
)

// Context is stuff
type Context struct {
	pgConnInfo          pg.ConnInfo
	backupTask          *api.BackupTask
	restoreTask         *api.BackupRestoreTask
	ctx                 context.Context
	connFactory         *secureconn.Factory
	esSidecarInfo       ESSidecarConnInfo
	releaseManifest     manifest.ReleaseManifest
	bucket              Bucket
	restoreBucket       Bucket
	locationSpec        LocationSpecification
	restoreLocationSpec LocationSpecification
	metadataWritten     ObjectManifest
}

type MetadataChecksums struct {
	ContentsSHA256 map[string]string `json:"contents_sha256"`
}

// NewContext returns a new backup context
func NewContext(opts ...ContextOpt) Context {
	ctx := &Context{
		pgConnInfo: &pg.A2ConnInfo{},
		backupTask: &api.BackupTask{},
	}

	for _, opt := range opts {
		opt(ctx)
	}

	if ctx.locationSpec != nil {
		ctx.bucket = ctx.locationSpec.ToBucket(ctx.backupTask.TaskID())
	}
	if ctx.restoreLocationSpec != nil {
		ctx.restoreBucket = ctx.restoreLocationSpec.ToBucket(ctx.backupTask.TaskID())
	}

	ctx.metadataWritten = NewObjectManifest()

	return *ctx
}

// ContextOpt represents an optional configuration function for a Runner
type ContextOpt func(*Context)

// WithContextBackupLocationSpecification configures the backup-gateway location
func WithContextBackupLocationSpecification(locationSpec LocationSpecification) ContextOpt {
	return func(ctx *Context) {
		ctx.locationSpec = locationSpec
	}
}

// WithContextRestoreLocationSpecification configures the backup restore remote location
func WithContextBackupRestoreLocationSpecification(locationSpec LocationSpecification) ContextOpt {
	return func(ctx *Context) {
		ctx.restoreLocationSpec = locationSpec
	}
}

// WithContextPgConnInfo configures the context postgres connection info
func WithContextPgConnInfo(info pg.ConnInfo) ContextOpt {
	return func(ctx *Context) {
		ctx.pgConnInfo = info
	}
}

// WithContextBackupTask configures the context backup task
func WithContextBackupTask(task *api.BackupTask) ContextOpt {
	return func(ctx *Context) {
		ctx.backupTask = task
	}
}

// WithContextBackupRestoreTask configures the context backup task
func WithContextBackupRestoreTask(task *api.BackupRestoreTask) ContextOpt {
	return func(ctx *Context) {
		ctx.restoreTask = task
		ctx.backupTask = task.Backup
	}
}

// WithContextCtx :shrug: configures the context's context...
func WithContextCtx(ctx2 context.Context) ContextOpt {
	return func(ctx *Context) {
		ctx.ctx = ctx2
	}
}

// WithContextConnFactory configures the secureconn.Factory to use to connect to services
func WithContextConnFactory(connFactory *secureconn.Factory) ContextOpt {
	return func(ctx *Context) {
		ctx.connFactory = connFactory
	}
}

// WithContextEsSidecarInfo configures the EsSidecar connection info
func WithContextEsSidecarInfo(esSidecarInfo ESSidecarConnInfo) ContextOpt {
	return func(ctx *Context) {
		ctx.esSidecarInfo = esSidecarInfo
	}
}

// WithContextReleaseManifest sets the release manifest. We need this in our testing
// environment to pick up the right version of things
func WithContextReleaseManifest(releaseManifest manifest.ReleaseManifest) ContextOpt {
	return func(ctx *Context) {
		ctx.releaseManifest = releaseManifest
	}
}

// TransactFunc is a function to execute that takes a backup context
type TransactFunc func(Context) error

// Transact runs f. It will first write a status object with 'pending'. If the
// function runs without error, the status object will be overwritten with
// 'completed'. Otherwise, it will be overwritten with 'failed'.
//
// NOTE: Transact used to move a completed backup into place atomically. This
//       assumed that we could do a rename. We now use a blob like api, and
//       there exists no atomic rename. With the blob api, this changed to
//       using a status file. Since the status file did not exist before,
//       a successful backup is one where either there is no status file,
//       or one where the status file contains 'completed'.
func (ctx Context) Transact(f TransactFunc) (string, error) {
	if err := ctx.writeStatus(BackupStatusInProgress); err != nil {
		return "", errors.Wrap(err, "Failed to set status object to pending")
	}

	errTf := f(ctx)

	if errTf != nil {
		if err := ctx.writeStatus(BackupStatusFailed); err != nil {
			return "", multierr.Combine(
				errTf,
				errors.Wrap(err, "Failed to set status object to failed"),
			)
		}
		return "", errTf
	}

	backupSHA256, err := ctx.writeMetadataChecksums()
	if err != nil {
		return "", err
	}

	if err := ctx.writeStatus(BackupStatusCompleted); err != nil {
		return "", errors.Wrap(err, "Failed to set status object to completed")
	}

	return backupSHA256, nil
}

func (ctx Context) TransactDelete(f TransactFunc) error {
	if err := ctx.writeStatus(BackupStatusDeleting); err != nil {
		return errors.Wrap(err, "Failed to set status object to pending")
	}

	errTf := f(ctx)

	if errTf != nil {
		return errTf
	}

	return errors.Wrap(
		ctx.DeleteBackupMetadata(),
		"failed to delete backup metadata")
}

func (ctx Context) DeleteBackupMetadata() error {
	err := ctx.bucket.Delete(ctx.ctx, []string{metadataChecksumsObjectName, statusObjectName})
	if err != nil && !IsNotExist(err) {
		return errors.Wrapf(err, "Failed to delete backup metadata objects [%s, %s]",
			metadataChecksumsObjectName, statusObjectName)
	}
	return nil
}

func (ctx Context) MetadataWritten(mdName string, mdData []byte) {
	ctx.metadataWritten.DataWritten(mdName, mdData)
}

func (ctx Context) writeStatus(status BackupStatus) error {
	logrus.WithFields(
		logrus.Fields{
			"status":    status,
			"backup_id": ctx.backupTask.TaskID(),
		},
	).Info("Writing backup status")
	return ctx.writeStringToBlob(statusObjectName, string(status))
}

func (ctx Context) writeMetadataChecksums() (string, error) {
	cksumData := MetadataChecksums{ContentsSHA256: ctx.metadataWritten.ObjectSHA256s()}

	data, err := json.MarshalIndent(cksumData, "", "    ")
	if err != nil {
		return "", errors.Wrap(err, "failed to marshal service metadata checksum information into JSON")
	}
	data = append(data, '\n')

	backupChecksummer := NewObjectManifest()
	backupChecksummer.DataWritten(metadataChecksumsObjectName, data)
	backupSHA256 := backupChecksummer.ObjectSHA256s()[metadataChecksumsObjectName]

	return backupSHA256, ctx.writeStringToBlob(metadataChecksumsObjectName, string(data))
}

func (ctx Context) writeStringToBlob(storageKey string, str string) error {
	// ctx.ctx gets canceled if the backup errors. Using it means we can never
	// correctly write out a failed status
	writeCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	w, err := ctx.bucket.NewWriter(writeCtx, storageKey)
	if err != nil {
		return errors.Wrapf(err, "Failed to create object %s", storageKey)
	}

	_, err = w.Write([]byte(str))
	if err != nil {
		w.Close()
		return errors.Wrapf(err, "Failed to write to object %s", storageKey)
	}
	err = w.Close()
	if err != nil {
		return errors.Wrapf(err, "Failed to commit %s", storageKey)
	}

	return nil
}
