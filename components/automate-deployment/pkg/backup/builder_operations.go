package backup

import (
	"bufio"
	"io"
	"path"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type BuilderMinioDumpOperation struct {
	Name       string   `json:"name"`
	ObjectName []string `json:"storage_key"`
}

var _ Operation = &BuilderMinioDumpOperation{}

func (d *BuilderMinioDumpOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	return errors.New("deprecated backup operation")
}

func (d *BuilderMinioDumpOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	backupBucketPrefix := d.backupBucketPrefix()

	logrus.WithFields(logrus.Fields{
		"name":      d.Name,
		"backup_id": backupCtx.backupTask.TaskID(),
		"prefix":    backupBucketPrefix,
		"operation": "builder_minio_dump",
		"action":    "restore",
	}).Info("Running backup operation")

	objects, err := d.readBuilderArtifactList(backupCtx, backupBucketPrefix, verifier)
	if err != nil {
		return err
	}

	objVerifier := &NoOpObjectVerifier{}
	for _, obj := range objects {
		if err := d.copyObjectFromBackup(backupCtx, backupBucketPrefix, obj, objVerifier); err != nil {
			return err
		}
	}
	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

const MinioDeleteBatchSize = 512

func (d *BuilderMinioDumpOperation) Delete(backupCtx Context) error {
	objVerifier := &NoOpObjectVerifier{}
	backupBucketPrefix := d.backupBucketPrefix()

	logrus.WithFields(logrus.Fields{
		"name":      d.Name,
		"backup_id": backupCtx.backupTask.TaskID(),
		"prefix":    backupBucketPrefix,
		"operation": "builder_minio_dump",
		"action":    "delete",
	}).Info("Running backup operation")

	objects, err := d.readBuilderArtifactList(backupCtx, backupBucketPrefix, objVerifier)
	if err != nil {
		return err
	}
	err = applyBatch(objects, MinioDeleteBatchSize, func(batch []string) error {
		for i, o := range batch {
			batch[i] = path.Join(backupBucketPrefix, o)
		}
		return backupCtx.bucket.Delete(backupCtx.ctx, batch)
	})
	if err != nil {
		return errors.Wrapf(err, "failed to delete objects")
	}

	manifestPath := builderArtifactManifestPath(backupBucketPrefix)
	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{manifestPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", manifestPath)
	}

	return nil
}

func (d *BuilderMinioDumpOperation) String() string {
	return d.Name
}

// applyBatch calls f with sub-slices of the src slice. The function f
// should know what it's doing if it modifies the elements of the
// passed slice.
func applyBatch(src []string, batchSize int, f func([]string) error) error {
	srcLen := len(src)
	if srcLen < batchSize {
		return f(src)
	}

	for i := 0; i < srcLen; i = i + batchSize {
		end := i + batchSize
		if end > srcLen {
			end = srcLen
		}
		err := f(src[i:end])
		if err != nil {
			return err
		}
	}
	return nil
}

// backupBucketPrefix returns the path prefix for all objects that we
// care about in the Backup Bucket.  Backup() places data INTO this
// prefix. Restore() takes data OUT OF this prefix.
func (d *BuilderMinioDumpOperation) backupBucketPrefix() string {
	return path.Join(d.ObjectName...)
}

// builderArtifactManifestPath is the path to our manifest of
// backed-up objects.
func builderArtifactManifestPath(backupBucketPrefix string) string {
	return path.Join(backupBucketPrefix, "builder-artifacts")
}

func (d *BuilderMinioDumpOperation) readBuilderArtifactList(backupCtx Context, prefix string, verifier ObjectVerifier) ([]string, error) {
	ctx := backupCtx.ctx
	reader, err := backupCtx.bucket.NewReader(ctx, builderArtifactManifestPath(prefix), verifier)
	if err != nil {
		return nil, err
	}
	defer reader.Close()
	artifacts := []string{}
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		artifacts = append(artifacts, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return artifacts, nil
}

// copyObjectFromBuilder copies data from the automate-builder-minio
// Bucket to the backup bucket.
func (d *BuilderMinioDumpOperation) copyObjectFromBuilder(backupCtx Context, backupPrefix string, obj BucketObject, objVerifier ObjectVerifier) error {
	ctx := backupCtx.ctx

	backupObjectName := path.Join(backupPrefix, obj.Name)
	builderObjectName := obj.Name

	reader, err := backupCtx.builderBucket.NewReader(ctx, builderObjectName, objVerifier)
	if err != nil {
		return err
	}
	defer reader.Close()

	writer, err := backupCtx.bucket.NewWriter(ctx, backupObjectName)
	if err != nil {
		return err
	}
	if _, err := io.Copy(writer, reader); err != nil {
		writer.Fail(err) // nolint: errcheck
		return err
	}
	return writer.Close()
}

// copyObjectFromBackup copies data from the backup bucket to the
// automate-builder-minio bucket.
func (d *BuilderMinioDumpOperation) copyObjectFromBackup(backupCtx Context, backupPrefix string, obj string, objVerifier ObjectVerifier) error {
	ctx := backupCtx.ctx

	backupObjectName := path.Join(backupPrefix, obj)
	builderObjectName := obj

	reader, err := backupCtx.bucket.NewReader(ctx, backupObjectName, objVerifier)
	if err != nil {
		return err
	}
	defer reader.Close()

	writer, err := backupCtx.builderBucket.NewWriter(ctx, builderObjectName)
	if err != nil {
		return err
	}
	if _, err := io.Copy(writer, reader); err != nil {
		writer.Fail(err) // nolint: errcheck
		return err
	}
	return writer.Close()
}
