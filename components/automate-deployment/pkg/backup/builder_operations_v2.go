package backup

import (
	"context"
	"encoding/json"
	"path"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type BuilderMinioDumpOperationV2 struct {
	Name       string   `json:"name"`
	ObjectName []string `json:"storage_key"`
}

type artifactRepoProgressAdapter struct {
	name     string
	progChan chan OperationProgress
}

func (r *artifactRepoProgressAdapter) ReportProgress(completed int64, total int64) {
	if total > 0 {
		r.progChan <- OperationProgress{
			Name:     r.name,
			Progress: (float64(completed) / float64(total)) * 100.0,
		}
	}
}

var _ Operation = &BuilderMinioDumpOperationV2{}

func artifactRepoFromBackupContext(backupCtx Context) *ArtifactRepo {
	return NewArtifactRepo(backupCtx.locationSpec)
}

func builderArtifactStream(backupCtx Context) ArtifactStream {
	objects, _, err := backupCtx.builderBucket.List(backupCtx.ctx, "", false)
	if err != nil {
		logrus.WithError(err).Fatal("failed to list builder artifacts")
	}

	requiredArtifacts := make([]string, 0, len(objects))

	for _, o := range objects {
		logrus.Infof("Builder Artifact: %s", o.Name)
		requiredArtifacts = append(requiredArtifacts, o.Name)
	}
	return NewArrayStream(requiredArtifacts)
}

func (d *BuilderMinioDumpOperationV2) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	ctx, cancel := context.WithCancel(backupCtx.ctx)
	defer cancel()

	requiredArtifacts := builderArtifactStream(backupCtx)
	repo := artifactRepoFromBackupContext(backupCtx)
	reporter := &artifactRepoProgressAdapter{
		name:     d.Name,
		progChan: progChan,
	}
	meta, err := repo.Snapshot(ctx, backupCtx.backupTask.TaskID(), backupCtx.builderBucket,
		requiredArtifacts, ArtifactRepoSnapshotReportProgress(reporter))
	if err != nil {
		return err
	}

	objectName := d.getObjectName()
	writer, err := backupCtx.bucket.NewWriter(ctx, objectName)
	if err != nil {
		return err
	}

	enc := json.NewEncoder(writer)
	if err := enc.Encode(meta); err != nil {
		return writer.Fail(err)
	}

	if err := writer.Close(); err != nil {
		return err
	}

	om.WriteFinished(objectName, writer)

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *BuilderMinioDumpOperationV2) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier,
	progChan chan OperationProgress) error {
	ctx, cancel := context.WithCancel(backupCtx.ctx)
	defer cancel()

	meta, err := d.readSnapshotMetadata(ctx, backupCtx, &NoOpObjectVerifier{})
	if err != nil {
		return err
	}

	repo := artifactRepoFromBackupContext(backupCtx)
	reporter := &artifactRepoProgressAdapter{
		name:     d.Name,
		progChan: progChan,
	}
	if err := repo.Restore(ctx, backupCtx.builderBucket, meta.Name,
		ArtifactRepoRestoreValidateChecksum(meta.Checksum),
		ArtifactRepoRestoreReportProgress(reporter)); err != nil {
		return err
	}

	return nil
}

func (d *BuilderMinioDumpOperationV2) Delete(backupCtx Context) error {
	ctx, cancel := context.WithCancel(backupCtx.ctx)
	defer cancel()

	objectName := d.getObjectName()

	meta, err := d.readSnapshotMetadata(ctx, backupCtx, &NoOpObjectVerifier{})
	if err != nil {
		return err
	}

	repo := artifactRepoFromBackupContext(backupCtx)

	if err := repo.Remove(ctx, meta.Name); err != nil && !IsNotExist(err) {
		return errors.Wrap(err, "failed to remove artifact snapshot")
	}

	if err := backupCtx.bucket.Delete(ctx, []string{objectName}); err != nil {
		return errors.Wrap(err, "failed to remove repo snapshot metadata file")
	}

	return nil
}

func (d *BuilderMinioDumpOperationV2) String() string {
	return d.Name
}

func (d *BuilderMinioDumpOperationV2) getObjectName() string {
	return path.Join(path.Join(d.ObjectName...), "snapshot.meta")
}

func (d *BuilderMinioDumpOperationV2) readSnapshotMetadata(ctx context.Context,
	backupCtx Context, verifier ObjectVerifier) (ArtifactRepoSnapshotMetadata, error) {

	objectName := d.getObjectName()
	meta := ArtifactRepoSnapshotMetadata{}

	if err := verifier.ObjectValid(objectName); err != nil {
		return meta, err
	}

	reader, err := backupCtx.bucket.NewReader(ctx, objectName, verifier)
	if err != nil {
		return meta, errors.Wrapf(err, "could not open object with name %s", objectName)
	}
	defer reader.Close() // nolint: errcheck

	dec := json.NewDecoder(reader)
	if err := dec.Decode(&meta); err != nil {
		return meta, errors.Wrap(err, "could not read artifact repo snapshot metadata file")
	}

	return meta, nil
}
