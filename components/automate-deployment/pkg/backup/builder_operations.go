package backup

import (
	"io"
	"path"
)

type BuilderMinioDumpOperation struct {
	Name       string   `json:"name"`
	ObjectName []string `json:"storage_key"`
}

var _ Operation = &BuilderMinioDumpOperation{}

func (d *BuilderMinioDumpOperation) Backup(backupCtx Context, _ ObjectManifest, progChan chan OperationProgress) error {
	ctx := backupCtx.ctx
	objects, _, err := backupCtx.builderBucket.List(ctx, "", false)
	if err != nil {
		return err
	}

	objVerifier := &NoOpObjectVerifier{}
	for _, obj := range objects {
		if err := d.copyObject(backupCtx, obj, objVerifier); err != nil {
			return err
		}
	}
	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *BuilderMinioDumpOperation) copyObject(backupCtx Context, obj BucketObject, objVerifier ObjectVerifier) error {
	ctx := backupCtx.ctx
	objectName := path.Join(path.Join(d.ObjectName...), obj.Name)
	writer, err := backupCtx.bucket.NewWriter(ctx, objectName)
	if err != nil {
		return err
	}
	defer writer.Close()
	reader, err := backupCtx.builderBucket.NewReader(ctx, obj.Name, objVerifier)
	if err != nil {
		return err
	}
	defer reader.Close()
	if _, err := io.Copy(writer, reader); err != nil {
		writer.Fail(err) // nolint: errcheck
		return err
	}
	return writer.Close()
}

func (d *BuilderMinioDumpOperation) Restore(backupCtx Context, serviceName string, verifier ObjectVerifier, progChan chan OperationProgress) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *BuilderMinioDumpOperation) Delete(backupCtx Context) error {
	select {
	case <-backupCtx.ctx.Done():
		return backupCtx.ctx.Err()
	default:
	}

	return nil
}

func (d *BuilderMinioDumpOperation) String() string {
	return d.Name
}
