package backup

import (
	"io"
	"path"

	"github.com/sirupsen/logrus"
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
		if err := d.copyObjectFromBuilder(backupCtx, obj, objVerifier); err != nil {
			return err
		}
	}
	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *BuilderMinioDumpOperation) copyObjectFromBuilder(backupCtx Context, obj BucketObject, objVerifier ObjectVerifier) error {
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
	ctx := backupCtx.ctx
	prefix := path.Join(d.ObjectName...)
	objects, _, err := backupCtx.bucket.List(ctx, prefix, false)
	if err != nil {
		return err
	}

	objVerifier := &NoOpObjectVerifier{}
	for _, obj := range objects {
		if err := d.copyObjectFromBackup(backupCtx, obj, objVerifier); err != nil {
			return err
		}
	}
	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func (d *BuilderMinioDumpOperation) copyObjectFromBackup(backupCtx Context, obj BucketObject, objVerifier ObjectVerifier) error {
	ctx := backupCtx.ctx
	backupObjName := path.Join(path.Join(d.ObjectName...), obj.Name)
	builderObjName := obj.Name
	logrus.Info("Copying from %s -> %s", backupObjName, builderObjName)
	writer, err := backupCtx.builderBucket.NewWriter(ctx, obj.Name)
	if err != nil {
		return err
	}
	defer writer.Close()
	reader, err := backupCtx.bucket.NewReader(ctx, backupObjName, objVerifier)
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
