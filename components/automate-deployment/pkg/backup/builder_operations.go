package backup

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"

	"github.com/pkg/errors"
)

type BuilderMinioDumpOperation struct {
	Name       string   `json:"name"`
	ObjectName []string `json:"storage_key"`
}

var _ Operation = &BuilderMinioDumpOperation{}

func (d *BuilderMinioDumpOperation) Backup(backupCtx Context, om ObjectManifest, progChan chan OperationProgress) error {
	ctx := backupCtx.ctx
	objects, _, err := backupCtx.builderBucket.List(ctx, "", false)
	if err != nil {
		return err
	}
	builderArtifacts, err := ioutil.TempFile("", "builderartifacts")
	if err != nil {
		return err
	}
	defer builderArtifacts.Close()
	defer os.Remove(builderArtifacts.Name())

	objVerifier := &NoOpObjectVerifier{}
	for _, obj := range objects {
		if err := d.copyObjectFromBuilder(backupCtx, obj, objVerifier); err != nil {
			return err
		}
		if _, err := fmt.Fprintln(builderArtifacts, obj.Name); err != nil {
			return err
		}
	}
	// Write file with artifact list
	if _, err := builderArtifacts.Seek(0, os.SEEK_SET); err != nil {
		return err
	}
	objectName := path.Join(path.Join(d.ObjectName...), "builder-artifacts")
	writer, err := backupCtx.bucket.NewWriter(ctx, objectName)
	if err != nil {
		return err
	}

	if _, err := io.Copy(writer, builderArtifacts); err != nil {
		writer.Fail(err) // nolint: errcheck
		return err
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
	prefix := path.Join(d.ObjectName...)

	objects, err := d.readBuilderArtifactList(backupCtx, prefix, verifier)
	if err != nil {
		return err
	}

	objVerifier := &NoOpObjectVerifier{}
	for _, obj := range objects {
		if err := d.copyObjectFromBackup(backupCtx, prefix, obj, objVerifier); err != nil {
			return err
		}
	}
	progChan <- OperationProgress{
		Name:     d.String(),
		Progress: float64(100),
	}

	return nil
}

func builderArtifactManifestPath(prefix string) string {
	return path.Join(prefix, "builder-artifacts")
}

func (d *BuilderMinioDumpOperation) readBuilderArtifactList(backupCtx Context, prefix string, verifier ObjectVerifier) ([]string, error) {
	ctx := backupCtx.ctx
	objName := builderArtifactManifestPath(prefix)
	reader, err := backupCtx.bucket.NewReader(ctx, objName, verifier)
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

func (d *BuilderMinioDumpOperation) copyObjectFromBackup(backupCtx Context, prefix string, obj string, objVerifier ObjectVerifier) error {
	ctx := backupCtx.ctx
	backupObjName := path.Join(prefix, obj)
	builderObjName := obj

	reader, err := backupCtx.bucket.NewReader(ctx, backupObjName, objVerifier)
	if err != nil {
		return err
	}
	defer reader.Close()
	writer, err := backupCtx.builderBucket.NewWriter(ctx, builderObjName)
	if err != nil {
		return err
	}
	if _, err := io.Copy(writer, reader); err != nil {
		writer.Fail(err) // nolint: errcheck
		return err
	}
	return writer.Close()
}

func (d *BuilderMinioDumpOperation) Delete(backupCtx Context) error {
	objVerifier := &NoOpObjectVerifier{}
	prefix := path.Join(d.ObjectName...)

	objects, err := d.readBuilderArtifactList(backupCtx, prefix, objVerifier)
	if err != nil {
		return err
	}

	for _, obj := range objects {
		objPath := path.Join(prefix, obj)
		if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{objPath}); err != nil {
			return errors.Wrapf(err, "failed to delete object %s", objPath)
		}
	}

	manifestPath := builderArtifactManifestPath(prefix)
	if err := backupCtx.bucket.Delete(backupCtx.ctx, []string{manifestPath}); err != nil {
		return errors.Wrapf(err, "failed to delete object %s", manifestPath)
	}

	return nil
}

func (d *BuilderMinioDumpOperation) String() string {
	return d.Name
}
