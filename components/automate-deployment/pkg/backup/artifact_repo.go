package backup

import (
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"go.uber.org/multierr"
)

var ErrSnapshotExists error = errors.New("Snapshot already exists")

type ArtifactRepo struct {
	artifactsRoot Bucket
	snapshotsRoot Bucket
}

type ArtifactRepoProgressReporter interface {
	ReportProgress(completed int64, total int64)
}

func NewArtifactRepo(backupLocationSpec LocationSpecification) *ArtifactRepo {
	return &ArtifactRepo{
		artifactsRoot: backupLocationSpec.ToBucket("shared/builder/artifacts"),
		snapshotsRoot: backupLocationSpec.ToBucket("shared/builder/snapshots"),
	}
}

type ArtifactRepoSnapshotMetadata struct {
	Name     string
	Checksum string
}

// uploadSnapshotArtifactIterator is used to sync files from one bucket to another.
// It is provided a list of artifacts that are in the dst bucket, along with a list
// of artifacts that are required from the src bucket. Only the missing artifacts
// will be uploaded.
// It is also capable of writing out the snapshot file once the artifacts have been
// synced
type uploadSnapshotArtifactIterator struct {
	ctx                      context.Context
	artifactsToUpload        ArtifactStream
	snapshotTmpFile          *os.File
	artifactsToUploadTmpFile *os.File
	numArtifactsToUpload     int64
	src                      Bucket
}

func logClose(c io.Closer, msg string) {
	fileutils.LogClose(c, logrus.StandardLogger(), msg)
}

func logErr(err error, msg string) {
	if err != nil {
		logrus.WithError(err).Error(msg)
	}
}

func newUploadSnapshotArtifactIterator(ctx context.Context, src Bucket,
	requiredArtifacts ArtifactStream, artifactsInRepo ArtifactStream) (*uploadSnapshotArtifactIterator, error) {

	artifactsToUploadTmpFile, err := ioutil.TempFile("", "artifacts-to-upload")
	if err != nil {
		return nil, err
	}
	if err := os.Remove(artifactsToUploadTmpFile.Name()); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		return nil, err
	}

	snapshotTmpFile, err := ioutil.TempFile("", "snapshot-new")
	if err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		return nil, err
	}

	// We're going to write out the required artifacts and artifacts to upload to temp files.
	// We need to do this because we want to get the number of artifacts to upload to report
	// progress
	artifactsToUploadCounter := NewCountingStream(
		Sub(NewLoggingStream(requiredArtifacts, snapshotTmpFile), artifactsInRepo))
	defer logClose(artifactsToUploadCounter, "failed to close counting stream")

	artifactsToUploadWriter := NewLoggingStream(
		artifactsToUploadCounter,
		artifactsToUploadTmpFile)
	defer logClose(artifactsToUploadWriter, "failed to close logging stream")

	if err := ConsumeStream(artifactsToUploadWriter); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}
	if err := artifactsToUploadWriter.Close(); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}
	if _, err := artifactsToUploadTmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}

	artifactsToUpload := NewLineReaderStream(artifactsToUploadTmpFile)

	return &uploadSnapshotArtifactIterator{
		ctx:                      ctx,
		artifactsToUpload:        artifactsToUpload,
		snapshotTmpFile:          snapshotTmpFile,
		artifactsToUploadTmpFile: artifactsToUploadTmpFile,
		numArtifactsToUpload:     artifactsToUploadCounter.Count(),
		src:                      src,
	}, nil
}

func (b *uploadSnapshotArtifactIterator) Next() (BlobUploadRequest, error) {
	nextKey, err := b.artifactsToUpload.Next()
	if err != nil {
		return BlobUploadRequest{}, err
	}

	reader, err := b.src.NewReader(b.ctx, nextKey, &NoOpObjectVerifier{})
	if err != nil {
		return BlobUploadRequest{}, err
	}
	return BlobUploadRequest{
		Key:    nextKey,
		Reader: reader,
	}, nil
}

func (b *uploadSnapshotArtifactIterator) EstimatedSize() int64 {
	return b.numArtifactsToUpload
}

func (b *uploadSnapshotArtifactIterator) WriteSnapshotFile(w io.Writer) (string, error) {
	if _, err := b.snapshotTmpFile.Seek(0, os.SEEK_SET); err != nil {
		return "", err
	}

	checksum := sha256.New()
	m := io.MultiWriter(w, checksum)
	g := gzip.NewWriter(m)

	if _, err := io.Copy(g, b.snapshotTmpFile); err != nil {
		return "", err
	}

	if err := g.Close(); err != nil {
		return "", err
	}
	return hex.EncodeToString(checksum.Sum(nil)), nil
}

func (b *uploadSnapshotArtifactIterator) Close() error {
	err1 := errors.Wrap(b.snapshotTmpFile.Close(), "failed to close temporary file")
	err2 := errors.Wrap(b.artifactsToUpload.Close(), "failed to close artifact stream")
	err3 := errors.Wrap(os.Remove(b.snapshotTmpFile.Name()), "failed to close temporary file")
	return multierr.Combine(err1, err2, err3)
}

type artifactSnapshotOptions struct {
	progress ArtifactRepoProgressReporter
}

type ArtifactSnapshotOpt func(*artifactSnapshotOptions)

func ArtifactRepoSnapshotReportProgress(progress ArtifactRepoProgressReporter) ArtifactSnapshotOpt {
	return func(r *artifactSnapshotOptions) {
		r.progress = progress
	}
}

func (repo *ArtifactRepo) Snapshot(ctx context.Context, name string, srcBucket Bucket,
	requiredArtifacts ArtifactStream, optFuncs ...ArtifactSnapshotOpt) (ArtifactRepoSnapshotMetadata, error) {
	opts := artifactSnapshotOptions{}
	for _, o := range optFuncs {
		o(&opts)
	}

	r, _, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		if !IsNotExist(err) {
			return ArtifactRepoSnapshotMetadata{}, err
		}
	} else {
		logClose(r, "failed to close snapshot file reader")
		return ArtifactRepoSnapshotMetadata{}, ErrSnapshotExists
	}

	artifactsInRepo := repo.ListArtifacts(ctx)
	defer logClose(artifactsInRepo, "failed to close artifact list stream")

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, srcBucket, requiredArtifacts, artifactsInRepo)
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}
	defer uploadIterator.Close() // nolint: errcheck

	uploader := NewBulkUploader(repo.artifactsRoot, "")
	if err := uploader.Upload(ctx, uploadIterator, opts.progress); err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	snapshotFileWriter, err := repo.snapshotsRoot.NewWriter(ctx, fmt.Sprintf("%s.snapshot", name))
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	checksum, err := uploadIterator.WriteSnapshotFile(snapshotFileWriter)
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	if err := snapshotFileWriter.Close(); err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	return ArtifactRepoSnapshotMetadata{
		Name:     name,
		Checksum: checksum,
	}, nil
}

type artifactRestoreOptions struct {
	validateChecksum bool
	checksum         string
	progress         ArtifactRepoProgressReporter
}

type ArtifactRestoreOpt func(*artifactRestoreOptions)

func ArtifactRepoRestoreValidateChecksum(checksum string) ArtifactRestoreOpt {
	return func(r *artifactRestoreOptions) {
		r.validateChecksum = true
		r.checksum = checksum
	}
}

func ArtifactRepoRestoreReportProgress(progress ArtifactRepoProgressReporter) ArtifactRestoreOpt {
	return func(r *artifactRestoreOptions) {
		r.progress = progress
	}
}

func (repo *ArtifactRepo) Restore(ctx context.Context, dstBucket Bucket, name string, optFuncs ...ArtifactRestoreOpt) error {
	opts := artifactRestoreOptions{}
	for _, o := range optFuncs {
		o(&opts)
	}

	artifactsToRestore, checksum, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}
	defer logClose(artifactsToRestore, "failed to close snapshot file reader")

	if opts.validateChecksum && checksum != opts.checksum {
		// TODO(jaym): We probably need a specific error type for this
		return errors.New("Invalid checksum")
	}

	uploader := NewBulkUploader(dstBucket, "")

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, repo.artifactsRoot,
		artifactsToRestore, EmptyStream())
	if err != nil {
		return err
	}

	if err := uploader.Upload(ctx, uploadIterator, opts.progress); err != nil {
		return err
	}

	return nil
}

func (repo *ArtifactRepo) Remove(ctx context.Context, name string) error {
	snapshotFiles, _, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}
	defer logClose(snapshotFiles, "failed to close snapshot stream")

	// TODO[integrity]: We should really 2 phase commit this change. As it stands now, if
	// we remove this file and then fail to remove the artifacts, they will be
	// leaked and we'll have no way to clean them up
	if err := repo.removeSnapshotFile(ctx, name); err != nil {
		return err
	}

	// Get the artifacts in all the snapshots excluding the one we're trying to delete
	remainingSnapshotsFiles := repo.ListArtifacts(ctx, name)
	defer logClose(remainingSnapshotsFiles, "failed to close artifact list stream")

	// Remove artifacts from the snapshot we're trying to exist which exist in other
	// snapshots
	filesToDelete := Sub(snapshotFiles, remainingSnapshotsFiles)
	defer logClose(filesToDelete, "failed to close fileToDelete stream")

	bulkDeleter := NewBulkDeleter(repo.artifactsRoot, "")
	return bulkDeleter.Delete(ctx, filesToDelete)
}

func (repo *ArtifactRepo) ListArtifacts(ctx context.Context, excludedSnapshots ...string) ArtifactStream {
	snapshots, _, err := repo.snapshotsRoot.List(ctx, "", false)
	if err != nil {
		return ErrStream(err)
	}

	// Merge as we go in order to avoid running out of file descriptors and/or stack
	// in the case where someone has a lot of snapshots.

	var lastMergedStream ArtifactStream
	for _, snapshot := range snapshots {
		if !strings.HasSuffix(snapshot.Name, ".snapshot") {
			continue
		}
		name := strings.TrimSuffix(snapshot.Name, ".snapshot")
		if stringutils.SliceContains(excludedSnapshots, name) {
			continue
		}

		snapshotStream, _, err := repo.openSnapshotFile(ctx, name)
		if err != nil {
			return ErrStream(err)
		}
		if lastMergedStream == nil {
			lastMergedStream = snapshotStream
		} else {
			lastMergedStream, err = mergeIntoFile(lastMergedStream, snapshotStream)
			if err != nil {
				return ErrStream(err)
			}
		}
	}

	if lastMergedStream == nil {
		return EmptyStream()
	}
	return lastMergedStream
}

// mergeIntoFile takes 2 streams and merges them into 1. This function is not
// lazy and merges them into a file, and then returns a Stream that reads
// from the beginning of that file
func mergeIntoFile(a ArtifactStream, b ArtifactStream) (ArtifactStream, error) {
	defer logClose(a, "failed to close stream")
	defer logClose(b, "failed to close stream")

	tmpFile, err := ioutil.TempFile("", "lastMerged")
	if err != nil {
		return nil, err
	}
	defer logErr(os.Remove(tmpFile.Name()), "failed to remove temp file")

	s := NewLoggingStream(Merge(a, b), tmpFile)
	defer logClose(s, "failed to close logging stream")

	// Consume the stream to get it written to the tmpfile
	if err := ConsumeStream(s); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, err
	}

	// seek to the beginning of the file
	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, err
	}

	return NewLineReaderStream(tmpFile), nil
}

func (repo *ArtifactRepo) openSnapshotFile(ctx context.Context, name string) (ArtifactStream, string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	r, err := repo.snapshotsRoot.NewReader(ctx, fmt.Sprintf("%s.snapshot", name), &NoOpObjectVerifier{})
	if err != nil {
		return nil, "", err
	}

	// r is closed with reader
	reader := newChecksummingReader(r)
	defer logClose(reader, "failed to close snapshot reader")

	tmpFile, err := ioutil.TempFile("", fmt.Sprintf("snapshot-%s", name))
	if err != nil {
		return nil, "", err
	}

	if err := os.Remove(tmpFile.Name()); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	g, err := gzip.NewReader(reader)
	if err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}
	defer logClose(g, "failed to close gzip reader")

	if _, err := io.Copy(tmpFile, g); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	checksum := reader.BlobSHA256()

	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	return NewLineReaderStream(tmpFile), checksum, nil
}

func (repo *ArtifactRepo) removeSnapshotFile(ctx context.Context, name string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	return repo.snapshotsRoot.Delete(ctx, []string{fmt.Sprintf("%s.snapshot", name)})
}
